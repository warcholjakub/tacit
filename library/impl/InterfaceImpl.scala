package tacit.library

import language.experimental.captureChecking
import caps.*

import java.io.{File => JFile, PrintStream, FileOutputStream}
import java.nio.file.{Files, Path, Paths}

/** The concrete capability API handed to agent code. The REPL preamble
 *  instantiates [[SandboxInterface]], whose config is the one registered via
 *  [[InterfaceImpl.configure]]; tests in this package construct
 *  `InterfaceImpl` directly. */
@assumeSafe
abstract class InterfaceImpl private[library] (
  configJson: String
) extends Interface:

  private val config = LibraryConfig.fromJson(configJson)

  private val DefaultClassifiedPatterns: Set[String] = Set(
    ".ssh", ".gnupg", ".env", ".env.*", ".netrc", ".npmrc", ".pypirc",
    ".docker", ".kube", ".aws", ".azure", ".gcloud",
  )
  private val strictMode: Boolean = config.strictMode.getOrElse(true)
  private val classifiedPatterns: Set[String] = config.classifiedPaths.getOrElse(DefaultClassifiedPatterns)
  /** Outer bound on file-system roots: every `requestFileSystem(root)` must
   *  resolve to a path within one of these. When unset, it defaults to the
   *  server's current working directory, so the sandbox is confined to that
   *  subtree by default (fail closed) rather than allowing any root. Set it
   *  explicitly to widen or relocate the bound. */
  private val allowedRoots: Set[String] =
    config.allowedRoots.getOrElse(Set(InterfaceImpl.currentWorkingDir))
  private val commandPermissions: Option[Set[String]] = config.commandPermissions
  private val networkPermissions: Option[Set[String]] = config.networkPermissions
  private val llmConfig: Option[LlmConfig] = config.llm
  /** Whether `writeClassified` is permitted. Defaults to true (current
   *  behavior); set to false to protect the *integrity* of classified files
   *  (e.g. `.ssh/authorized_keys`) against agent writes — confidentiality is
   *  already enforced by the classified-read path. */
  private val classifiedWriteEnabled: Boolean = config.classifiedWrite.getOrElse(true)

  /** Optional secondary sink that receives the *unmasked* form of printed values.
   *  When configured, `println`/`print`/`printf` still write a masked view
   *  (`Classified(***)`) to the normal output, but also append the fully
   *  unwrapped content to this file — only the end user reading that file
   *  can see classified data. */
  private val secureWriter: Option[PrintStream] = config.secureOutput.map(InterfaceImpl.secureWriterFor)

  private def withSecureOut(op: => Unit): Unit =
    secureWriter.foreach(w => scala.Console.withOut(w)(op))

  private def unwrapForSecure(x: Any): Any = x match
    case c: Classified[?] =>
      ClassifiedImpl.unwrap(c).fold(
        e => s"<classified error: ${e.getMessage}>",
        v => v
      )
    case other => other

  private def maskForMain(x: Any): Any = x match
    case _: Classified[?] => "Classified(***)"
    case other            => other

  /** Builds the `FileSystem` for one `requestFileSystem` scope. Real disk by
   *  default; tests override it with a [[VirtualFileSystem]]. The
   *  `classifiedWrite` gate is passed explicitly so an override cannot
   *  silently drop it (the entry-level `writeClassified`/`mkdir` checks live
   *  in the file system, not here).
   *
   *  `root` arrives canonical: absolute, normalized and with symlinks
   *  resolved, exactly as it was checked against the bounds. */
  protected def createFS(
    root: String,
    filter: String -> Boolean,
    classifiedPatterns: Set[String],
    classifiedWrite: Boolean,
    readOnly: Boolean
  ): FileSystem =
    new RealFileSystem(root, filter, classifiedPatterns, classifiedWrite, readOnly)

  export FileOps.*
  export ProcessOps.*
  export WebOps.*

  private val llmOps = LlmOps(llmConfig)

  export llmOps.*

  def println(x: Any)(using IOCapability): Unit =
    // Classified.toString already returns "Classified(***)" so the
    // main stream is automatically masked.
    scala.Predef.println(x)
    withSecureOut(scala.Predef.println(unwrapForSecure(x)))

  def println()(using IOCapability): Unit =
    scala.Predef.println()
    withSecureOut(scala.Predef.println())

  def print(x: Any)(using IOCapability): Unit =
    scala.Predef.print(x)
    withSecureOut(scala.Predef.print(unwrapForSecure(x)))

  def printf(fmt: String, args: Any*)(using IOCapability): Unit =
    // printf's format specifiers bypass toString, so mask Classified args
    // explicitly for the main stream.
    scala.Predef.printf(fmt, args.map(maskForMain)*)
    withSecureOut(scala.Predef.printf(fmt, args.map(unwrapForSecure)*))

  /** Resolves a path to the canonical form used by the bound check below and
   *  sent to the permission oracle: absolute + normalized, then through
   *  symlinks. Resolving symlinks matters for the bound check. Otherwise a
   *  symlink *named* inside an allowed root but *pointing* outside it would
   *  pass. A path that does not exist yet is resolved through its longest
   *  existing ancestor, so it keeps the same form once it is created. */
  private def resolveRootForBound(p: String): Path =
    InterfaceImpl.canonical(Paths.get(p).toAbsolutePath.nn.normalize.nn)

  /** Entry-time outer-bound check for [[requestFileSystem]]: the requested root
   *  must resolve to a path equal to, or nested under, one of `allowedRoots`
   *  (which defaults to the current working directory). */
  /** Returns the canonical root that was checked (and maybe approved), so the
   *  file system is built on exactly that path. */
  private def requireRootAllowed(root: String, access: FileAccess, reason: String): Path =
    val resolved = resolveRootForBound(root)
    val permitted = allowedRoots.exists(allowed => resolved.startsWith(resolveRootForBound(allowed)))
    if permitted then resolved
    else
      val request = io.circe.Json.obj(
        "kind" -> io.circe.Json.fromString("filesystem"),
        "root" -> io.circe.Json.fromString(root),
        "resolved" -> io.circe.Json.fromString(resolved.toString),
        "access" -> io.circe.Json.fromString(access match
          case FileAccess.ReadOnly => "read"
          case FileAccess.ReadWrite => "write"),
        "reason" -> io.circe.Json.fromString(reason)
      )
      InterfaceImpl.askPermission(request) match
        case PermissionAnswer.Allow => resolved
        case PermissionAnswer.Deny(Some(message)) => throw SecurityException(message)
        case PermissionAnswer.Deny(None) =>
          throw SecurityException(
            s"Access denied: filesystem root '$root' is not within any allowed root $allowedRoots"
          )

  def requestFileSystem[T](
    root: String,
    access: FileAccess = FileAccess.ReadWrite,
    reason: String = ""
  )(op: FileSystem^ ?=> T)(using IOCapability): T =
    val checkedRoot = requireRootAllowed(root, access, reason)
    val fs = createFS(
      checkedRoot.toString, _ => true, classifiedPatterns, classifiedWriteEnabled,
      readOnly = access == FileAccess.ReadOnly
    )
    op(using fs)

  /** Entry-time check shared by [[requestExecPermission]] and
   *  [[requestNetwork]]: each item in `scope` must match at least one pattern
   *  in `policy`, or be granted by the host's permission oracle. For command
   *  patterns that carry args (e.g. `"sbt run *"`), we match against the
   *  pattern's command-word (the part before the first space) so a bare scope
   *  command like `"sbt"` passes entry — per-invocation arg filtering still
   *  happens at runtime. Returns the items the oracle granted. */
  private def requireWithinPolicy(
    scope: Set[String],
    policy: Option[Set[String]],
    kind: String,
    label: String,
    details: (String, io.circe.Json)*
  ): Set[String] =
    policy match
      case None => Set.empty
      case Some(patterns) =>
        val outside = scope.filterNot: item =>
          patterns.exists(pattern => GlobMatcher.matches(item, pattern.takeWhile(_ != ' ')))
        if outside.isEmpty then Set.empty
        else
          val request = io.circe.Json.obj(
            Seq(
              "kind" -> io.circe.Json.fromString(kind),
              "items" -> io.circe.Json.arr(outside.toList.sorted.map(io.circe.Json.fromString)*)
            ) ++ details*
          )
          InterfaceImpl.askPermission(request) match
            case PermissionAnswer.Allow => outside
            case PermissionAnswer.Deny(Some(message)) => throw SecurityException(message)
            case PermissionAnswer.Deny(None) =>
              throw SecurityException(
                s"Access denied: scope $label '${outside.toList.sorted.head}' is not permitted by server policy $patterns"
              )

  def requestExecPermission[T](
    commands: Set[String],
    reason: String = ""
  )(op: ProcessPermission^ ?=> T)(using IOCapability): T =
    // Server-configured commandPermissions is the outer bound: every command
    // the scope declares must be permitted by some pattern's command-word, or
    // granted by the host. A granted command may run with any arguments.
    val granted = requireWithinPolicy(
      commands, commandPermissions, "exec", "command",
      "reason" -> io.circe.Json.fromString(reason)
    )
    val perm = new ProcessPermissionImpl(commands, strictMode, commandPermissions, granted)
    op(using perm)

  def requestNetwork[T](
    hosts: Set[String],
    access: NetworkAccess = NetworkAccess.Send,
    reason: String = ""
  )(op: Network^ ?=> T)(using IOCapability): T =
    // Server-configured networkPermissions is the outer bound: every host the
    // scope declares must match at least one pattern, or be granted by the host.
    val _ = requireWithinPolicy(
      hosts, networkPermissions, "network", "host",
      "access" -> io.circe.Json.fromString(access match
        case NetworkAccess.Fetch => "fetch"
        case NetworkAccess.Send => "send"),
      "reason" -> io.circe.Json.fromString(reason)
    )
    val net = new NetworkImpl(hosts, access)
    op(using net)

  def classify[T](value: T): Classified[T] = ClassifiedImpl.wrap(value)

  def access(path: String)(using fs: FileSystem): FileEntry^{fs} =
    fs.access(path)

  def readClassified(path: String)(using fs: FileSystem): Classified[String] =
    fs.access(path).readClassified()

  def writeClassified(path: String, content: Classified[String])(using fs: FileSystem): Unit =
    if !classifiedWriteEnabled then
      throw SecurityException(
        s"Access denied: writeClassified is disabled by the server configuration (classifiedWrite = false)"
      )
    fs.access(path).writeClassified(content)

object InterfaceImpl:
  /** The library config JSON of this sandbox, registered once by the server
    * via [[configure]]. Static state is scoped to the class loader, and the
    * server gives every REPL its own sandboxed loader with the library JAR,
    * so "once" means once per REPL / stateless execution. */
  private val configured = java.util.concurrent.atomic.AtomicReference[String | Null](null)

  /** Register the sandbox's library config. The server calls this (through
    * the sandbox class loader, before any REPL code runs) exactly once; a
    * second call throws. */
  private[library] def configure(configJson: String): Unit =
    if !configured.compareAndSet(null, configJson) then
      throw SecurityException("The TACIT sandbox is already configured.")

  /** The config registered by [[configure]]; throws if the sandbox has not
    * been configured. */
  private[library] def configuredJson: String =
    configured.get() match
      case null => throw IllegalStateException("The TACIT sandbox has not been configured.")
      case json => json

  /** The host's answer to requests outside the configured bounds, registered
    * once by the server via [[installPermissionOracle]]. It takes a JSON request
    * and returns a JSON answer, so it only needs JDK types to cross from the
    * server's class loader into this sandboxed one. */
  private val permissionOracle =
    java.util.concurrent.atomic.AtomicReference[java.util.function.Function[String, String] | Null](null)

  /** Register the host's permission oracle. Like [[configure]], it is not
    * reachable from agent code and can only be set once per sandbox. */
  private[library] def installPermissionOracle(oracle: java.util.function.Function[String, String]): Unit =
    if !permissionOracle.compareAndSet(null, oracle) then
      throw SecurityException("The TACIT permission oracle is already installed.")

  /** Asks the host about a request outside the configured bounds. Without an
    * oracle, on an answer it cannot parse, or if the oracle fails, the request
    * is denied: host errors must not reach agent code. */
  private[library] def askPermission(request: io.circe.Json): PermissionAnswer =
    permissionOracle.get() match
      case null => PermissionAnswer.Deny(None)
      case oracle =>
        scala.util.Try(oracle.apply(request.noSpaces)).toOption.flatMap(Option(_)) match
          case Some(answer) => PermissionAnswer.fromJson(answer)
          case None => PermissionAnswer.Deny(None)

  /** `toRealPath` of `path` if it exists, otherwise the real path of its
    * nearest existing ancestor with the remaining components appended. */
  private[library] def canonical(path: Path): Path =
    if Files.exists(path) then path.toRealPath().nn
    else
      Option(path.getParent) match
        case Some(parent) => canonical(parent).resolve(path.getFileName.nn).nn
        case None => path

  /** The server process's current working directory, used as the default
    * `allowedRoots` bound. Falls back to "." if the `user.dir` property is
    * absent (it normally is not). */
  private[library] def currentWorkingDir: String =
    Option(System.getProperty("user.dir")).getOrElse(".")

  /** One append-mode `PrintStream` per secureOutput path, shared process-wide.
    * A fresh `InterfaceImpl` is built on every REPL init (and stateless
    * `execute` builds a REPL per call), so opening a new `FileOutputStream` in
    * each instance would leak a file descriptor per execution until the process
    * runs out. Cache and reuse keyed by canonical path, so different spellings
    * of the same file (`a/./log`, `a/sub/../log`) share one stream. */
  private val secureWriters = scala.collection.mutable.HashMap[String, PrintStream]()

  private[library] def secureWriterFor(path: String): PrintStream =
    secureWriters.synchronized:
      val file = JFile(path).getAbsoluteFile.nn
      val key =
        try file.getCanonicalPath.nn
        catch case _: java.io.IOException => file.getAbsolutePath.nn
      secureWriters.getOrElseUpdate(key, {
        Option(file.getParentFile).foreach(_.mkdirs())
        createOwnerOnly(file.toPath)
        PrintStream(FileOutputStream(file, true), true, "UTF-8")
      })

  /** Create the sink file with owner-only permissions (`rw-------`) if it does
    * not exist yet. Creation and permissions are one atomic step (`O_CREAT`
    * with the mode), so there is no window in which the file exists with
    * umask-default permissions. An existing file is left as it is; on
    * non-POSIX file systems the file is created with default permissions. */
  private def createOwnerOnly(path: Path): Unit =
    val perms = java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
    try Files.createFile(path, java.nio.file.attribute.PosixFilePermissions.asFileAttribute(perms))
    catch
      case _: java.nio.file.FileAlreadyExistsException => ()
      case _: UnsupportedOperationException =>
        try Files.createFile(path)
        catch case _: java.nio.file.FileAlreadyExistsException => ()
      case _: java.io.IOException => () // FileOutputStream below reports the real problem

/** The class the server preamble instantiates (`object api extends
  * SandboxInterface`). It has no constructor parameters: its policy is the
  * config registered via [[InterfaceImpl.configure]]. Kept abstract because a
  * concrete subclass of [[InterfaceImpl]] cannot be defined inside the
  * library itself (the compiler rejects the `printf(fmt, args: Any*)`
  * override check in this compilation unit); the REPL supplies the concrete
  * `object`. */
@assumeSafe
abstract class SandboxInterface extends InterfaceImpl(InterfaceImpl.configuredJson)

/** The host's decision on a request outside the configured bounds. */
private[library] enum PermissionAnswer:
  case Allow
  case Deny(message: Option[String])

private[library] object PermissionAnswer:
  /** `{"allow": true}` or `{"allow": false, "message": "..."}`. */
  def fromJson(json: String): PermissionAnswer =
    io.circe.parser.parse(json).toOption.map(_.hcursor) match
      case Some(c) if c.get[Boolean]("allow").contains(true) => Allow
      case Some(c) => Deny(c.get[String]("message").toOption)
      case None => Deny(None)
