package tacit
package executor

import core.{ApiMode, Context, Log}
import Context.*

import dotty.tools.repl.*
import dotty.tools.dotc.reporting.Diagnostic

import java.io.{File => JFile, PrintStream}
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.util.jar.JarFile

case class ExecutionResult(
    success: Boolean,
    output: String,
    error: Option[String] = None
)

object ManagedRepl:

  /** Hard cap on captured execution output (bytes of UTF-8). Without it,
    * `while(true) println("x")` grows the capture buffer until the server
    * runs out of memory. Output beyond the cap is discarded. */
  val MaxOutputBytes: Int = 10 * 1024 * 1024

  /** Appended to captured output that hit [[MaxOutputBytes]], so the client
    * can tell the output was cut rather than complete. */
  val TruncationMarker: String = "\n... [output truncated: exceeded 10 MiB capture limit]"

  /** How long a timed-out evaluation gets to stop after being interrupted
    * before the session treats it as still running. */
  private val InterruptGraceMs: Long = 200

  /** How many timed-out executions may keep running, across every REPL in
    * the process, before new sessions and stateless executions are refused.
    * Each keeps a whole compiler in memory until it ends. */
  val MaxAbandonedExecutions: Int = 10

  /** Timed-out executions still running, process-wide (see
    * [[MaxAbandonedExecutions]]). */
  private val abandonedCount = java.util.concurrent.atomic.AtomicInteger()

  def abandonedExecutions: Int = abandonedCount.get()

  /** [[MaxAbandonedExecutions]], lowered by tests. */
  @volatile private[executor] var maxAbandoned: Int = MaxAbandonedExecutions

  /** Why a new session or stateless execution must be refused, if it must. */
  def abandonedLimitReached: Option[String] =
    val max = maxAbandoned
    Option.when(abandonedCount.get() >= max)(
      s"$max timed-out executions are still running in the background, " +
        "so no new session or stateless execution can start until some of them end."
    )

  val StillRunningMessage: String =
    "An earlier execution in this session timed out and is still running in the background; " +
      "this session cannot run code until it ends."

  /** An [[java.io.OutputStream]] that retains at most `limit` bytes and
    * silently discards the rest, setting [[truncated]] once anything is
    * dropped. Writes never throw, so the cap logic can never break the
    * evaluation path (PrintStream also swallows I/O errors on its own). */
  private[executor] final class BoundedOutputStream(private val limit: Int) extends java.io.OutputStream:
    private val buf = new java.io.ByteArrayOutputStream(math.min(limit, 8192))
    @volatile var truncated: Boolean = false

    override def write(b: Int): Unit =
      if buf.size() < limit then buf.write(b)
      else truncated = true

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      val room = limit - buf.size()
      if room <= 0 then truncated = true
      else
        if len > room then truncated = true
        buf.write(b, off, math.min(len, room))

    def resetCapture(): Unit =
      buf.reset()
      truncated = false

    def capturedString: String = buf.toString(java.nio.charset.StandardCharsets.UTF_8)
  end BoundedOutputStream

  /** Subclass of [[ReplDriver]] that exposes the protected `runBody`/`interpret`
    * pair, so we can feed a pre-parsed [[ParseResult]] to the driver instead of
    * re-parsing the same source inside `driver.run(String)`.
    *
    * `redirectOutput` is off: the driver would otherwise swap the
    * process-wide `System.out`/`System.err` around every run, which breaks as
    * soon as two sessions run at once. Diagnostics and rendered values go to
    * `out` either way; [[RoutingPrintStream]] catches the rest.
    */
  private class OpenReplDriver(
      settings: Array[String],
      out: PrintStream,
      cl: Option[ClassLoader]
  ) extends ReplDriver(settings, out, cl):
    override def redirectOutput: Boolean = false
    def runParseResult(res: ParseResult)(using State): State =
      runBody(interpret(res))

  /** Reads a UTF-8 text resource bundled inside the library JAR (e.g. the
    * `Interface.scala` API reference shown by `show_interface`). The reference
    * lives with the library it documents rather than on the server's own
    * classpath. Returns `None` if the JAR or entry can't be read.
    */
  def readLibraryResource(name: String)(using Context): Option[String] =
    try
      val jar = JarFile(ctx.config.libraryJarPath)
      try
        Option(jar.getJarEntry(name)).map: entry =>
          val stream = jar.getInputStream(entry)
          try scala.io.Source.fromInputStream(stream)(using scala.io.Codec.UTF8).mkString
          finally stream.close()
      finally jar.close()
    catch case _: Exception => None

  /** Core library JAR plus any TACIT plugin JARs declared in config. */
  private[executor] def replClasspathJars(using Context): List[String] =
    ctx.config.libraryJarPath :: ctx.plugins.map(_.jarPath)

  /** The library fat JAR provides the full classpath (Scala stdlib + library
    * classes + library dependencies), so we don't need `-usejavacp`. TACIT
    * plugin JARs are appended to the classpath.
    */
  private def replClasspathArgs(using Context): Array[String] =
    val classpath = replClasspathJars
      .map(JFile(_).getAbsolutePath)
      .mkString(JFile.pathSeparator)
    Array(
      "-classpath", classpath,
      "-color:never",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Yexplicit-nulls",
      "-Ycheck-all-patmat",
      "-Wsafe-init",
      "-language:experimental.captureChecking",
      "-language:experimental.modularity",
      // No interrupt instrumentation. With the default (`true`) the REPL
      // loader re-defines every non-JDK class it touches, including the
      // library's, so the library would exist twice (once in
      // `sandboxedClassLoader`, once instrumented in the REPL loader) with
      // separate static state, and the config registered by
      // `configureLibrary` would not be the one agent code sees. `local`
      // instruments REPL code with calls to `dotty.tools.repl.StopRepl`, which
      // the sandboxed parent loader cannot resolve. Nothing here sets the
      // REPL stop flag anyway: timeouts rely on `Thread.interrupt`.
      "-Xrepl-interrupt-instrumentation:false"
    )

  /** Exposes only JDK platform classes and the library JARs (core + plugins),
    * keeping user code away from server internals (tacit.core, tacit.mcp,
    * tacit.executor) and server dependencies (circe, scopt, etc.).
    */
  private def sandboxedClassLoader(using Context): ClassLoader =
    val urls = replClasspathJars.map(p => JFile(p).toURI.toURL).toArray
    new URLClassLoader(urls, ClassLoader.getPlatformClassLoader)

  /** Core tacit-library preamble: imports the tacit Interface and exposes
    * `api.*`. Used when no plugin is loaded, or when every loaded plugin
    * declares `apiMode: extend-core`. `SandboxInterface` has no constructor
    * parameters: its policy is the config the server registered in
    * [[ManagedRepl.configureLibrary]].
    */
  private val corePreamble: String =
    """|import tacit.library.*
       |import caps.*
       |@assumeSafe object api extends SandboxInterface
       |import api.*
       |@assumeSafe given IOCapability = GlobalIOCap
       |""".stripMargin

  /** Register the sandbox's library config *before* any REPL code runs, by
    * calling `InterfaceImpl.configure` on the library classes loaded through
    * the REPL's own sandboxed class loader (static state is per loader). The
    * method is not accessible from REPL code, hence reflection. Doing this
    * from the server rather than from the preamble also sidesteps REPL
    * evaluation order: REPL objects initialize lazily, so nothing that
    * depends on `api` having been touched first would be safe.
    */
  private def configureLibrary(loader: ClassLoader, libraryConfigJson: String): Unit =
    invokeLibrary(loader, "configure", classOf[String], libraryConfigJson)

  /** Register the host's permission oracle the same way. It is passed as a
    * JDK `Function` so it crosses the class-loader boundary. */
  private def installPermissionOracle(loader: ClassLoader, oracle: String => String): Unit =
    val function: java.util.function.Function[String, String] = oracle(_)
    invokeLibrary(loader, "installPermissionOracle", classOf[java.util.function.Function[?, ?]], function)

  /** Makes every capability call from `thread` fail, through the library
    * loaded by `loader` (see `InterfaceImpl.revoke`). */
  private def revokeLibraryAccess(loader: ClassLoader, thread: Thread): Unit =
    invokeLibrary(loader, "revoke", classOf[Thread], thread)

  private def invokeLibrary(loader: ClassLoader, method: String, paramType: Class[?], arg: AnyRef): Unit =
    val module = Class.forName("tacit.library.InterfaceImpl$", true, loader)
    val instance = module.getField("MODULE$").get(null)
    try
      val _ = module.getMethod(method, paramType).invoke(instance, arg)
    catch case e: java.lang.reflect.InvocationTargetException =>
      throw Option(e.getCause).getOrElse(e)

  /** Preamble injected before any user code so the capability API is in scope.
    *
    *  - No plugins → core preamble only.
    *  - All plugins `extend-core` → core preamble followed by each plugin's
    *    preamble in declaration order.
    *  - All plugins `replace-core` → only plugin preambles, no core. This
    *    matters because plugin libraries (e.g. `safemode.lib`) may define
    *    `Classified[T]`/`FileSystem`/`IOCapability` shapes that would collide
    *    with tacit-library's if both preambles ran.
    *
    * Mixed `extend-core` + `replace-core` is impossible at this point because
    * `PluginLoader.loadAll` rejects it at startup.
    */
  private[tacit] def libraryPreamble(using Context): String =
    val plugins = ctx.plugins
    if plugins.isEmpty then corePreamble
    else
      val includeCore = plugins.forall(_.manifest.apiMode == ApiMode.ExtendCore)
      val parts =
        Option.when(includeCore)(corePreamble).toList ++ plugins.map(_.preamble)
      parts.mkString("\n")

  /** Where the execution running on this thread captures its output. */
  private val captureTarget = ThreadLocal[PrintStream]()

  /** Installed once as `System.out` and `System.err`: each write goes to the
    * capture of the execution running on the writing thread, or to the
    * stream it replaced when none is. Agent `println` lands here because
    * every session loads its own `scala.Console` (the library JAR bundles the
    * standard library), and `Console.out` is whatever `System.out` was when
    * it was first initialized. Sessions can therefore run in parallel without
    * swapping process-wide streams.
    *
    * Every method hands the call to the target stream whole, so text is
    * encoded by the target, never by this shared stream: its encoder would
    * carry half a surrogate pair from one session's write into the next
    * session's, and its lock would make sessions wait on each other. Threads
    * started by agent or library code have no target, so their output goes to
    * the replaced stream (a host's stderr or terminal), not to a capture.
    */
  private[executor] final class RoutingPrintStream(fallback: PrintStream)
      extends PrintStream(
        new java.io.OutputStream:
          override def write(b: Int): Unit = currentTarget(fallback).write(b)
          override def write(b: Array[Byte], off: Int, len: Int): Unit =
            currentTarget(fallback).write(b, off, len)
          override def flush(): Unit = currentTarget(fallback).flush()
        ,
        true,
        StandardCharsets.UTF_8
      ):
    private def target: PrintStream = currentTarget(fallback)

    override def write(b: Int): Unit = target.write(b)
    override def write(buf: Array[Byte], off: Int, len: Int): Unit = target.write(buf, off, len)
    override def write(buf: Array[Byte]): Unit = target.write(buf)
    override def writeBytes(buf: Array[Byte]): Unit = target.writeBytes(buf)
    override def flush(): Unit = target.flush()
    /** Never closes a target: they belong to sessions and to the host. */
    override def close(): Unit = target.flush()
    override def checkError(): Boolean = target.checkError()

    override def print(b: Boolean): Unit = target.print(b)
    override def print(c: Char): Unit = target.print(c)
    override def print(i: Int): Unit = target.print(i)
    override def print(l: Long): Unit = target.print(l)
    override def print(f: Float): Unit = target.print(f)
    override def print(d: Double): Unit = target.print(d)
    override def print(s: Array[Char]): Unit = target.print(s)
    override def print(s: String | Null): Unit = target.print(s)
    override def print(obj: Object | Null): Unit = target.print(obj)

    override def println(): Unit = target.println()
    override def println(x: Boolean): Unit = target.println(x)
    override def println(x: Char): Unit = target.println(x)
    override def println(x: Int): Unit = target.println(x)
    override def println(x: Long): Unit = target.println(x)
    override def println(x: Float): Unit = target.println(x)
    override def println(x: Double): Unit = target.println(x)
    override def println(x: Array[Char]): Unit = target.println(x)
    override def println(x: String | Null): Unit = target.println(x)
    override def println(x: Object | Null): Unit = target.println(x)

    override def printf(format: String | Null, args: (Object | Null)*): PrintStream =
      target.printf(format, args*)
      this
    override def printf(l: java.util.Locale | Null, format: String | Null, args: (Object | Null)*): PrintStream =
      target.printf(l, format, args*)
      this
    override def format(format: String | Null, args: (Object | Null)*): PrintStream =
      target.format(format, args*)
      this
    override def format(l: java.util.Locale | Null, format: String | Null, args: (Object | Null)*): PrintStream =
      target.format(l, format, args*)
      this

    override def append(csq: CharSequence | Null): PrintStream =
      target.append(csq)
      this
    override def append(csq: CharSequence | Null, start: Int, end: Int): PrintStream =
      target.append(csq, start, end)
      this
    override def append(c: Char): PrintStream =
      target.append(c)
      this
  end RoutingPrintStream

  private def currentTarget(fallback: PrintStream): PrintStream =
    val t = captureTarget.get()
    if t == null then fallback else t

  /** Puts [[RoutingPrintStream]] in place before a session loads its
    * `scala.Console`. Checked for every new session, so a host that replaced
    * `System.out` after the first one (e.g. to keep it off a JSON-RPC
    * channel) gets routing again on top of its stream. */
  private def routeStandardStreams(): Unit = synchronized:
    System.out match
      case _: RoutingPrintStream => ()
      case out                   => System.setOut(RoutingPrintStream(out))
    System.err match
      case _: RoutingPrintStream => ()
      case err                   => System.setErr(RoutingPrintStream(err))

  private def withOutputCapture(
    outputCapture: BoundedOutputStream,
    printStream: PrintStream
  )(run: => Unit): (String, Option[Throwable]) =
    outputCapture.resetCapture()
    val previous = captureTarget.get()
    captureTarget.set(printStream)
    val thrown =
      try
        run
        None
      // NonFatal (not just Exception) so a non-fatal interpreter *Error* — e.g.
      // dotty's `AssertionError: denotation ... invalid in run N` on certain
      // symbols is reported as a failed execution instead of escaping to the
      // JSON-RPC loop, where it would leave the client without any response.
      catch case scala.util.control.NonFatal(e) => Option(e)
      finally
        // remove() rather than set(null): callers may be long-lived pool or
        // JSON-RPC threads.
        if previous == null then captureTarget.remove() else captureTarget.set(previous)
        printStream.flush()
    val captured = outputCapture.capturedString.trim
    val output = if outputCapture.truncated then captured + TruncationMarker else captured
    (output, thrown)
end ManagedRepl

/** Wraps a [[ReplDriver]] together with its output-capture streams and
  * accumulated [[State]]. Call [[init]] once, then [[run]] repeatedly; each
  * run accumulates into the persistent state. `name` identifies it in logs
  * and thread names (a session id).
  */
class ManagedRepl(name: String = "stateless")(using Context):
  import ManagedRepl.*

  routeStandardStreams()

  /** A timed-out evaluation that ignored its interrupt and still runs
    * against [[driver]]. The driver is not thread-safe, so the session
    * refuses to run anything until that thread ends. */
  @volatile private var abandoned: Option[Thread] = None

  /** Whether a timed-out evaluation is still running, so that [[run]]
    * refuses code. It keeps this REPL's compiler and class loader in memory
    * until it ends, which a host should count against its limits. */
  def blocked: Boolean = abandoned.exists(_.isAlive)

  @volatile private var running = false

  /** Whether code is running, or a timed-out evaluation still is. Deleting a
    * busy REPL does not free its memory yet. */
  def busy: Boolean = running || blocked

  private val outputCapture = new BoundedOutputStream(MaxOutputBytes)
  private val printStream = new PrintStream(outputCapture, true, StandardCharsets.UTF_8)
  private val sandboxLoader = sandboxedClassLoader
  private val driver = new OpenReplDriver(replClasspathArgs, printStream, Some(sandboxLoader))
  private var state: State = driver.initialState

  /** Preamble compile errors are intentionally *not* captured: a broken
    * preamble is a programmer bug that should surface loudly.
    */
  def init(): this.type =
    configureLibrary(sandboxLoader, ctx.config.libraryConfig.noSpaces)
    ctx.permissionOracle.foreach(installPermissionOracle(sandboxLoader, _))
    state = driver.run(libraryPreamble)(using state)
    if ctx.config.safeMode then
      state = driver.run("import language.experimental.safe")(using state)
    this

  /** Execute `code` against the current state.
    *
    * Reimplements the dispatch of `ReplDriver.run(String)` so we can reject
    * REPL meta-commands outside the `:type`/`:doc`/`:imports` allowlist
    * (blocking `:load`, `:sh`, `:quit`, ...), parse exactly once, and read
    * compile-error state structurally instead of scraping stdout.
    *
    * Executions in one session run one at a time; different sessions run in
    * parallel. Without an execution timeout, a call made while another runs
    * in the same session waits for it, forever if it never ends.
    */
  def run(code: String): ExecutionResult = synchronized:
    running = true
    try runLocked(code)
    finally running = false

  private def runLocked(code: String): ExecutionResult =
    // A finished abandoned thread would otherwise keep its result (a whole
    // REPL state) reachable.
    if abandoned.exists(!_.isAlive) then abandoned = None
    val violations = CodeValidator.validate(code)
    if blocked then
      ExecutionResult(false, "", Some(StillRunningMessage))
    else if violations.nonEmpty then
      ExecutionResult(false, "", Some(CodeValidator.formatErrors(violations)))
    else
      ParseResult(terminated(code))(using state) match
        case p: Parsed =>
          dispatch(p)
        case cmd @ (_: TypeOf | _: DocOf | Imports) =>
          dispatch(cmd)
        case _: Command =>
          ExecutionResult(false, "",
            Some("Only :type, :doc, and :imports REPL commands are allowed."))
        case Newline =>
          ExecutionResult(true, "")
        case SyntaxErrors(_, errors, _) =>
          ExecutionResult(false, "",
            Some("Syntax error:\n" + formatDiagnostics(errors)))
        case other =>
          ExecutionResult(false, "", Some(s"Unexpected parse result: $other"))

  /** `code` with trailing whitespace replaced by a single newline. The REPL
    * parser treats input that ends inside an indentation region without a
    * newline (`x match` followed by `case` lines, `def f =` with an indented
    * body, ...) as *incomplete* and reports "unindent expected, but eof
    * found"; that is how the interactive REPL knows to keep reading. Our
    * clients send complete snippets, so we close them ourselves. Only the
    * tail is touched, so line numbers in diagnostics are unaffected.
    */
  private def terminated(code: String): String =
    code.stripTrailing() + "\n"

  /** Dispatch a parse result, honoring the optional execution timeout. */
  private def dispatch(res: ParseResult): ExecutionResult =
    ctx.config.executionTimeoutMs match
      case None        => adopt(res, evaluate(res))
      case Some(limit) => dispatchWithTimeout(res, limit)

  /** Runs `res` against the current `state` and returns the resulting state
    * alongside captured output. Deliberately does *not* mutate `this.state`, so
    * the caller decides whether to adopt the new state, so a timed-out run can
    * be discarded without corrupting the session.
    */
  private def evaluate(res: ParseResult): (State, String, Option[Throwable]) =
    var newState = state
    val (output, thrown) = withOutputCapture(outputCapture, printStream):
      newState = driver.runParseResult(res)(using state)
    (newState, output, thrown)

  /** Adopt an evaluation result as the new session state and turn it into an
    * [[ExecutionResult]].
    *
    * For a [[Parsed]] we read compile-error state from its `StoreReporter`:
    * `errorCount` stays live even after the driver drains the diagnostic list
    * via `removeBufferedMessages`, so `hasErrors` is a structured signal
    * rather than a stdout heuristic. For allowlisted meta-commands (`:type`,
    * `:doc`, `:imports`) there is no reporter, so we treat dispatch as
    * successful and let any error text surface in the captured output.
    */
  private def adopt(res: ParseResult, evaluated: (State, String, Option[Throwable])): ExecutionResult =
    val (newState, output, thrown) = evaluated
    state = newState
    thrown match
      case Some(e) =>
        ExecutionResult(false, output, Option(e.getMessage))
      case None =>
        val compileFailed = res match
          case p: Parsed => p.reporter.hasErrors
          case _         => false
        ExecutionResult(!compileFailed, output)

  /** Run the evaluation on a worker thread, bounding it to `limitMs`.
    *
    * On timeout the client gets a prompt error instead of hanging, and the
    * session keeps its prior state (the abandoned statement has no observable
    * effect). The interrupt is best-effort: blocking I/O and sleeps respond to
    * it, but a CPU-bound loop (in REPL, library or JDK code) keeps
    * running in the background. Such a thread is kept as [[abandoned]] and
    * blocks only this session, until it ends. Its permissions are revoked as
    * soon as it times out, so it cannot keep acting after its caller has
    * moved on. This is *not* a hard sandbox; true preemption requires
    * process isolation.
    */
  private def dispatchWithTimeout(res: ParseResult, limitMs: Long): ExecutionResult =
    val resultRef = java.util.concurrent.atomic.AtomicReference[(State, String, Option[Throwable])]()
    // Running -> Abandoned (by this thread, counted in abandonedCount) ->
    // Finished (by the worker, uncounted if it was abandoned).
    val phase = java.util.concurrent.atomic.AtomicInteger(Running)
    val worker = Thread: () =>
      try resultRef.set(evaluate(res))
      finally
        if phase.getAndSet(Finished) == Abandoned then
          val _ = abandonedCount.decrementAndGet()
    worker.setName(s"tacit-repl-eval-$name")
    worker.setDaemon(true)
    worker.start()
    worker.join(limitMs)
    if worker.isAlive then
      // Before the interrupt: code that catches it and carries on must already
      // find every file system, network, process, LLM and permission call
      // refused, including grants a user approves later.
      // A library JAR older than this server has no `revoke`; the run must
      // still be interrupted and abandoned, or the next one would share the
      // driver with it.
      try revokeLibraryAccess(sandboxLoader, worker)
      catch case scala.util.control.NonFatal(e) =>
        Log.warn(s"REPL $name: could not revoke the permissions of a timed-out execution (${e.getClass.getName}: ${e.getMessage}); is the library JAR older than the server?")
      worker.interrupt()
      // The abandoned run compiled into the next wrapper object
      // (`rs$line$N`), whose class may already be loaded. Skip its name, or
      // the next run would load that class instead of its own.
      state = state.copy(objectIndex = state.objectIndex + 1)
      worker.join(InterruptGraceMs)
      val stillRunning = worker.isAlive
      if stillRunning then
        abandoned = Some(worker)
        abandonedCount.incrementAndGet()
        // The worker may have finished since isAlive; then it did not see
        // Abandoned and will not undo the count, so undo it here.
        if !phase.compareAndSet(Running, Abandoned) then
          val _ = abandonedCount.decrementAndGet()
        Log.log(s"REPL $name: an execution timed out and ignored its interrupt; its thread keeps running with its permissions revoked, and the REPL refuses code until it ends.")
      val message = s"Execution timed out after ${limitMs}ms" +
        (if stillRunning then
           ". It did not stop when interrupted and keeps running in the background; " +
             "this session cannot run code until it ends."
         else "")
      ExecutionResult(false, "", Some(message))
    else
      resultRef.get() match
        case null      => ExecutionResult(false, "", Some("Execution failed (no result; possible fatal error)"))
        case evaluated => adopt(res, evaluated)

  private inline val Running = 0
  private inline val Abandoned = 1
  private inline val Finished = 2

  private def formatDiagnostics(diags: List[Diagnostic]): String =
    diags.map: d =>
      val pos = d.pos
      if pos != null && pos.exists then s"Line ${pos.line + 1}: ${d.message}"
      else d.message
    .mkString("\n")
