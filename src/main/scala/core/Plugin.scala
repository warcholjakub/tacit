package tacit
package core

import io.circe.*
import io.circe.parser.decode

import java.io.File as JFile
import java.nio.charset.StandardCharsets
import java.util.jar.JarFile
import scala.util.Using

enum ApiMode:
  case ExtendCore, ReplaceCore

object ApiMode:
  given Decoder[ApiMode] = Decoder.decodeString.emap:
    case "extend-core"  => Right(ApiMode.ExtendCore)
    case "replace-core" => Right(ApiMode.ReplaceCore)
    case other          => Left(s"Unsupported apiMode: $other")

case class PluginEntrypoint(
  preamble: String = "preamble.scala",
  apiDocs: String = "api-docs.md",
)
object PluginEntrypoint:
  // Hand-written so manifest defaults survive missing JSON fields. Circe 0.14's
  // semiauto derivation treats `String = "preamble.scala"` as required and
  // would fail on `"entrypoint": {}`.
  given Decoder[PluginEntrypoint] = Decoder.instance: c =>
    for
      preamble <- c.getOrElse[String]("preamble")("preamble.scala")
      apiDocs  <- c.getOrElse[String]("apiDocs")("api-docs.md")
    yield PluginEntrypoint(preamble, apiDocs)

case class PluginManifest(
  schemaVersion: Int,
  id: String,
  name: String,
  version: String,
  apiMode: ApiMode,
  domain: Option[String] = None,
  description: Option[String] = None,
  entrypoint: Option[PluginEntrypoint] = None,
  samplePrompts: List[String] = Nil,
  /** IDs of plugins this one depends on (must also be loaded). The loader
    * orders plugins topologically and rejects cycles, self-deps, and missing
    * required IDs.
    */
  requires: List[String] = Nil,
):
  def resolvedEntrypoint: PluginEntrypoint = entrypoint.getOrElse(PluginEntrypoint())

object PluginManifest:
  // Hand-written for the same reason as [[PluginEntrypoint]]'s decoder: circe
  // 0.14 semiauto ignores case-class defaults for non-Option fields, so
  // `samplePrompts: List[String] = Nil` would be treated as required.
  given Decoder[PluginManifest] = Decoder.instance: c =>
    for
      schemaVersion <- c.downField("schemaVersion").as[Int]
      id            <- c.downField("id").as[String]
      name          <- c.downField("name").as[String]
      version       <- c.downField("version").as[String]
      apiMode       <- c.downField("apiMode").as[ApiMode]
      domain        <- c.downField("domain").as[Option[String]]
      description   <- c.downField("description").as[Option[String]]
      entrypoint    <- c.downField("entrypoint").as[Option[PluginEntrypoint]]
      samplePrompts <- c.getOrElse[List[String]]("samplePrompts")(Nil)
      requires      <- c.getOrElse[List[String]]("requires")(Nil)
    yield PluginManifest(
      schemaVersion, id, name, version, apiMode,
      domain, description, entrypoint, samplePrompts, requires,
    )

case class LoadedPlugin(
  jarPath: String,
  manifest: PluginManifest,
  preamble: String,
  apiDocs: String,
)

object PluginLoader:

  /** Scan a folder for top-level `*.jar` files. Non-recursive. Returns absolute
    * paths sorted alphabetically by file name (stable composition order).
    */
  def scanDir(dir: String): Either[String, List[String]] =
    val d = JFile(dir)
    if !d.exists() then Left(s"Plugin scan dir not found: $dir")
    else if !d.isDirectory then Left(s"Plugin scan dir is not a directory: $dir")
    else
      val jars = Option(d.listFiles((_, name) => name.endsWith(".jar")))
        .map(_.toList)
        .getOrElse(Nil)
      Right(jars.sortBy(_.getName).map(_.getAbsolutePath))

  /** Load every plugin JAR in order. Fail-fast: a single failure aborts the
    * whole batch. Also rejects mixing `extend-core` and `replace-core` plugins
    * in the same process, since their API surfaces cannot compose unambiguously
    * (one wants to extend the core preamble, the other wants to replace it).
    */
  def loadAll(paths: List[String]): Either[String, List[LoadedPlugin]] =
    loadAll(paths, Nil)

  /** Load explicit `jars` plus every `*.jar` discovered under each entry of
    * `scanDirs`. Explicit jars come first; scanned dirs append after, in
    * declaration order. Duplicates by absolute path are removed (first wins).
    */
  def loadAll(
      jars: List[String],
      scanDirs: List[String],
  ): Either[String, List[LoadedPlugin]] =
    val zeroDirs: Either[String, List[String]] = Right(Nil)
    val scanned = scanDirs.foldLeft(zeroDirs): (acc, d) =>
      for
        xs <- acc
        ys <- scanDir(d)
      yield xs ++ ys
    scanned.flatMap: scannedPaths =>
      val explicit = jars.map(p => JFile(p).getAbsolutePath)
      val seen     = scala.collection.mutable.LinkedHashSet.empty[String]
      val allPaths = (explicit ++ scannedPaths).filter(p => seen.add(p))
      val zero: Either[String, List[LoadedPlugin]] = Right(Nil)
      val loaded = allPaths.toList.foldLeft(zero): (acc, path) =>
        for
          xs <- acc
          x  <- load(path)
        yield xs :+ x
      loaded.flatMap(validateAndOrder)

  def load(path: String): Either[String, LoadedPlugin] =
    val file = JFile(path)
    if !file.exists() then Left(s"Plugin JAR not found: $path")
    else
      val attempt = Using(JarFile(file)): jar =>
        for
          json     <- readResource(jar, "tacit-plugin.json", path)
          manifest <- decode[PluginManifest](json).left.map: err =>
            s"Invalid tacit-plugin.json in $path: ${err.getMessage}"
          _        <- validateManifest(manifest, path)
          ep        = manifest.resolvedEntrypoint
          preamble <- readResource(jar, ep.preamble, path)
          apiDocs  <- readResource(jar, ep.apiDocs, path)
        yield LoadedPlugin(file.getAbsolutePath, manifest, preamble, apiDocs)
      attempt.toEither match
        case Left(t)            => Left(s"Failed to read plugin JAR $path: ${t.getMessage}")
        case Right(Left(msg))   => Left(msg)
        case Right(Right(plug)) => Right(plug)

  private def readResource(jar: JarFile, name: String, path: String): Either[String, String] =
    Option(jar.getEntry(name))
      .toRight(s"Plugin JAR $path is missing required resource: $name")
      .flatMap: entry =>
        Using(jar.getInputStream(entry)): in =>
          new String(in.readAllBytes(), StandardCharsets.UTF_8)
        .toEither.left.map(t => s"Failed to read $name from $path: ${t.getMessage}")

  private def validateManifest(m: PluginManifest, path: String): Either[String, Unit] =
    if m.schemaVersion != 1      then Left(s"Unsupported plugin schemaVersion in $path: ${m.schemaVersion}")
    else if m.id.trim.isEmpty    then Left(s"Plugin id must not be empty in $path")
    else if m.name.trim.isEmpty  then Left(s"Plugin name must not be empty in $path")
    else if m.version.trim.isEmpty then Left(s"Plugin version must not be empty in $path")
    else Right(())

  /** Validate a hand-built list and return it in topological order (dependencies
    * before dependents; alphabetical `jarPath` tie-break within a stratum).
    *
    * Failure modes (any one returns `Left`):
    *  - Duplicate plugin IDs across the list.
    *  - A plugin declares itself as a `requires` entry.
    *  - A plugin requires an ID that is not in the loaded set.
    *  - A dependency cycle exists.
    *  - Mixed `extend-core` and `replace-core` plugins in the same set.
    */
  def validateAndOrder(plugins: List[LoadedPlugin]): Either[String, List[LoadedPlugin]] =
    for
      _       <- checkNoDuplicateIds(plugins)
      _       <- checkNoSelfDeps(plugins)
      _       <- checkRequiresPresent(plugins)
      _       <- checkNoMixedMode(plugins)
      ordered <- topologicallyOrder(plugins)
    yield ordered

  private def checkNoDuplicateIds(plugins: List[LoadedPlugin]): Either[String, Unit] =
    val dups = plugins.groupBy(_.manifest.id).filter((_, xs) => xs.size > 1).keys.toList.sorted
    if dups.isEmpty then Right(())
    else Left(s"Duplicate plugin id(s): ${dups.mkString(", ")}")

  private def checkNoSelfDeps(plugins: List[LoadedPlugin]): Either[String, Unit] =
    val selfDeps = plugins.filter(p => p.manifest.requires.contains(p.manifest.id))
    if selfDeps.isEmpty then Right(())
    else Left(s"Plugin(s) declare themselves in `requires`: ${selfDeps.map(_.manifest.id).mkString(", ")}")

  private def checkRequiresPresent(plugins: List[LoadedPlugin]): Either[String, Unit] =
    val loadedIds = plugins.map(_.manifest.id).toSet
    val problems = plugins.flatMap: p =>
      val missing = p.manifest.requires.filterNot(loadedIds.contains)
      if missing.isEmpty then None
      else Some(s"${p.manifest.id} requires missing plugin(s): ${missing.mkString(", ")}")
    if problems.isEmpty then Right(())
    else
      val avail = if loadedIds.isEmpty then "(none)" else loadedIds.toList.sorted.mkString(", ")
      Left(s"${problems.mkString("; ")}. Available plugin IDs: $avail")

  private def checkNoMixedMode(plugins: List[LoadedPlugin]): Either[String, Unit] =
    val modes = plugins.map(_.manifest.apiMode).distinct
    if modes.contains(ApiMode.ExtendCore) && modes.contains(ApiMode.ReplaceCore) then
      Left("Cannot mix replace-core and extend-core plugins in the same TACIT process")
    else Right(())

  /** Kahn's algorithm: peel nodes whose `requires` are all satisfied. Within a
    * stratum, preserve the caller's input order (which itself reflects explicit
    * `--plugin` order followed by alphabetical scan-dir order). Stable on
    * cycles too: cycle members are reported in input order.
    */
  private def topologicallyOrder(plugins: List[LoadedPlugin]): Either[String, List[LoadedPlugin]] =
    @annotation.tailrec
    def peel(
        ordered: List[LoadedPlugin],
        remaining: List[LoadedPlugin],
    ): Either[String, List[LoadedPlugin]] =
      if remaining.isEmpty then Right(ordered.reverse)
      else
        val satisfied = ordered.map(_.manifest.id).toSet
        // `filter` preserves the caller's input order.
        val ready = remaining.filter(p => p.manifest.requires.forall(satisfied.contains))
        if ready.isEmpty then
          val cycleIds = remaining.map(_.manifest.id).mkString(", ")
          Left(s"Plugin dependency cycle among: $cycleIds")
        else
          val nextOrdered = ready.foldLeft(ordered)((acc, p) => p :: acc)
          val readySet = ready.toSet
          peel(nextOrdered, remaining.filterNot(readySet.contains))

    peel(Nil, plugins)
