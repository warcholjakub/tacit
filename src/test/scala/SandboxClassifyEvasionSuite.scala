import tacit.executor.ScalaExecutor
import tacit.core.{Context, Config}
import java.nio.file.Files

/** Regression: a RELATIVE classified pattern (one containing '/', e.g. the docs'
  * `secrets/keys`) must not be evadable by requesting a filesystem root INSIDE
  * it. Such patterns are anchored to the request root; an agent that rooted
  * deeper stripped the leading pattern components and read the classified file
  * in plaintext. The matcher now also anchors at every ancestor of the request
  * root, so re-rooting cannot hide a classified path. Safe mode ON. */
class SandboxClassifyEvasionSuite extends munit.FunSuite:
  val base = Files.createTempDirectory("tacit-classify-gap").nn
  val secretDir = base.resolve("secrets").nn
  Files.createDirectories(secretDir)
  val secretFile = secretDir.resolve("apikey").nn
  Files.writeString(secretFile, "RELCLASSIFIED_SECRET_123")

  given Context = Context(Config(libraryConfig = io.circe.Json.obj(
    "allowedRoots" -> io.circe.Json.arr(io.circe.Json.fromString(base.toString)),
    "classifiedPaths" -> io.circe.Json.arr(io.circe.Json.fromString("secrets/apikey"))
  )), None)

  private def run(code: String) = ScalaExecutor.execute(code)

  test("relative classified pattern cannot be evaded by re-rooting inside it"):
    // Rooted at the top: classified, plaintext read refused.
    val top = run(s"""
      requestFileSystem("${base}") {
        try access("${secretFile}").read() catch { case e: Throwable => "BLOCKED" }
      }
    """)
    assert(top.output.contains("BLOCKED"),
      s"control failed, expected classified at top root: ${top.output}")

    // Rooted INSIDE secrets/: must STILL be classified (no plaintext).
    val inside = run(s"""
      requestFileSystem("${secretDir}") {
        try access("${secretFile}").read() catch { case e: Throwable => "BLOCKED" }
      }
    """)
    assert(!inside.output.contains("RELCLASSIFIED_SECRET_123"),
      s"EVASION: secret leaked in plaintext when rooting inside: ${inside.output}")
    assert(inside.output.contains("BLOCKED"),
      s"expected classified when rooting inside: ${inside.output}")

    val isClass = run(s"""
      requestFileSystem("${secretDir}") { access("${secretFile}").isClassified }
    """)
    assert(isClass.output.contains("true"),
      s"isClassified must be true when rooting inside: ${isClass.output}")
