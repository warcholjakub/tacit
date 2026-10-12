import tacit.executor.ScalaExecutor
import tacit.core.{Context, Config}

/** Attempted sandbox escapes that safe mode must keep rejecting.
  *
  * Each test runs an escape attempt through the REPL with safe mode on (the
  * Config default) and asserts the secure outcome: capabilities captured into
  * `Classified.map`, impure closures laundered into pure function types, and a
  * capability stashed to outlive its scope. `result.output` is included in every
  * message so the compiler's or runtime's behavior is visible on failure.
  */
class SandboxHardeningProbeSuite extends munit.FunSuite:
  // allowedRoots "/" so the probes test Classified and capture laundering,
  // not the root bound.
  given Context = Context(Config(libraryConfig = io.circe.Json.obj(
    "allowedRoots" -> io.circe.Json.arr(io.circe.Json.fromString("/"))
  )), None)

  private def run(code: String) = ScalaExecutor.execute(code)

  // ── P1: println the raw plaintext inside Classified.map ────────────────────
  // map's closure receives the UNWRAPPED String. If it may capture the ambient
  // `given IOCapability`, `println(plaintext)` leaks the secret to normal output.
  test("P1 println-in-map does not leak plaintext"):
    val r = run("""
      val sec = classify("LEAK_P1_SENTINEL")
      sec.map(x => { println(x); x })
      println("done-p1")
    """)
    assert(!r.output.contains("LEAK_P1_SENTINEL"),
      s"P1 LEAK: plaintext printed. success=${r.success} output=${r.output} err=${r.error}")

  // ── P1b: print (not println) the plaintext ────────────────────────────────
  test("P1b print-in-map does not leak plaintext"):
    val r = run("""
      val sec = classify("LEAK_P1B_SENTINEL")
      sec.map(x => { print(x); x })
      println("done-p1b")
    """)
    assert(!r.output.contains("LEAK_P1B_SENTINEL"),
      s"P1b LEAK: success=${r.success} output=${r.output}")

  // ── P1c: println via an agent helper that takes `using IOCapability` ───────
  test("P1c println via helper does not leak plaintext"):
    val r = run("""
      def leak(x: String)(using IOCapability): String = { println(x); x }
      val sec = classify("LEAK_P1C_SENTINEL")
      sec.map(x => leak(x))
      println("done-p1c")
    """)
    assert(!r.output.contains("LEAK_P1C_SENTINEL"),
      s"P1c LEAK: success=${r.success} output=${r.output}")

  // ── P2: capture the FileSystem capability inside map and write the secret ──
  test("P2 fs-write-in-map must not compile / leak"):
    val r = run("""
      requestFileSystem("/tmp") {
        val sec = classify("LEAK_P2_SENTINEL")
        sec.map(x => { access("/tmp/probe-p2.txt").write(x); x })
      }
    """)
    val back = run("""
      requestFileSystem("/tmp") {
        try access("/tmp/probe-p2.txt").read() catch { case _: Throwable => "no-file" }
      }
    """)
    assert(!back.output.contains("LEAK_P2_SENTINEL"),
      s"P2 LEAK: secret written to disk. p2.success=${r.success} p2.out=${r.output} read=${back.output}")

  // ── P4: explicit impure->pure laundering function ─────────────────────────
  // Returning an impure `String => String` where a pure `String -> String` is
  // expected must be rejected at the `launder` definition.
  test("P4 explicit launder def must not compile"):
    val r = run("""
      def launder(f: String => String): String -> String = f
      requestFileSystem("/tmp") {
        val sec = classify("LEAK_P4_SENTINEL")
        val impure: String => String = s => { access("/tmp/probe-p4.txt").write(s); s }
        sec.map(launder(impure))
      }
    """)
    val back = run("""
      requestFileSystem("/tmp") {
        try access("/tmp/probe-p4.txt").read() catch { case _: Throwable => "no-file" }
      }
    """)
    assert(!r.success, s"P4: launder def unexpectedly compiled. output=${r.output}")
    assert(!back.output.contains("LEAK_P4_SENTINEL"), s"P4 LEAK: read=${back.output}")

  // ── P5: direct assignment impure -> pure type ─────────────────────────────
  test("P5 impure-to-pure assignment must not compile"):
    val r = run("""
      requestFileSystem("/tmp") {
        val impure: String => String = s => { access("/tmp/probe-p5.txt").write(s); s }
        val pure: String -> String = impure
        classify("LEAK_P5_SENTINEL").map(pure)
      }
    """)
    assert(!r.success, s"P5: impure accepted as pure. output=${r.output}")

  // ── P6: Conversion-based laundering ───────────────────────────────────────
  test("P6 given Conversion launder must not compile/leak"):
    val r = run("""
      given Conversion[String => String, String -> String] = f => f
      requestFileSystem("/tmp") {
        val sec = classify("LEAK_P6_SENTINEL")
        val impure: String => String = s => { access("/tmp/probe-p6.txt").write(s); s }
        sec.map(impure)
      }
    """)
    val back = run("""
      requestFileSystem("/tmp") {
        try access("/tmp/probe-p6.txt").read() catch { case _: Throwable => "no-file" }
      }
    """)
    assert(!back.output.contains("LEAK_P6_SENTINEL"),
      s"P6 LEAK: success=${r.success} out=${r.output} read=${back.output}")

  // ── P7: stash the FileSystem capability in a top-level var to outlive scope ─
  test("P7 capability must not escape its scope via var"):
    val r = run("""
      var stash: FileSystem = null
      requestFileSystem("/tmp") { stash = summon[FileSystem] }
      stash.access("/etc/passwd").read()
    """)
    assert(!r.success, s"P7: capability escaped scope. output=${r.output}")

  // ── P8: summon the ambient IOCapability and keep it (sanity: printing is ok,
  // but capturing it into a Classified.map is the real question, covered by P1).
  test("P8 IOCapability cannot be used to unwrap Classified"):
    val r = run("""
      val sec = classify("LEAK_P8_SENTINEL")
      // Attempt: funnel through map to a plain String escaping the wrapper.
      val escaped: String = sec.map(x => x).toString
      println(escaped)
    """)
    assert(!r.output.contains("LEAK_P8_SENTINEL"),
      s"P8 LEAK: success=${r.success} output=${r.output}")

  // ── P9: read a classified file then println the mapped identity ───────────
  // Uses a real classified path (.env under /tmp) to ensure the plaintext path,
  // not just classify(), is covered by the println defense.
  test("P9 classified-file plaintext not leaked via println-in-map"):
    val setup = run("""
      requestFileSystem("/tmp") {
        writeClassified("/tmp/probe-p9-dir/.env", classify("SECRET_P9=LEAK_P9_SENTINEL"))
      }
    """)
    val r = run("""
      requestFileSystem("/tmp") {
        val sec = readClassified("/tmp/probe-p9-dir/.env")
        sec.map(x => { println(x); x })
        println("done-p9")
      }
    """)
    assert(!r.output.contains("LEAK_P9_SENTINEL"),
      s"P9 LEAK: setup.success=${setup.success} success=${r.success} output=${r.output}")
