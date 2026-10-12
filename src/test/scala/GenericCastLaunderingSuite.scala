import tacit.executor.ScalaExecutor
import tacit.core.{Context, Config}

/** Regression: a type pattern against an abstract type parameter erases to a
  * plain cast (only an *unchecked* warning, no `asInstanceOf`), which was a way
  * to launder an impure, capability-capturing closure into a pure function type
  * that `Classified.map` accepts — exfiltrating a classified secret. The REPL
  * now escalates the unchecked category to an error (`-Wconf:cat=unchecked:e`). */
class GenericCastLaunderingSuite extends munit.FunSuite:
  given Context = Context(Config(libraryConfig = io.circe.Json.obj(
    "allowedRoots" -> io.circe.Json.arr(io.circe.Json.fromString("/"))
  )), None)

  test("classified secret cannot be laundered out of Classified.map via a generic type-pattern cast"):
    val r = ScalaExecutor.execute("""
      def cast[A, B](a: A): B = a match { case b: B => b }
      val secret = classify("pw-REGRESSION-123")
      requestFileSystem("/tmp") {
        val impure: String => String = (s: String) => { access("/tmp/exfil-regression.txt").write(s); s }
        secret.map(cast[String => String, String -> String](impure)); ()
      }
      requestFileSystem("/tmp") {
        try access("/tmp/exfil-regression.txt").read() catch { case _: Throwable => "no-file" }
      }
    """)
    assert(!r.success, s"the laundering cast must not compile, got success with: ${r.output}")
    assert(!r.output.contains("pw-REGRESSION-123"), s"classified secret leaked: ${r.output}")

  test("legitimate pattern matches and filesystem use still compile and run"):
    val r = ScalaExecutor.execute("""
      val x: Option[Int] = Some(21)
      val doubled = x match { case Some(v) => v * 2; case None => 0 }
      requestFileSystem("/tmp") {
        val f = access("/tmp/legit-regression.txt"); f.write(s"d=$doubled"); val c = f.read(); f.delete(); c
      }
    """)
    assert(r.success, s"legit code must still work: ${r.error.getOrElse(r.output)}")
    assert(r.output.contains("d=42"), s"unexpected output: ${r.output}")
