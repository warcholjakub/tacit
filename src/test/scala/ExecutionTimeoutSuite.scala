import tacit.executor.{ScalaExecutor, SessionManager}
import tacit.core.{Context, Config}

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger

/** Tests for the optional wall-clock execution timeout (`executionTimeoutMs`).
  * The timeout runs evaluation on a worker thread; a timed-out run must return
  * an error to the client without corrupting later session state. `Thread.sleep`
  * stands in for a slow-but-interruptible operation; the validator allows it
  * (it is not `new Thread`). Safe mode is disabled here only because safe mode
  * would reject `Thread.sleep` as impure at compile time; the timeout mechanism
  * itself is independent of safe mode.
  */
class ExecutionTimeoutSuite extends munit.FunSuite:
  private def ctx(limitMs: Long): Context =
    Context(Config(safeMode = false, executionTimeoutMs = Some(limitMs)), None)

  test("execution within the timeout succeeds and returns its value"):
    val r = ScalaExecutor.execute("1 + 1")(using ctx(10000))
    assert(r.success, s"got: ${r.error.getOrElse(r.output)}")
    assert(r.output.contains("2"))

  test("a slow execution times out with an error"):
    val r = ScalaExecutor.execute("Thread.sleep(20000); 42")(using ctx(500))
    assert(!r.success)
    assert(r.error.exists(_.contains("timed out")), s"got: ${r.error}")
    assert(!r.output.contains("42"))

  test("a timed-out statement does not corrupt session state"):
    given Context = ctx(800)
    val sm = new SessionManager
    val sid = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    try
      assert(sm.executeInSession(sid, "val x = 41").success)
      val timedOut = sm.executeInSession(sid, "Thread.sleep(20000); x")
      assert(!timedOut.success, "expected the slow statement to time out")
      // The session must still be usable, with `x` intact from before the timeout.
      val after = sm.executeInSession(sid, "x + 1")
      assert(after.success, s"session broken after timeout: ${after.error.getOrElse(after.output)}")
      assert(after.output.contains("42"))
    finally sm.deleteSession(sid)

  test("a loop that ignores the interrupt blocks only its own session"):
    given Context = ctx(500)
    val sm = new SessionManager
    val stuck = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    val other = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    try
      // Spins for 4s without checking the interrupt flag.
      val loop = "val end = System.nanoTime + 4000000000L\nwhile System.nanoTime < end do ()"
      val timedOut = sm.executeInSession(stuck, loop)
      assert(!timedOut.success)
      assert(timedOut.error.exists(_.contains("keeps running")), s"got: ${timedOut.error}")

      val started = System.nanoTime
      val unaffected = sm.executeInSession(other, "1 + 1")
      assert(unaffected.success, s"got: ${unaffected.error.getOrElse(unaffected.output)}")
      assert((System.nanoTime - started) / 1000000 < 3000, "the other session waited for the loop")

      val refused = sm.executeInSession(stuck, "1 + 1")
      assertEquals(refused.error, Some(tacit.executor.ManagedRepl.StillRunningMessage))
      assert(sm.getSession(stuck).exists(_.blocked))
      assert(!sm.getSession(other).exists(_.blocked))

      Thread.sleep(4500)
      val recovered = sm.executeInSession(stuck, "1 + 2")
      assert(recovered.success, s"got: ${recovered.error.getOrElse(recovered.output)}")
      // Its own result, not the abandoned run's (both compile to a wrapper
      // object named after the run).
      assert(recovered.output.contains("3"), s"got: ${recovered.output}")
    finally
      sm.deleteSession(stuck)
      sm.deleteSession(other)

  test("a timed-out run that keeps going loses its permissions"):
    val dir = Files.createTempDirectory("tacit-revoke").toRealPath()
    val log = dir.resolve("log.txt")
    val asked = AtomicInteger()
    given Context = Context(
      Config(
        safeMode = false,
        executionTimeoutMs = Some(500),
        libraryConfig = io.circe.Json.obj(
          "allowedRoots" -> io.circe.Json.arr(io.circe.Json.fromString(dir.toString))
        )
      ),
      None,
      permissionOracle = Some: _ =>
        asked.incrementAndGet()
        """{"allow":true}"""
    )
    val sm = new SessionManager
    val sid = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    try
      // Writes inside its root and asks the oracle (for a root outside it)
      // in a loop that swallows every failure, the interrupt included.
      val code = s"""
        |val end = System.nanoTime + 3000000000L
        |while System.nanoTime < end do
        |  try
        |    requestFileSystem("$dir") { access("$log").append("x") }
        |    requestFileSystem("/") { access("/").exists }
        |  catch case _: Throwable => ()
        |  try Thread.sleep(20) catch case _: Throwable => ()
        |""".stripMargin
      val timedOut = sm.executeInSession(sid, code)
      assert(timedOut.error.exists(_.contains("keeps running")), s"got: ${timedOut.error}")
      assert(Files.size(log) > 0 && asked.get > 0, "the loop never ran before the timeout")
      Thread.sleep(100)
      val (size, asks) = (Files.size(log), asked.get)
      Thread.sleep(600)
      assertEquals(Files.size(log), size, "the abandoned run kept writing")
      assertEquals(asked.get, asks, "the abandoned run kept asking the oracle")
    finally sm.deleteSession(sid)

  test("a deleted session whose run keeps going still counts toward the cap"):
    given Context = ctx(300)
    val sm = new SessionManager(maxSessions = 1)
    val sid = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    val loop = "val end = System.nanoTime + 2500000000L\nwhile System.nanoTime < end do ()"
    assert(!sm.executeInSession(sid, loop).success)
    assert(sm.deleteSession(sid))
    val refused = sm.createSession()
    assert(refused.left.exists(_.contains("still running")), s"got: $refused")
    Thread.sleep(3000)
    val created = sm.createSession()
    assert(created.isRight, s"got: $created")
    created.foreach(sm.deleteSession)
