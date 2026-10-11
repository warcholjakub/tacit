import tacit.executor.{BlockingOracle, ManagedRepl, ScalaExecutor, SessionManager}
import tacit.core.{Context, Config}


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

  test("a runaway loop is stopped at the timeout and the session keeps working"):
    given Context = Context(Config(executionTimeoutMs = Some(1500)), None) // safe mode
    val sm = new SessionManager
    val sid = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    try
      assert(sm.executeInSession(sid, "val x = 41").success)
      val abandonedBefore = ManagedRepl.abandonedExecutions
      for loop <- List(
          "while true do ()",
          // Catching the stop does not save it: the outer loop checks again.
          "while true do\n  try { while true do () } catch { case _: Throwable => () }"
        )
      do
        val stopped = sm.executeInSession(sid, loop)
        assert(stopped.error.exists(_.contains("timed out")), s"got: ${stopped.error}")
        assert(!stopped.error.exists(_.contains("keeps running")), s"got: ${stopped.error}")
        assert(!sm.getSession(sid).exists(_.blocked))
        assertEquals(ManagedRepl.abandonedExecutions, abandonedBefore)
      val after = sm.executeInSession(sid, "x + 1")
      assert(after.output.contains("42"), s"got: ${after.error.getOrElse(after.output)}")
    finally sm.deleteSession(sid)

  test("safe mode rejects starting threads, which stopping and revocation rely on"):
    given Context = Context(Config(), None)
    List(
      "Thread(() => ()).start()",
      "Thread.ofVirtual().start(() => ())",
      "Thread.startVirtualThread(() => ())",
      "java.util.concurrent.Executors.newSingleThreadExecutor()",
      "java.util.concurrent.CompletableFuture.runAsync(() => ())",
      "scala.concurrent.Future(1)(using scala.concurrent.ExecutionContext.global)",
      "java.util.Timer()"
    ).foreach: code =>
      val r = ScalaExecutor.execute(code)
      assert(!r.success, s"$code was accepted: ${r.output}")
      // Rejected by safe mode, not by some other compile error.
      assert(r.output.contains("from safe code"), s"$code failed for another reason: ${r.output}")

  test("a run stuck outside REPL code blocks only its own session"):
    val oracle = BlockingOracle()
    given Context = oracle.context(500)
    val sm = new SessionManager
    val stuck = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    val other = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    try
      val timedOut = sm.executeInSession(stuck, BlockingOracle.StuckCode)
      assert(timedOut.error.exists(_.contains("keeps running")), s"got: ${timedOut.error}")
      assert(sm.getSession(stuck).exists(_.blocked))

      val started = System.nanoTime
      val unaffected = sm.executeInSession(other, "1 + 1")
      assert(unaffected.success, s"got: ${unaffected.error.getOrElse(unaffected.output)}")
      assert((System.nanoTime - started) / 1000000 < 10000, "the other session waited for the stuck one")
      assert(!sm.getSession(other).exists(_.blocked))

      val refused = sm.executeInSession(stuck, "1 + 1")
      assertEquals(refused.error, Some(ManagedRepl.StillRunningMessage))

      oracle.release()
      BlockingOracle.eventually(!sm.getSession(stuck).exists(_.blocked), "the stuck run ends")
      val recovered = sm.executeInSession(stuck, "1 + 2")
      // Its own result, not the abandoned run's (both compile to a wrapper
      // object named after the run).
      assert(recovered.output.contains("3"), s"got: ${recovered.error.getOrElse(recovered.output)}")
    finally
      oracle.release()
      sm.deleteSession(stuck)
      sm.deleteSession(other)

  test("a stuck run's permission requests are refused once it times out"):
    val oracle = BlockingOracle()
    given Context = oracle.context(500)
    val sm = new SessionManager
    val sid = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    try
      // The first request waits in the oracle past the timeout; once released,
      // the second one would reach the oracle unless the run was revoked.
      val code =
        s"""try ${BlockingOracle.StuckCode} catch { case _: Throwable => () }
           |requestFileSystem("/tmp") { () }""".stripMargin
      val timedOut = sm.executeInSession(sid, code)
      assert(timedOut.error.exists(_.contains("keeps running")), s"got: ${timedOut.error}")
      oracle.release()
      BlockingOracle.eventually(!sm.getSession(sid).exists(_.blocked), "the stuck run ends")
      assertEquals(oracle.calls.get, 1, "the revoked run asked the oracle again")
    finally
      oracle.release()
      sm.deleteSession(sid)

  test("a deleted session whose run keeps going still counts toward the cap"):
    val oracle = BlockingOracle()
    given Context = oracle.context(500)
    val sm = new SessionManager(maxSessions = 1)
    val sid = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    try
      assert(!sm.executeInSession(sid, BlockingOracle.StuckCode).success)
      assert(sm.deleteSession(sid))
      val refused = sm.createSession()
      assert(refused.left.exists(_.contains("still running")), s"got: $refused")
      oracle.release()
      BlockingOracle.eventually(sm.createSession().fold(_ => false, id => { sm.deleteSession(id); true }), "a session can be created")
    finally oracle.release()
