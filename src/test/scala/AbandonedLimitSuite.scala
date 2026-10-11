package tacit.executor

import tacit.core.{Context, Config}

/** The process-wide limit on timed-out executions that keep running, and
  * deleted sessions that are still running code. Runs are kept stuck in a
  * [[BlockingOracle]], outside REPL code, until the test releases them. */
class AbandonedLimitSuite extends munit.FunSuite:
  private def awaitAbandoned(count: Int): Unit =
    BlockingOracle.eventually(ManagedRepl.abandonedExecutions == count, s"$count abandoned executions remain")

  // Runs left by other suites end on their own; the counts below are exact
  // only once they have.
  override def beforeEach(context: BeforeEach): Unit = awaitAbandoned(0)

  test("stateless executions and new sessions are refused while too many timed-out runs keep going"):
    val oracle = BlockingOracle()
    given Context = oracle.context(500)
    val saved = ManagedRepl.maxAbandoned
    ManagedRepl.maxAbandoned = 2
    try
      for _ <- 1 to 2 do
        val r = ScalaExecutor.execute(BlockingOracle.StuckCode)
        assert(r.error.exists(_.contains("keeps running")), s"got: ${r.error}")
      assertEquals(ManagedRepl.abandonedExecutions, 2)
      val refused = ScalaExecutor.execute("1 + 1")
      assert(refused.error.exists(_.contains("timed-out executions")), s"got: ${refused.error}")
      val noSession = (new SessionManager).createSession()
      assert(noSession.left.exists(_.contains("timed-out executions")), s"got: $noSession")

      oracle.release()
      awaitAbandoned(0)
      assert(ScalaExecutor.execute("1 + 1").success)
    finally
      oracle.release()
      ManagedRepl.maxAbandoned = saved

  test("a session deleted while its code runs or waits to run still counts toward the cap"):
    val oracle = BlockingOracle()
    given Context = oracle.context(60000) // no timeout during the test
    val sm = new SessionManager(maxSessions = 1)
    val id = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    val first = Thread(() => { val _ = sm.executeInSession(id, BlockingOracle.StuckCode) })
    val queued = Thread(() => { val _ = sm.executeInSession(id, "1 + 1") })
    try
      first.start()
      oracle.awaitCalls(1) // running, inside the oracle
      queued.start()
      Thread.sleep(100) // waiting behind it
      assert(sm.deleteSession(id))
      val refused = sm.createSession()
      assert(refused.left.exists(_.contains("still running")), s"got: $refused")
    finally
      oracle.release()
      first.join()
      queued.join()
    val created = sm.createSession()
    assert(created.isRight, s"got: $created")
    created.foreach(sm.deleteSession)
