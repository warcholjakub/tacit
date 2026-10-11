package tacit.executor

import tacit.core.{Context, Config}

/** The process-wide limit on timed-out executions that keep running, and
  * deleted sessions that are still running code. Safe mode is off so the
  * loops can sleep: each one swallows its interrupt and ends after a few
  * seconds. */
class AbandonedLimitSuite extends munit.FunSuite:
  private given Context = Context(Config(safeMode = false, executionTimeoutMs = Some(300)), None)

  private def loop(seconds: Int): String =
    s"""val end = System.nanoTime + ${seconds}000000000L
       |while System.nanoTime < end do
       |  try Thread.sleep(50) catch case _: InterruptedException => ()""".stripMargin

  private def awaitAbandoned(count: Int): Unit =
    val deadline = System.nanoTime + 15000000000L
    while ManagedRepl.abandonedExecutions > count && System.nanoTime < deadline do Thread.sleep(100)
    assertEquals(ManagedRepl.abandonedExecutions, count)

  test("stateless executions and new sessions are refused while too many timed-out runs keep going"):
    val baseline = ManagedRepl.abandonedExecutions
    val saved = ManagedRepl.maxAbandoned
    ManagedRepl.maxAbandoned = baseline + 2
    try
      for _ <- 1 to 2 do
        val r = ScalaExecutor.execute(loop(4))
        assert(r.error.exists(_.contains("keeps running")), s"got: ${r.error}")
      assertEquals(ManagedRepl.abandonedExecutions, baseline + 2)
      val refused = ScalaExecutor.execute("1 + 1")
      assert(refused.error.exists(_.contains("timed-out executions")), s"got: ${refused.error}")
      val sm = new SessionManager
      val noSession = sm.createSession()
      assert(noSession.left.exists(_.contains("timed-out executions")), s"got: $noSession")

      awaitAbandoned(baseline)
      assert(ScalaExecutor.execute("1 + 1").success)
    finally ManagedRepl.maxAbandoned = saved

  test("a session deleted while its code runs, before any timeout, still counts toward the cap"):
    val baseline = ManagedRepl.abandonedExecutions
    val sm = new SessionManager(maxSessions = 1)
    val id = sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)
    val call = Thread(() => { val _ = sm.executeInSession(id, loop(3)) })
    call.start()
    Thread.sleep(150) // the run has started but not timed out yet
    assert(sm.deleteSession(id))
    val refused = sm.createSession()
    assert(refused.left.exists(_.contains("still running")), s"got: $refused")
    call.join()
    awaitAbandoned(baseline)
    val created = sm.createSession()
    assert(created.isRight, s"got: $created")
    created.foreach(sm.deleteSession)
