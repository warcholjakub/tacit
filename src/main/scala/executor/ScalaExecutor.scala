package tacit
package executor

import core.Context

import scala.collection.concurrent.TrieMap
import java.util.UUID

/** Executes Scala code snippets */
object ScalaExecutor:

  /** Execute a Scala code snippet stateless and return the result */
  def execute(code: String)(using Context): ExecutionResult =
    ManagedRepl.abandonedLimitReached match
      case Some(reason) => ExecutionResult(false, "", Some(reason))
      case None         => ManagedRepl().init().run(code)

/** A REPL session that maintains state across executions */
class ReplSession(val id: String)(using Context):
  private val repl = ManagedRepl(id).init()

  /** Execute code in this session and return the result */
  def execute(code: String): ExecutionResult = repl.run(code)

  /** Whether a timed-out execution is still running, so the session refuses
    * code until it ends (see [[ManagedRepl.blocked]]). */
  def blocked: Boolean = repl.blocked

  /** Whether code is running in this session, or a timed-out execution
    * still is (see [[ManagedRepl.busy]]). */
  def busy: Boolean = repl.busy

object ReplSession:
  def create(using Context): ReplSession = ReplSession(UUID.randomUUID().toString)

/** Manages multiple REPL sessions. Thread-safe via TrieMap.
  *
  * The number of live sessions is capped: each session carries a full
  * compiler instance, so unbounded creation is a memory/DoS vector.
  */
class SessionManager(maxSessions: Int = SessionManager.MaxSessions)(using Context):
  private val sessions = TrieMap[String, ReplSession]()

  /** Deleted sessions still running code, timed out or not: they keep their
    * compiler in memory, so they count toward the cap until it ends. */
  private val retired = TrieMap[String, ReplSession]()

  /** Create a new session and return its ID, or a human-readable error when
    * the session cap is reached. */
  def createSession(): Either[String, String] = synchronized:
    retired.filterInPlace((_, session) => session.busy)
    if sessions.size + retired.size >= maxSessions then
      val stuck =
        if retired.isEmpty then ""
        else s", ${retired.size} of them deleted but still running code"
      Left(s"Session limit reached ($maxSessions active sessions$stuck); delete a session before creating a new one.")
    else ManagedRepl.abandonedLimitReached match
      case Some(reason) => Left(reason)
      case None =>
        val session = ReplSession.create
        sessions(session.id) = session
        Right(session.id)

  /** Delete a session by ID */
  def deleteSession(sessionId: String): Boolean = synchronized:
    sessions.remove(sessionId) match
      case Some(session) =>
        if session.busy then retired(sessionId) = session
        true
      case None => false

  /** Get a session by ID */
  def getSession(sessionId: String): Option[ReplSession] =
    sessions.get(sessionId)

  /** Execute code in a specific session */
  def executeInSession(sessionId: String, code: String): ExecutionResult =
    sessions.get(sessionId) match
      case Some(session) => session.execute(code)
      case None => ExecutionResult(false, "", Some(s"Session not found: $sessionId"))

  /** List all active session IDs */
  def listSessions(): List[String] =
    sessions.keys.toList

object SessionManager:
  /** Upper bound on simultaneously live sessions (each one is a full
    * compiler warmup; unbounded creation is a DoS vector). */
  val MaxSessions = 100
