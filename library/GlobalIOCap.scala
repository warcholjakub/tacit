package tacit.library
import language.experimental.captureChecking
import caps.*

/** The global singleton instance of IOCapability. The REPL preamble, which
  * runs before safe mode is enabled, turns it into the session's `given`.
  *
  * Deliberately not `@assumeSafe`: agent code that could name it would get an
  * IOCapability without capturing one, e.g. `println(x)(using GlobalIOCap)`
  * inside a pure `Classified.map`, and print the classified value.
  */
object GlobalIOCap extends IOCapability
