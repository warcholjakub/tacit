package tacit
package core

import executor.CodeRecorder

case class Context(
  config: Config,
  recorder: Option[CodeRecorder],
  plugins: List[LoadedPlugin] = Nil,
)

object Context:

  def usingContext[R](config: Config, plugins: List[LoadedPlugin])(op: Context ?=> R): R =
    val recorder: Option[CodeRecorder] = config.recordPath.map: dir =>
      CodeRecorder(java.io.File(dir))
    val myCtx = Context(config, recorder, plugins)
    try op(using myCtx)
    finally recorder.foreach(_.close())

  def ctx(using c: Context): Context = c

end Context
