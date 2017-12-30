package semantics
import syntax.{Box}
import defs_etc._

class BoxSem(world: Model, discourse: Box)  {
  lazy val incorporate_utterance: Utterance => BoxSem = (_) => this
  lazy val check_discourse: Boolean = world.BoxSatisfiable(discourse)
  lazy val curr_interpretations: Set[Embedding] = world.Embeddings(discourse)
  lazy val clear_conversation: BoxSem = new BoxSem(world, new Box(Seq(),Seq()))
}