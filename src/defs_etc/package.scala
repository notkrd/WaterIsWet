
import scala.collection.immutable.Seq

/* Relevant type synonyms. */
package object defs_etc {
  type KeyPhrase = String
  type Variable = String
  type Entity = String
  type Referent = Either[Variable, KeyPhrase]
  type Utterance = String
  type PredSing = Entity => Boolean
  type PredBin = Entity => Entity => Boolean
  type Embedding = Map[Variable, Entity]
  type Lexicon[A] = Map[A, Set[Utterance]]
  
  implicit def RefVar(r: Referent): Variable = r match {
    case Left(v) => v
    case Right(v) => v
  }
  
  def tuplesToPredBin(tups: Set[Tuple2[Entity,Entity]]): PredBin = (x: Entity) => (y: Entity) => {
    tups.contains((x,y))
  }
}