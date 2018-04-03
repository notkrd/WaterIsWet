package models

import scala.collection.immutable.Seq
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import defs_etc._
import syntax._
import semantics._

object thisRoom {
  type ParseResult[A] = CombinatoryGrammar.ParseResult[A]
  type Sentence = CombinatoryGrammar.Sentence
  type Term = CombinatoryGrammar.Term
  
  val d_entities: Map[KeyPhrase, Entity] = Map("the rug" -> "BLUE_RUG", "the window" -> "WINDOW", "the peel" -> "ORANGE_PEEL", "ant" -> "ANT", "the dishes" -> "DISHES", "I" -> "AUTHOR", "the deer" -> "DEER")
  
  val isAlive: PredSing = Set("ANT", "AUTHOR", "DEER")
  val seeTuples: Set[Tuple2[Entity,Entity]] = (for {a_thing <- d_entities.values} yield ("AUTHOR", a_thing)).toSet + (("ANT","AUTHOR"))
  val doesSee: PredBin = tuplesToPredBin(seeTuples)
  
  val d_rel1: Map[KeyPhrase, PredSing] = Map("lives" -> isAlive)
  val d_rel2: Map[KeyPhrase, PredBin] = Map("sees" -> doesSee)
  
  val discRepresentation = new Box(Seq(), Seq())
  
  val d_model: Model = new Model(d_entities, d_rel1, d_rel2)
  
  def parseSentence(s: String): ParseResult[Sentence] = CombinatoryGrammar.EngSParser(d_model)(new CharSequenceReader(s))
  
  def evalSentence(s: String): Either[String, Set[Embedding]] = parseSentence(s) match {
    case CombinatoryGrammar.Error(e, r) => Left("Parser error with message " + e)
    case CombinatoryGrammar.Failure(f, r) => Left("Parser failed with message " + f)
    case CombinatoryGrammar.Success(s, r) => Right(d_model.Embeddings(s.meaning(discRepresentation)))
  }
}