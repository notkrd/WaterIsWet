package tests

import scala.collection.immutable.Seq
import scala.util.parsing.combinator._

import defs_etc._
import semantics._
import syntax._

object tests {
  def main(args: Array[String]) = {
    model_tests()
    embedding_tests()
    parser_tests()
  }
  
  val textbook_entities = Map("Anwar" -> "AS", "Mohammed" -> "MA", "Noam" -> "NC", "John" -> "JM")
  def is_bald = Set(textbook_entities("Anwar"), textbook_entities("John"))
  
  def does_love: Entity => Entity => Boolean = (e1: Entity) => (e2: Entity) => Seq(e1,e2) match {
    case Seq(e1, e2)  if e1 == e2 => true
    case _ => false
  }
  val textbook_r1 = Map("is bald" -> is_bald, "balds" -> is_bald)
  val textbook_r2 = Map("loves" -> does_love)
  val textbook_model = new Model(textbook_entities, textbook_r1, textbook_r2)
  
  def model_tests() = {
    
    println(is_bald("JM") == true)
    println(does_love("NC")("NC") == true)
    println(does_love("MA")("JM") == false)
    println(textbook_model.SemR1("is bald")("John") == true)
    println(textbook_model.SemR2("loves")("Noam")("Noam") == true)
    println(textbook_model.SemR2("loves")("Mohammed")("John") == false)
    
    println(is_bald("JM") == true)
    println(does_love("NC")("NC") == true)
    println(does_love("MA")("JM") == false)
    println(textbook_model.SemR1("is bald")("John") == true)
    println(textbook_model.SemR2("loves")("Noam")("Noam") == true)
    println(textbook_model.SemR2("loves")("Mohammed")("John") == false)
  }
  
  val SampleBox: Box = (new Box(
      Vector("X","Y","W","Z"),
      Vector(var_assignment("Z","AS"),
          var_equality("W","Z"),
          pred_sing("is bald","X")))).Merge(new Box(
              Vector("Y"),
              Vector(pred_bin("loves","X","Y"))))
  
  val SampleBox2: Box = (new Box(
      Vector("X","Y","W","Z"),
      Vector(var_assignment("Z","ANT"),
          var_equality("W","Z"),
          pred_sing("lives","X")))).Merge(new Box(
              Vector("Y"),
              Vector(pred_bin("sees","X","Y"))))
              
  def embedding_tests() = {
    println(textbook_model Embeddings SampleBox)
    println(textbook_model.PlausibleEmbeddingsOnVars(SampleBox)(SampleBox.the_vars.toSeq).size)
    println(models.thisRoom.d_model Embeddings SampleBox2)
    println(models.thisRoom.d_model.PlausibleEmbeddingsOnVars(SampleBox2)(SampleBox2.the_vars.toSeq).size)
  }
  
  def parser_tests() = {
    println(CombinatoryGrammar.parse(CombinatoryGrammar.accept('h'),"hello"))
    println(CombinatoryGrammar.parse("hi": CombinatoryGrammar.Parser[String],"hi there"))
    println(CombinatoryGrammar.parse("hi": CombinatoryGrammar.Parser[String],"bye there"))
    println(CombinatoryGrammar.parse(CombinatoryGrammar.parsePhrase[Referent, CombinatoryGrammar.Term](CombinatoryGrammar.NP("Jeet")), "! Jeet lives"))
    val j_bald = CombinatoryGrammar.parse(CombinatoryGrammar.EngSParser(textbook_model), "John balds")
    println(j_bald)
    println(textbook_model.Embeddings(CombinatoryGrammar.unwrapSent(j_bald)))
    println(models.thisRoom.evalSentence("ant lives"))
  }
  
}