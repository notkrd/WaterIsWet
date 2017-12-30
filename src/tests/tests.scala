package tests

import scala.util.parsing.combinator._

import defs_etc._
import semantics._
import syntax._

object tests {
  def main(args: Array[String]) = {
    model_tests()
    parser_tests()
    embedding_tests()
  }
  
  val textbook_entities = Map("Anwar" -> "AS", "Mohammed" -> "MA", "Noam" -> "NC", "John" -> "JM")
  def is_bald = Set(textbook_entities("Anwar"), textbook_entities("John"))
  
  def does_love: Entity => Entity => Boolean = (e1: Entity) => (e2: Entity) => Seq(e1,e2) match {
    case Seq(e1, e2)  if e1 == e2 => true
    case _ => false
  }
  val textbook_r1 = Map("bald" -> is_bald)
  val textbook_r2 = Map("loves" -> does_love)
  val textbook_model = new Model(textbook_entities, textbook_r1, textbook_r2)
  
  def model_tests() = {
    
    println(is_bald("JM") == true)
    println(does_love("NC")("NC") == true)
    println(does_love("MA")("JM") == false)
    println(textbook_model.SemR1("bald")("John") == true)
    println(textbook_model.SemR2("loves")("Noam")("Noam") == true)
    println(textbook_model.SemR2("loves")("Mohammed")("John") == false)
  }
  
  val SampleBox: Box = (new Box(
      Set("X","Y","W","Z"),
      Set(var_assignment("Z","AS"),
          var_equality("W","Z"),
          pred_sing("bald","X")))).Merge(new Box(
              Set("Y"),
              Set(pred_bin("loves","X","Y"))))
  
  def embedding_tests() = {
    println(textbook_model Embeddings SampleBox)
    println(textbook_model.PlausibleEmbeddingsOnVars(SampleBox)(SampleBox.the_vars.toSeq).size)
  }
  
  def parser_tests() = {
    object Whatever extends RegexParsers
    println(Whatever.parse(Whatever.accept('h'),"hello"))
    println(Whatever.parse("hi": Whatever.Parser[String],"hi there"))
    println(Whatever.parse("hi": Whatever.Parser[String],"bye there"))
  }
  
}