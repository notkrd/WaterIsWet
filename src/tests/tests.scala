package tests
import defs_etc._
import semantics._

object tests {
  def main(args: Array[String]) = {
    model_tests()
  }
  
  def model_tests() = {
    val textbook_entities = Map("Anwar" -> "AS", "Mohammed" -> "MA", "Noam" -> "NC", "John" -> "JW")
    val is_bald = Map("AS" -> false, "MA" -> true, "NC" -> false, "JW" -> true)
    
    val does_love: Entity => Entity => Boolean = (e1: Entity) => (e2: Entity) => List(e1,e2) match {
      case List(e1, e2)  if e1 == e2 => true
      case _ => false
    }
    val textbook_r1 = Map("bald" -> is_bald)
    val textbook_r2 = Map("loves" -> does_love)
    lazy val textbook_model = new Model(textbook_entities, textbook_r1, textbook_r2)
    
    println(is_bald("JW") == true)
    println(does_love("NC")("NC") == true)
    println(does_love("MA")("JW") == false)
    println(textbook_model.SemR1("bald")("John") == true)
    println(textbook_model.SemR2("loves")("Noam")("Noam") == true)
    println(textbook_model.SemR2("loves")("Mohammed")("John") == false)
  }
}