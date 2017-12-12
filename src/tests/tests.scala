package tests
import defs_etc._
import semantics._
import syntax._

object tests {
  def main(args: Array[String]) = {
    model_tests()
  }
  
  val textbook_entities = Map("Anwar" -> "AS", "Mohammed" -> "MA", "Noam" -> "NC", "John" -> "JW")
  val is_bald = Map("AS" -> false, "MA" -> true, "NC" -> false, "JW" -> true)
  
  val does_love: Entity => Entity => Boolean = (e1: Entity) => (e2: Entity) => Seq(e1,e2) match {
    case Seq(e1, e2)  if e1 == e2 => true
    case _ => false
  }
  val textbook_r1 = Map("bald" -> is_bald)
  val textbook_r2 = Map("loves" -> does_love)
  lazy val textbook_model = new Model(textbook_entities, textbook_r1, textbook_r2)
  
  def model_tests() = {
    
    println(is_bald("JW") == true)
    println(does_love("NC")("NC") == true)
    println(does_love("MA")("JW") == false)
    println(textbook_model.SemR1("bald")("John") == true)
    println(textbook_model.SemR2("loves")("Noam")("Noam") == true)
    println(textbook_model.SemR2("loves")("Mohammed")("John") == false)
  }
  
  val SampleBox: Box = (new Box(
      Set("X"),
      Set(/*var_assignment("X","John"),*/
          pred_sing("bald","X")))).Merge(new Box(
              Set("Y"),
              Set(pred_bin("loves","X","Y"))))
}