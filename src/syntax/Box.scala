package syntax
import defs_etc._
import semantics.Model

abstract class Box {
  val the_vars: Set[Variable]
  val the_conds: Set[Condition]
}
case class ConditionsBox(the_vars: Set[Variable], the_conds: Set[Condition]) extends Box
case class Merge(left_box: Box, right_box: Box) extends Box{
  val the_vars = left_box.the_vars ++ right_box.the_vars
  val the_conds = left_box.the_conds ++ right_box.the_conds
}