package syntax
import defs_etc._
import semantics.Model

class Box(val the_vars: Set[Variable], val the_conds: Set[Condition]) {
  val Merge: Box => Box = (other_box) => {
    new Box(the_vars ++ other_box.the_vars, the_conds ++ other_box.the_conds)
  }
}