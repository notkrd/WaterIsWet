package syntax
import defs_etc._
import semantics.Model

class Box(val the_vars: Set[Variable], val the_conds: Set[Condition]) {
  val Merge: Box => Box = (other_box) => {
    new Box(the_vars ++ other_box.the_vars, the_conds ++ other_box.the_conds)
  }
  
  def ++(other_box: Box): Box = Merge(other_box)
  
  val insertVar: Variable => Box = (the_var) =>
    new Box(the_vars + the_var, the_conds)
  
  val addCond: Condition => Box = (the_cond) =>
    new Box(the_vars, the_conds + the_cond)
    
  val addNamedVar: Variable => Phrase => Box = (new_var) => (the_name) =>
    new Box(the_vars + new_var, the_conds + var_assignment(new_var, the_name))
}