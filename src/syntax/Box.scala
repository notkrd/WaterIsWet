package syntax

import scala.collection.immutable.Seq
import defs_etc._
import semantics.Model

class Box(val the_vars: Seq[Variable], val the_conds: Seq[Condition]) {
  def Merge: Box => Box = (other_box) => {
    new Box(the_vars ++ other_box.the_vars, the_conds ++ other_box.the_conds)
  }
  
  implicit def singleton(cond: Condition): Box = new Box(cond.varsOf, Seq(cond))
  
  def ++(other_box: Box): Box = Merge(other_box)
  
  def &@(v: Variable): Box =
    new Box(the_vars :+ v, the_conds)
  
  def &=(c: Condition): Box = 
    new Box(the_vars, the_conds :+ c)
    
  def addNamedVar: Variable => KeyPhrase => Box = (new_var) => (the_name) =>
    new Box(the_vars :+ new_var, the_conds :+ var_assignment(new_var, the_name))
    
  def &?(v: Referent): Box = v match {
      case Left(a_var: Variable) => this &@ a_var
      case Right(a_thing: KeyPhrase) => this.addNamedVar(a_thing)(a_thing)
    }
    
}