package syntax
import defs_etc._

abstract class Condition {
  def varsOf: Seq[Variable] = this match {
    case var_assignment(v, _) => Seq(v)
    case pred_sing(_, v) => Seq(v)
    case pred_bin(_, v1, v2) => Seq(v1,v2)
    case var_equality(v1, v2) => Seq(v1, v2)
    case not_box(b) => b.the_vars
    case sub_box(l, r) => l.the_vars ++ r.the_vars
    case or_box(l, r) => l.the_vars ++ r.the_vars
    case _ => Seq()
  }
}
case class truth_value(polarity: Boolean) extends Condition
case class var_assignment(the_var: Variable, the_val: KeyPhrase) extends Condition
case class pred_sing(the_pred: KeyPhrase, the_var: Variable) extends Condition
case class pred_bin(the_pred: KeyPhrase, left_var: Variable, right_var: Variable) extends Condition
case class var_equality(left_var: Variable, right_var: Variable) extends Condition
case class not_box(the_box: Box) extends Condition
case class sub_box(left_box: Box, right_box: Box) extends Condition
case class or_box(left_box: Box, right_box: Box) extends Condition