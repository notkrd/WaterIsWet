package syntax
import defs_etc._

abstract class Condition {
  def ConditionType(): String = this match {
    case truth_value(_) => "truth_value"
    case var_assignment(_,_) => "var_assignment"
    case pred_sing(_,_) => "pred_sing"
    case pred_bin(_,_,_) => "pred_bin"
    case var_equality(_,_) => "var_equality"
    case not_box(_) => "not_box"
    case sub_box(_,_) => "sub_box"
    case or_box(_,_) => "or_box"
  }
}
case class truth_value(polarity: Boolean) extends Condition
case class var_assignment(the_var: Variable, the_val: Phrase) extends Condition
case class pred_sing(the_pred: Phrase, the_var: Variable) extends Condition
case class pred_bin(the_pred: Phrase, left_var: Variable, right_var: Variable) extends Condition
case class var_equality(left_var: Variable, right_var: Variable) extends Condition
case class not_box(the_box: Box) extends Condition
case class sub_box(left_box: Box, right_box: Box) extends Condition
case class or_box(left_box: Box, right_box: Box) extends Condition