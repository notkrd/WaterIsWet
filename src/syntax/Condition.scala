package syntax
import defs_etc._

abstract class Condition {
}
case class truth_value(polarity: Boolean) extends Condition
case class var_assignment(the_var: Variable, the_val: Phrase) extends Condition
case class pred_sing(the_pred: Phrase, the_var: Variable) extends Condition
case class pred_bin(the_pred: Phrase, left_var: Variable, right_var: Variable) extends Condition
case class var_equality(left_var: Variable, right_var: Variable) extends Condition
case class not_box(the_box: Box) extends Condition
case class sub_box(left_box: Box, right_box: Box) extends Condition
case class or_box(left_box: Box, right_box: Box) extends Condition