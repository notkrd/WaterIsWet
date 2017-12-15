package syntax
import defs_etc._

object TransformHelper {
  val sub_vars: SyntaxCat => SyntaxCat => SyntaxCat = (sub_cat) => (a_cat) => {
    if(a_cat == "X") {
      sub_cat
    }
    else {
      a_cat
    }
  }
  
  def general_rule(on_cats: Seq[SyntaxCat], rule_template: TransformRule): Seq[TransformRule] = {
    for {
      a_cat <- on_cats
      val updated_out = rule_template.cats_out.map(sub_vars(a_cat))
      val template_applied = new TransformRule(sub_vars(a_cat)(rule_template.cat_in), updated_out)
    } yield template_applied
  }
  
  def divide_utterance(an_utterance: Utterance): Seq[Phrase] = {
    an_utterance.split(" ")
  }
}