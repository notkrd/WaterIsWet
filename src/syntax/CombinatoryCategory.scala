package syntax

/* Types in a combinatory syntax.
 * 
 * The only operation in this syntax is a MERGE
 */
abstract class CombinatoryCategory{
  def label: String
  def compatible_cats(c: CombinatoryCategory) = (this, c) match {
    case (a_cat, LSlash(cat_in, _)) if cat_in == a_cat => true
    case (RSlash(cat_in, _), a_cat) if cat_in == a_cat => true
    case _ => false
  }
  def +(c: CombinatoryCategory): CombinatoryCategory = (this, c) match {
    case (a_cat, LSlash(cat_in, cat_out)) if cat_in == a_cat => cat_out
    case (RSlash(cat_in, cat_out), a_cat) if cat_in == a_cat => cat_out
    case _ => Gibberish()
  }
    
  def @/@(that: CombinatoryCategory): CombinatoryCategory = RSlash(this, that)
    
  def @\@(that: CombinatoryCategory): CombinatoryCategory = LSlash(this, that)
}
/* A Term() is a Noun Phrase */
case class Term() extends CombinatoryCategory{
  def label = "NP"
}
case class Sentence() extends CombinatoryCategory{
  def label = "S"
}
/* This is really bad error handling, but currently, rather than raising errors, syntax should accomodate gibberish*/
case class Gibberish() extends CombinatoryCategory {
  def label = "Gibberish"
}
case class RSlash(cat_in: CombinatoryCategory, cat_out: CombinatoryCategory) extends CombinatoryCategory {
  def label = "(" + cat_out.label + "/" + cat_in.label + ")" 
}
case class LSlash(cat_in: CombinatoryCategory, cat_out: CombinatoryCategory) extends CombinatoryCategory {
  def label = "(" + cat_out.label + "\\" + cat_in.label + ")"
}