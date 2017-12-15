package syntax
import defs_etc._

/* A grammatical rule
 * 
 * @param cat_in: left category. "X" is reserved for general templates
 * @param cats_out: right categories. "X" is reserved for general templates
 */
class TransformRule(val cat_in: SyntaxCat, val cats_out: Seq[SyntaxCat]){
  override def toString = "[" + cat_in.toString() + "-> " + cats_out.mkString(sep=", ") + "]"
}