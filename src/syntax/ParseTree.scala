package syntax
import defs_etc._

/* A recursively defined structure for parse trees
 * 
 * @param the_phrase: the string parsed in the tree
 * @param SyntaxCat: category of the whole parse. Note that "X" is off limits!
 * 
 */
abstract class ParseTree {
  val the_phrase: Phrase
  val the_cat: SyntaxCat
}
case class PhraseLeaf(the_phrase: Phrase, the_cat: SyntaxCat) extends ParseTree
case class PhraseNode(sub_trees: Seq[ParseTree], the_cat: SyntaxCat) extends ParseTree {
  val the_phrase = sub_trees.foldLeft("") { (s, t) => s + t.the_phrase }
  val rule_for = new TransformRule(the_cat, sub_trees map ((t) => t.the_cat))
}