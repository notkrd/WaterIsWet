package semantics
import syntax._
import defs_etc._

/* Words contain their meanings, as well as their names.
 * 
 * @param str: the underlying phrase
 * @param cat: the syntactical category of the phrase
 * @param meaning: the function the word encodes.
 * 
 * Note, even though unenforced, the type A must correspond to the type for meanings of the relevant category.
 */

class Words[A](val str: String, val cat: CombinatoryCategory, val meaning: A) {
  def apply_meaning[B](that: Words[A => B]): B = that.meaning(this.meaning)
  def join_strs[B](that: Words[B]) = (that.cat, this.cat) match {
    case (LSlash(cat_in, _), a_cat) if cat_in == a_cat => this.str + " " + that.str
    case (RSlash(cat_in, _), a_cat) if cat_in == a_cat => that.str + " " + this.str
    case _ => this.str + " " + that.str
  }
  def apply[B](that: Words[A => B]): Words[B] = {
    new Words[B](join_strs(that), this.cat + that.cat, apply_meaning(that))
  }
}