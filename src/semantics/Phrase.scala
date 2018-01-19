package semantics

import scala.collection.immutable.Seq
import syntax._
import defs_etc._

/* A phrase with its meaning in a combinatory categorial grammar. Syntax is given by meaning type */
class Phrase[+A](val txt: String, val meaning: A, val dir: String = "none") {
  
  type Sentence = Phrase[Box => Box]
  type Term = Phrase[Referent]
  
  override def toString: String = "Phrase: \"" + txt + "\" Meaning: " + this.meaning
  
  val isSimple: Boolean = true
  
  /* Apply into argument */
  def @<@[B](that: LSlash[B, A]): B = that(this.meaning)
  
  def apply[B](that: LSlash[B, A]) = this @<@ that
  
  def +>[T]: RSlash[T, LSlash[T,A]] =
    new RSlash(txt, (that: LSlash[T,A]) => this @<@ that)
  
  def +<[T]: LSlash[T, RSlash[T,Phrase[A]]] = 
    new LSlash(txt, (that: RSlash[T, Phrase[A]]) => that @>@ this)
}
class RSlash[+A, -B](txt: String, meaning: B => A, dir: String = "right") extends Phrase[B => A](txt, meaning, dir) {
  val cat = "/"
  override val isSimple = false
  
  def apply(that: B) = this @>@ that
  
  implicit def toFunc: B => A = meaning
  
  def @>>@[C](that: RSlash[B,C]): RSlash[A,C] = 
    new RSlash(this.txt + " " + that.txt, (c: C) => this(that(c)))
  
  def @>@(that: B): A = meaning(that)
}
class LSlash[+A, -B](txt: String, meaning: B => A, dir: String = "left") extends Phrase[B => A](txt, meaning, dir) {
  override val isSimple = false
  val cat = "\\"
  
  implicit def toFunc: B => A = meaning
  
  def apply(that: B) = meaning(that)
  
  def @<<@[C](that: LSlash[C, A]): LSlash[C,B] = 
    new LSlash(this.txt + " " + that.txt, (b: B) => that(this(b)))
}

trait Phrases {
  
  type Sentence = Phrase[Box => Box]
  type Term = Phrase[Referent]
  
  implicit def pullUp(s: String): Term = new Phrase(s, Right(s))
  
  implicit def knockDownR[A,B](r: RSlash[Phrase[A], Phrase[B]]): RSlash[A,B] = {
    new RSlash[A,B](r.txt, (b: B) => r(new Phrase("", b)).meaning)
  }
  
  implicit def pullUpR[A,B](r: RSlash[A, B]): RSlash[Phrase[A], Phrase[B]] = {
    new RSlash[Phrase[A],Phrase[B]](r.txt, (b: Phrase[B]) => new Phrase(r.txt + " " + b.txt, r(b.meaning)))
  }
  
  implicit def knockDownL[A,B](r: LSlash[Phrase[A], Phrase[B]]): LSlash[A,B] = {
    new LSlash[A,B](r.txt, (b: B) => r(new Phrase("", b)).meaning)
  }
  
  implicit def pullUpL[A,B](r: LSlash[A, B]): LSlash[Phrase[A], Phrase[B]] = {
    new LSlash[Phrase[A],Phrase[B]](r.txt, (b: Phrase[B]) => new Phrase(r.txt + " " + b.txt, r(b.meaning)))
  }
  
  /* Makes a noun phrase from a key to an entity */
  def NP(thing: KeyPhrase): Term = new Phrase(thing.toString(), Right(thing))
  /* Makes a sentence from text, and an update function */
  def S(txt: String, update: Box => Box): Sentence = new Phrase(txt, update)
  /* Makes an intransitive verb from a key to a predicate */
  def VI(txt: KeyPhrase): LSlash[Sentence, Term] = {
    val vi_meaning = (thing: Referent) => (b: Box) => (b &= pred_sing(txt, thing)) &? thing
    new LSlash(txt, (thing_phr: Phrase[Referent]) => 
      S(thing_phr.txt + " " + txt, vi_meaning(thing_phr.meaning)))
  }
  /* Makes a transitive verb from a key to a binary relation */
  def VT(txt: KeyPhrase): RSlash[LSlash[Sentence, Term], Term] = {
    val vt_meaning: Referent => Referent => Box => Box = (thing1) => (thing2) => (b) =>
      ((b &= pred_bin(txt, thing2, thing2)) &? thing1) &? thing2
    new RSlash(txt, (thing2_phr: Phrase[Referent]) => 
      new LSlash(txt + thing2_phr.txt, (thing1_phr: Phrase[Referent]) =>
        S(thing1_phr.txt + " " + txt + " " + thing2_phr.txt, vt_meaning(thing1_phr.meaning)(thing2_phr.meaning))))
  }
  
}