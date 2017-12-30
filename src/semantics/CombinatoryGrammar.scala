package semantics

import syntax._
import defs_etc._

import scala.util.parsing.combinator._

object CombinatoryGrammar extends RegexParsers{
  
  def lift[T](t: T): Parser[T] = (in: Input) => Success(t, in)
  
  def applyWrapped[A,B](p: Parser[A], q: Parser[A => B]): Parser[B] = (in: Input) => p(in) match {
    case Failure(s, r) => Failure(s, r)
    case Error(s, r) => Error(s, r)
    case Success(a, r) => q(r) match {
      case Failure(s, r2) => Failure(s, r2)
      case Error(s, r2) => Failure(s, r2)
      case Success(f, r2) => Success(f(a), r2)
    }
  }
  
  def applyWrappedL[A,B](p: Parser[A => B], q: Parser[A]): Parser[B] = (in: Input) => p(in) match {
    case Failure(s, r) => Failure(s, r)
    case Error(s, r) => Error(s, r)
    case Success(f, r) => q(r) match {
      case Failure(s, r2) => Failure(s, r2)
      case Error(s, r2) => Failure(s, r2)
      case Success(a, r2) => Success(f(a), r2)
    }
  }
  
  def TryRule[A,B,C](ps: Seq[Words[B => A]], qs: Seq[Words[(B => A) => C]]): Parser[C] = applyWrapped(catParser(ps), catParser(qs))
  
  def TryRuleL[A,B,C](ps: Seq[Words[(B => A) => C]], qs: Seq[Words[B => A]]): Parser[C] = applyWrappedL(catParser(ps), catParser(qs))
  
  def choiceParser[T](ps: Seq[Parser[T]]): Parser[T] = (in: Input) => {
    if(ps.isEmpty) {
      Failure("ran out of choices", in)
    }
    else {
      ps.head(in) match {
        case Success(r,n) => Success(r,n)
        case _ => choiceParser(ps.tail)(in)
      }
    }
  }
  
  implicit def parseWords[A,B](w: Words[B => A]): Parser[B => A] = {
    w.str ^^^ w.meaning
  }
  
  def catParser[A,B](w: Seq[Words[B => A]]): Parser[B => A] = choiceParser(w.toSeq.map(parseWords _))
  
  def @\@[A,B,C](l: Parser[B => A], r: Parser[(B => A) => C]): Parser[C] = applyWrapped(l, r)

  def @/@[A,B,C](l: Parser[(B => A) => C], r: Parser[B => A]): Parser[C] = applyWrappedL(l, r)
}