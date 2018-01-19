package semantics

import scala.collection.immutable.Seq
import syntax._
import defs_etc._

import scala.util.parsing.combinator._

object CombinatoryGrammar extends RegexParsers with Phrases{
  
  class SugaredParser[+T](p: Parser[T]){
    def @>@[B](q: Parser[T => B]): Parser[B] = applyWrappedL[T,B](p, q)
  }
    
  implicit def sugarMe[T](p: Parser[T]): SugaredParser[T] = new SugaredParser(p)
  
  implicit def lowerPhraseParser[A](p: Parser[Phrase[A]]): Parser[A] = (in: Input) => p(in) match {
    case  Success(phr, r) => Success(phr.meaning, r)
    case Error(s, r) => Error(s, r)
    case Failure(s, r) => Failure(s, r)
  }
  
  def lift[T](t: T): Parser[T] = (in: Input) => Success(t, in)
  
  def applyWrapped[A,B](p: Parser[A => B], q: Parser[A]): Parser[B] = (in: Input) => p(in) match {
    case Failure(s, r) => Failure(s, r)
    case Error(s, r) => Error(s, r)
    case Success(f, r) => q(r) match {
      case Failure(s, r2) => Failure(s, r2)
      case Error(s, r2) => Failure(s, r2)
      case Success(a, r2) => Success(f(a), r2)
    }
  }
  
  def applyWrappedL[A,B](p: Parser[A], q: Parser[A => B]): Parser[B] = (in: Input) => p(in) match {
    case Failure(s, r) => Failure(s, r)
    case Error(s, r) => Error(s, r)
    case Success(a, r) => q(r) match {
      case Failure(s, r2) => Failure(s, r2)
      case Error(s, r2) => Failure(s, r2)
      case Success(f, r2) => Success(f(a), r2)
    }
  }
  
  def choiceParser[T](ps: Seq[Parser[T]]): Parser[T] = {
    if(ps.isEmpty) {
      (in: Input) => Failure("ran out of choices", in)
    }
    else {
      ps.head | choiceParser(ps.tail)
    }
  }
  
  def sepParser: Parser[Char] = accept(' ') | accept(',') | accept('!') | accept('\n') | accept('?') | accept('-')
  
  implicit def parseMeaning[A, P <: Phrase[A]](p: P): Parser[A] = {
    opt(sepParser.*) ~> (p.txt ^^^ p.meaning)
  }
  
  implicit def parsePhrase[A, P <: Phrase[A]](p: P): Parser[P] = {
    opt(sepParser.*) ~> (p.txt ^^^ p)
  }
  
  
  def catMeaning[A, P <: Phrase[A]](ps: Seq[P]): Parser[A] = choiceParser(ps.map(parseMeaning[A, P] _))
  
  def catParser[A, P <: Phrase[A]](ps: Seq[P]): Parser[P] = choiceParser(ps.map(parsePhrase[A, P] _))
  
  def ruleParserR[A, B, Q <: Phrase[B], P <: Phrase[Q => A]](ps: Seq[P], qs: Seq[Q]): Parser[A] = 
    applyWrapped(catMeaning[Q => A, P](ps), catParser[B,Q](qs))
    
  def ruleParserL[A,B,P <: Phrase[B], Q <: Phrase[P => A]](ps: Seq[P], qs: Seq[Q]): Parser[A] = 
    applyWrappedL(catParser[B, P](ps), catMeaning[P => A, Q](qs))
    
  def rewriteParserR[A,B,P <: Phrase[B], Q <: Phrase[P => A]](ab_parser: Parser[Q], b_parser: Parser[P]): Parser[A] = 
    applyWrapped(ab_parser, b_parser)
    
  def rewriteParserL[A,B,P <: Phrase[B], Q <: Phrase[P => A]](b_parser: Parser[P], ab_parser: Parser[Q]): Parser[A] =
    applyWrappedL(b_parser, ab_parser)
    
  def NP_parser(nps: Seq[KeyPhrase]): Parser[Term] = 
    catParser[Referent, Term](nps.map(NP _))
    
  def VPtoVT_NP(vts: Seq[KeyPhrase], nps: Seq[KeyPhrase]): Parser[LSlash[Sentence, Term]] = {
      ruleParserR[LSlash[Sentence, Term], Referent, Term, RSlash[LSlash[Sentence, Term], Term]](vts.map(VT _), nps.map((an_n: KeyPhrase) => new Phrase[Referent](an_n, Right(an_n))))
  }
    
  def VP_VIparser(vis: Seq[KeyPhrase]): Parser[LSlash[Sentence, Term]] =
    catParser[Term => Sentence, LSlash[Sentence, Term]](vis.map(VI _))
    
  def VP_parser(vis: Seq[KeyPhrase], vts: Seq[KeyPhrase], nps: Seq[KeyPhrase]): Parser[LSlash[Sentence, Term]] =
    VP_VIparser(vis) | VPtoVT_NP(vts, nps)
    
  def StoNP_VP(nps: Parser[Term], vps: Parser[LSlash[Sentence, Term]]): Parser[Sentence] =
    rewriteParserL[Sentence, Referent, Term, LSlash[Sentence, Term]](nps, vps)
    
  def EngSParser(m: Model): Parser[Sentence] = StoNP_VP(NP_parser(m.entities.keys.toVector), VP_parser(m.relations1.keys.toVector, m.relations2.keys.toVector, m.entities.keys.toVector))
    
  def unwrapSent(s: ParseResult[Sentence]): Box = s match {
    case Error(e, _) => new Box(Seq(), Seq())
    case Failure(e, _) => new Box(Seq(), Seq())
    case Success(f, _) => f.meaning(new Box(Seq(), Seq()))
  }
}