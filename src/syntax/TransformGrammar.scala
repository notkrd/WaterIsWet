package syntax
import defs_etc._

class TransformGrammar(cats: Seq[SyntaxCat], rules: Seq[TransformRule]) {
  type SynParser = Parser[Seq[Phrase],ParseTree]
  
  val PhrParser: SyntaxCat => Phrase => SynParser = (cat) => (phr) => (phrases) => {
    if(phrases.isEmpty){
      Right(())
    }
    else if(phrases.head == phr) {
      Left((phrases.tail, PhraseLeaf(phr,cat)))
    }
    else{
      Right(())
    }
  }
  
  def parse(s: Utterance): ParseTree = {
    PhraseLeaf("FAKE", s)
  }
}