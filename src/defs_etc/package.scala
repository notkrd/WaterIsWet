
/* Relevant type synonyms. */
package object defs_etc {
  type Phrase = String
  type Variable = String
  type Entity = String
  type Utterance = String
  /* Note that "X" is off limits! */
  type SyntaxCat = String
  type PredSing = Entity => Boolean
  type PredBin = Entity => Entity => Boolean
  type Embedding = Map[Variable, Entity]
  type Assignment = Map[Variable, Phrase]
  type Parser[U,+T] = U => Either[(U,T), Unit]
}