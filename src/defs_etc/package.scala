
/* Relevant type synonyms. */
package object defs_etc {
  type KeyPhrase = String
  type Variable = String
  type Entity = String
  type Utterance = String
  type PredSing = Entity => Boolean
  type PredBin = Entity => Entity => Boolean
  type Embedding = Map[Variable, Entity]
  type Lexicon[A] = Map[A, Set[Utterance]]
}