
package object defs_etc {
  /* Relevant type synonyms. */
  type Phrase = String
  type Variable = String
  type Entity = String
  type PredSing = Entity => Boolean
  type PredBin = Entity => Entity => Boolean
  type Embedding = Map[Variable, Entity]
  type Assignment = Map[Variable, Phrase]
}