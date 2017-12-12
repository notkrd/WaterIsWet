package semantics
import defs_etc._
import syntax._

/* Simple semantic model:
 * @param entities a partial function from names to individuals
 * @param relations1 predicates in the model, represented as one-place partial functions from entities to truth values
 * @param relations2 binary relations in the model, represented as two place partial functions from entities to truth values
 */
class Model(entities: Map[Phrase, Entity], relations1: Map[Phrase, PredSing], relations2: Map[Phrase, Entity => Entity => Boolean]) {
  val entities_set: Set[Entity] = this.entities.values.toSet
  val relations1_set: Set[PredSing] = this.relations1.values.toSet
  val relations2_set: Set[PredBin] = this.relations2.values.toSet
  val lexicon_set: Set[Phrase] = entities.keySet ++ relations1.keySet ++ relations2.keySet
  
  val SemR1: Phrase => Phrase => Boolean = (vi) => (n) => relations1(vi)(entities(n))
  val SemR2: Phrase => Phrase => Phrase => Boolean = (vt) => (n1) => (n2) => relations2(vt)(entities(n1))(entities(n2))
  val SemAnd: Phrase => Phrase => Boolean = (ph1) => (ph2) => false
  
  val ValidAssignment: Embedding => Boolean = (the_func) => the_func.values.toSet subsetOf this.entities_set
  
  val IsConditionEmbedding: Assignment => Condition => Boolean = (an_embedding) => (a_cond) => a_cond match {
    case truth_value(a_polarity) => a_polarity
    case var_assignment(the_var, the_val) => an_embedding(the_var) == entities(the_val)
    case pred_sing(the_pred, the_var) => relations1(the_pred)(an_embedding(the_var))
    case pred_bin(the_pred, left_var, right_var) => relations2(the_pred)(an_embedding(left_var))(an_embedding(right_var))
    case var_equality(left_var, right_var) => an_embedding(left_var) == an_embedding(right_var)
    case not_box(the_box) => !IsBoxEmbedding(an_embedding)(the_box)
    case sub_box(left_box, right_box) => !(IsBoxEmbedding(an_embedding)(left_box) && !IsBoxEmbedding(an_embedding)(right_box))
    case or_box(left_box, right_box) => IsBoxEmbedding(an_embedding)(left_box) || IsBoxEmbedding(an_embedding)(right_box)
    case _ => true
  }
  
  val IsBoxEmbedding: Embedding => Box => Boolean = (an_embedding) => (the_box) => {
    the_box.the_conds.forall(IsConditionEmbedding(an_embedding))
  }
  
  val BoxSatisfiable: Box => Boolean = (_) => true
  
  val AllEmbeddingsOnVars: Seq[Variable] => Seq[Embedding] = (the_vars) => {
    if(the_vars.isEmpty) {
      Seq()
    }
    else if(the_vars.size == 1) {
      entities.values.map((e) => Map(the_vars.head -> e)).toSeq
    }
    else {
      for {
        an_embedding <- AllEmbeddingsOnVars(the_vars.tail)
        an_entity <- entities.values
      } yield an_embedding + (the_vars.head -> an_entity)
    }
  }
  
  val Embeddings: Box => Seq[Embedding] = (a_box) => 
    AllEmbeddingsOnVars(a_box.the_vars.toSeq) filter ((e) => ValidAssignment(e) && IsBoxEmbedding(e)(a_box))

}