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
  
  val IsConditionEmbedding: Embedding => Condition => Boolean = (an_embedding) => (a_cond) => a_cond match {
    case truth_value(a_polarity) => a_polarity
    case var_assignment(the_var, the_val) => an_embedding(the_var) == entities(the_val)
    case pred_sing(the_pred, the_var) => relations1(the_pred)(an_embedding(the_var))
    case pred_bin(the_pred, left_var, right_var) => relations2(the_pred)(entities(left_var))(entities(right_var))
    case var_equality(left_var, right_var) => an_embedding(left_var) == an_embedding(right_var)
    case not_box(the_box) => !IsBoxEmbedding(an_embedding)(the_box)
    case sub_box(left_box, right_box) => !(IsBoxEmbedding(an_embedding)(left_box) && !IsBoxEmbedding(an_embedding)(right_box))
    case or_box(left_box, right_box) => IsBoxEmbedding(an_embedding)(left_box) || IsBoxEmbedding(an_embedding)(right_box)
    case _ => true
  }
  
  val IsBoxEmbedding: Embedding => Box => Boolean = (an_embedding) => (the_box) => the_box match {
    case ConditionsBox(_, the_conds) => the_conds forall IsConditionEmbedding(an_embedding)
    case Merge(left_box, right_box) => IsBoxEmbedding(an_embedding)(left_box) && IsBoxEmbedding(an_embedding)(right_box)
  }
  
  val BoxSatisfiable: Box => Boolean = (_) => true
  
  val EmbeddingCandidates: Box => Set[Embedding] = (a_box) => {
    val ordered_vars = a_box.the_vars.toList
    var var_candidates = scala.collection.mutable.Map[Variable, Either[Set[Entity],Variable]]()
    a_box.the_conds.foreach((c) => c match {
      case var_assignment(the_var, the_val) => var_candidates(the_var) = Left(Set[Entity](the_val))
      case pred_sing(the_pred, the_var) => if(!var_candidates.isDefinedAt(the_var)) {
        var_candidates(the_var) = Left(entities_set.filter(relations1(the_pred)))
      }
      case var_equality(left_var, right_var) => if(var_candidates.isDefinedAt(left_var)) {
        var_candidates(right_var) = Right(left_var)
      }
      else if(var_candidates.isDefinedAt(right_var)) {
        var_candidates(left_var) = Right(right_var)
      }
      else {
        var_candidates(right_var) = Right(left_var)
      }
      
    })
    ordered_vars.foreach {(_) => 
      println("no")
    }
    Set[Embedding]()
  }
}