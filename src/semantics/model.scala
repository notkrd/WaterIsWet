package semantics
import defs_etc._
import syntax._

/* Minimal semantic model:
 * 
 * @param entities a partial function from names to individuals
 * @param relations1 a partial function from words to predicates in the model, where a predicate is a from entitie to truth values
 * @param relations2 a partial function from words to binary relations in the model
 * 
 * The reason for constructing the model with partial function from names rather than simply the objects themselves, is to
 * allow accessing parts of the model through strings: the Model can contain the data of the strctured representations of 
 * predicates, while the semantics only knows how to "send a word into the portal"
 */
class Model(entities: Map[Phrase, Entity], relations1: Map[Phrase, PredSing], relations2: Map[Phrase, Entity => Entity => Boolean]) {
  /* The entities in the model */
  val entities_set: Set[Entity] = this.entities.values.toSet
  /* The one-place relations in the model */
  val relations1_set: Set[PredSing] = this.relations1.values.toSet
  /* The binary relations in the model */
  val relations2_set: Set[PredBin] = this.relations2.values.toSet
  /* All words in the model */
  val lexicon_set: Set[Phrase] = entities.keySet ++ relations1.keySet ++ relations2.keySet
  
  /* Evaluates a predicate, accessed by name */
  val SemR1: Phrase => Phrase => Boolean = (vi) => (n) => relations1(vi)(entities(n))
  /* Evaluates a binary relation, accessed by name */
  val SemR2: Phrase => Phrase => Phrase => Boolean = (vt) => (n1) => (n2) => relations2(vt)(entities(n1))(entities(n2))
  /* Evaluates an "and" expression. Doesn't work */
  val SemAnd: Phrase => Phrase => Boolean = (ph1) => (ph2) => false
  
  /* Ensures an embedding won't lead to key errors */
  val ValidAssignment: Embedding => Boolean = (the_func) => the_func.values.toSet subsetOf this.entities_set
  
  /* Tests whether an embedding satisfies a given condition */
  val IsConditionEmbedding: Embedding => Condition => Boolean = (an_embedding) => (a_cond) => a_cond match {
    case truth_value(a_polarity) => a_polarity
    case var_assignment(the_var, the_val) => an_embedding(the_var) == the_val
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
  
  lazy val BoxSatisfiable: Box => Boolean = (a_box) => !(Embeddings(a_box).isEmpty)
  
  lazy val AllEmbeddingsOnVars: Seq[Variable] => Seq[Embedding] = (the_vars) => {
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
  
  /* A sequence of possible assignments for the_var at a_box. Contains all valid assignments, though possibly other junk
   *  
   *  This is the one method that is not yet functional, simply out of laziness.
   */
  lazy val PossibleAssignments: Box => Variable => Embedding => Seq[Entity] = (the_box) => (the_var) => (prev_asig) => {
    var assignment_options: Seq[Entity]  = Seq()
    the_box.the_conds.foreach((c) => c match {
      case var_assignment(a_var, x) if a_var == the_var => assignment_options = Seq(x)
      case var_equality(a_var, other_var) if a_var == the_var && prev_asig.isDefinedAt(other_var) => assignment_options = Seq(prev_asig(other_var))
      case var_equality(other_var, a_var) if a_var == the_var && prev_asig.isDefinedAt(other_var) => assignment_options = Seq(prev_asig(other_var))
      case pred_sing(the_pred, a_var) if a_var == the_var && assignment_options.size != 1 => assignment_options = entities_set.filter(relations1(the_pred)).toSeq
      case _ => ()
    })
    if(assignment_options.isEmpty) {
      assignment_options = entities_set.toSeq
    }
    assignment_options
  }
  
   /* A sequence of possible embeddings for the_vars at a_box. Contains all valid embeddings, though possibly other junk */
  lazy val PlausibleEmbeddingsOnVars: Box => Seq[Variable] => Set[Embedding] = (a_box) => (the_vars) => {
    if(the_vars.isEmpty) {
      Set()
    }
    else if(the_vars.size == 1) {
      PossibleAssignments(a_box)(the_vars.head)(Map()).map((v) => Map(the_vars.head -> v)).toSet
    }
    else {
      for {
        an_embedding <- PlausibleEmbeddingsOnVars(a_box)(the_vars.tail)
        an_entity <- PossibleAssignments(a_box)(the_vars.head)(an_embedding)
      } yield an_embedding + (the_vars.head -> an_entity)
    }
  }

  /* All possible embeddings for a DRT into this model */
  lazy val Embeddings: Box => Set[Embedding] = (a_box) => 
    PlausibleEmbeddingsOnVars(a_box)(a_box.the_vars.toSeq) filter ((e) => ValidAssignment(e) && IsBoxEmbedding(e)(a_box))
  
  /* A valid embedding of a DRT on the model, with no other guarantees about it */
  lazy val an_embedding: Box => Embedding = (a_box) => Embeddings(a_box).head
}