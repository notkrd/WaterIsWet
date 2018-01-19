package semantics

import scala.collection.immutable.Seq
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
class Model(val entities: Map[KeyPhrase, Entity], val relations1: Map[KeyPhrase, PredSing], val relations2: Map[KeyPhrase, Entity => Entity => Boolean]) {
  /* The entities in the model */
  val entities_set: Set[Entity] = this.entities.values.toSet
  /* The one-place relations in the model */
  val relations1_set: Set[PredSing] = this.relations1.values.toSet
  /* The binary relations in the model */
  val relations2_set: Set[PredBin] = this.relations2.values.toSet
  /* All words in the model */
  val lexicon_set: Set[KeyPhrase] = entities.keySet ++ relations1.keySet ++ relations2.keySet
  
  /* Evaluates a predicate, accessed by name */
  def SemR1 (vi: KeyPhrase)(subj: KeyPhrase) = relations1(vi)(entities(subj))
  /* Evaluates a binary relation, accessed by name */
  def SemR2(vt: KeyPhrase)(obj: KeyPhrase)(subj: KeyPhrase) = relations2(vt)(entities(obj))(entities(subj))
  /* Evaluates an "and" expression. Doesn't work */
  def SemAnd(ph1: KeyPhrase)(ph2: KeyPhrase) = false
  
  /* Ensures an embedding won't lead to key errors */
  def ValidAssignment(the_func: Embedding) = the_func.values.toSet subsetOf this.entities_set
  
  /* Tests whether an embedding satisfies a given condition */
  def IsConditionEmbedding(an_embedding: Embedding)(a_cond: Condition): Boolean = a_cond match {
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
  
  def IsBoxEmbedding(an_embedding: Embedding)(the_box: Box): Boolean = {
    the_box.the_conds.forall(IsConditionEmbedding(an_embedding))
  }
  
  def BoxSatisfiable(a_box: Box): Boolean = !(Embeddings(a_box).isEmpty)
  
  def AllEmbeddingsOnVars(the_vars: Seq[Variable]): Seq[Embedding] = {
    if(the_vars.isEmpty) {
      Seq()
    }
    else if(the_vars.size == 1) {
      entities.values.map((e) => Map(the_vars.head -> e)).to[collection.immutable.Vector]
    }
    else {
      for {
        an_embedding <- AllEmbeddingsOnVars(the_vars.tail)
        an_entity <- entities.values
      } yield an_embedding + (the_vars.head -> an_entity)
    }
  }
  
  /* A sequence of possible assignments for the_var at a_box. Contains all valid assignments, though possibly other junk. 
   */
  def PossibleAssignments(the_box: Box)(the_var: Variable)(prev_asig: Embedding): Seq[Entity] = {
    PossibleAssignmentsHelper(the_box.the_conds.toSeq)(the_var)(prev_asig)(Vector())
  }
  
  def PossibleAssignmentsHelper(the_conds: Seq[Condition])(the_var: Variable)(prev_asig: Embedding)(asig_op: Seq[Entity]): Seq[Entity] = {
    if(the_conds.isEmpty) {
      if(asig_op.isEmpty) {
        entities_set.to[collection.immutable.Vector]
      }
      else {
        asig_op
      }
    }
    else {
      the_conds.head match {
        case var_assignment(a_var, x) if a_var == the_var => Seq(x)
        case var_equality(a_var, other_var) if a_var == the_var && prev_asig.isDefinedAt(other_var) => 
          PossibleAssignmentsHelper(the_conds.tail)(the_var)(prev_asig)(Seq(prev_asig(other_var)))
        case var_equality(other_var, a_var) if a_var == the_var && prev_asig.isDefinedAt(other_var) => 
          PossibleAssignmentsHelper(the_conds.tail)(the_var)(prev_asig)(Seq(prev_asig(other_var)))
        case pred_sing(the_pred, a_var) if a_var == the_var && prev_asig.size != 1 =>  
          PossibleAssignmentsHelper(the_conds.tail)(the_var)(prev_asig)(entities_set.filter(relations1(the_pred)).to[collection.immutable.Vector])
        case _ =>   
          PossibleAssignmentsHelper(the_conds.tail)(the_var)(prev_asig)(asig_op)
      } 
    }
  }
  
   /* A sequence of possible embeddings for the_vars at a_box. Contains all valid embeddings, though possibly other junk. Order matters. */
  def PlausibleEmbeddingsOnVars(a_box: Box)(the_vars: Seq[Variable]): Set[Embedding] = {
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
  def Embeddings(a_box: Box): Set[Embedding] =  
    PlausibleEmbeddingsOnVars(a_box)(a_box.the_vars.to[collection.immutable.Vector]) filter ((e) => ValidAssignment(e) && IsBoxEmbedding(e)(a_box))
  
  /* A valid embedding of a DRT on the model, with no other guarantees about it. Fails if there are no embeddings */
  def an_embedding(a_box: Box): Embedding = Embeddings(a_box).head
}