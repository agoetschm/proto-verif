import Term._
import Substitution.getSubstitution
import Derivation.resolution

// to simplify the algorithm, we assume here that a fact only contains one term
case class Fact private (factDef: Fact.Def, msg: Term):
  override def toString(): String = s"${factDef.pred}(${msg})"

object Fact:
  case class Def(pred: Id):
    def apply(m: Term): Fact = Fact(this, m)

    /** @param resolutionOf
      *   contains history of resolutions performed resulting in this clause
      */
case class Clause private (
    hypos: Set[Fact],
    concl: Fact,
    resolutionOf: List[Clause]
):
  override def toString(): String =
    s"{${hypos.map(h => s"$h").mkString(",")}} => $concl" +
      (if resolutionOf.nonEmpty then
         s" [resolution of: ${resolutionOf.map(_.toString()).mkString(",")}]"
       else "")

  def subsumes(that: Clause): Boolean =
    val s = getSubstitution(this.concl.msg, that.concl.msg)
    s match
      case None    => false
      case Some(s) => this.hypos.map(s(_)).subsetOf(that.hypos)

  val vars = hypos
    .map(_.msg.vars)
    .reduceOption(_ ++ _)
    .getOrElse(Set()) ++ concl.msg.vars

  def withVarsDifferentFrom(that: Clause): Clause =
    val overlapping = that.vars.intersect(this.vars)
    val allVars = that.vars ++ this.vars
    val renaming =
      for
        v <- overlapping
        maxVersion = allVars.filter(_.id == v.id).maxBy(_.version).version
      // there should be other vars with same id
      yield (v, v.withVersion(maxVersion + 1))
    val subst = Substitution(renaming.toMap)
    subst(this)

  def withoutResolutions = this.copy(resolutionOf = Nil)

object Clause:
  def apply(hypos: Set[Fact], concl: Fact): Clause =
    apply(hypos, concl, resolutionOf = Nil)

  def apply(hypos: Set[Fact], concl: Fact, resolutionOf: List[Clause]): Clause =
    // check that all variables in the conclusion are defined in the hypothesis
    // might be very costly
    require(
      concl.msg.vars.forall(
        hypos
          .map(_.msg.vars)
          .reduceOption(_ ++ _)
          .getOrElse(Set())
          .contains
      )
    )
    new Clause(hypos, concl, resolutionOf)
