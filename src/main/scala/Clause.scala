import Term._
import Substitution.getSubstitution
import Derivation.resolution
import scala.annotation.tailrec

// to simplify the algorithm, we assume here that a fact only contains one term
case class Fact private (factDef: Fact.Def, msg: Term):
  override def toString(): String = s"${factDef.pred}(${msg})"

object Fact:
  case class Def(pred: Id):
    def apply(m: Term): Fact = Fact(this, m)

/** @param resolutionOf
  *   contains the history of resolutions performed resulting in this clause
  */
case class Clause private (
    hypos: Set[Fact],
    concl: Fact,
    resolutionOf: Option[(Clause, Clause, Substitution)],
    label: Option[String]
):
  def display(
      path: List[Boolean] = Nil
  ): String =
    @tailrec def prefix(acc: String = "", p: List[Boolean]): String = p match
      case Nil          => s"$acc"
      case false :: Nil => s"$acc┌"
      case true :: Nil  => s"$acc├" // ├ └
      case false :: tl  => prefix(s"$acc ", tl)
      case true :: tl   => prefix(s"$acc│", tl)
    (resolutionOf match
      case Some((left, right, s)) =>
        s"""${left.display(path :+ false)}
           #${right.display(path :+ true)}
           #${prefix(p = path)}""".stripMargin('#')
      case None => s"${prefix(p = path)}"
    ) +
      s"{${hypos.map(h => s"${h}").mkString(",")}} => $concl" +
      label.map(l => s" [$l]").getOrElse("")

  override def toString(): String = display()

  /** cf Def 1 in Blanchet2011
    */
  def subsumes(that: Clause): Boolean =
    val s = getSubstitution(this.concl.msg, that.concl.msg)
    s match
      case None    => false
      case Some(s) => this.hypos.map(s(_)).subsetOf(that.hypos)

  val vars = hypos
    .map(_.msg.vars)
    .reduceOption(_ ++ _)
    .getOrElse(Set()) ++ concl.msg.vars

  /** renames the variables occuring in both clauses
    */
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

  def withoutResolution = this.copy(resolutionOf = None)

  def withSubstitution(s: Substitution): Clause =
    this.copy(hypos = hypos.map(s(_)), concl = s(concl))

object Clause:

  def apply(
      hypos: Set[Fact],
      concl: Fact,
      resolutionOf: Option[(Clause, Clause, Substitution)] = None,
      label: Option[String] = None
  ): Clause =
    // check that all variables in the conclusion are defined in the hypothesis
    // TODO might be very costly
    require(
      concl.msg.vars.forall(
        hypos
          .map(_.msg.vars)
          .reduceOption(_ ++ _)
          .getOrElse(Set())
          .contains
      )
    )
    new Clause(hypos, concl, resolutionOf, label)
