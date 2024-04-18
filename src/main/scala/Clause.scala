import Term._
import Substitution.getSubstitution

// to simplify the algorithm, we assume here that a fact only contains one term
case class Fact private (factDef: Fact.Def, msg: Term):
  override def toString(): String = s"${factDef.pred}(${msg})"

object Fact:
  case class Def(pred: Id):
    def apply(m: Term): Fact = Fact(this, m)

case class Clause(hypos: Set[Fact], concl: Fact):
  override def toString(): String =
    s"{${hypos.map(h => s"\n  $h").mkString(",")}\n} => $concl)"
  def subsumes(that: Clause): Boolean =
    val s = getSubstitution(this.concl.msg, that.concl.msg)
    s match
      case None    => false
      case Some(s) => this.hypos.map(s(_)).subsetOf(that.hypos)
