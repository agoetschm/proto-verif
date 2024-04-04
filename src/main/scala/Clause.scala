import Term._

// case class Fact(pred: Id, msgs: Seq[Term])

// to simplify the algorithm, we assume here that a fact only contains one term
case class Fact private (fdef: Fact.Def, msg: Term)

object Fact:
  case class Def(pred: Id):
    def apply(m: Term): Fact = Fact(this, m)

// TODO ensure that the conlusion does not contain variables not in hypothesis
case class Clause(hypos: Set[Fact], concl: Fact):
  def doesSubsume(that: Clause): Boolean =
    val s = Term.getSubstitution(this.concl.msg, that.concl.msg)
    s match
      case None    => false
      case Some(s) => this.hypos.map(f => f.fdef(s(f.msg))).subsetOf(that.hypos)
