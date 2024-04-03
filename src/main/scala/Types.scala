type Id = String

sealed abstract class Term(id: Id)
object Term:
  case class Var(id: Id) extends Term(id)
  case class Name(id: Id, msgs: Seq[Term] = Seq()) extends Term(id)
  case class Func(id: Id, msgs: Seq[Term]) extends Term(id)

case class FuncDef(id: Id, arity: Int):
  def apply(params: Term*): Term.Func =
    assert(params.length == arity)
    Term.Func(id, params)

case class Fact(pred: Id, msgs: Seq[Term])

case class Clause(hypos: Seq[Fact], concl: Fact)
