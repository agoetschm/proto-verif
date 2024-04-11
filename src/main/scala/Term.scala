import Term._

type Id = String

sealed abstract class Term(id: Id):
  lazy val vars: Set[Var] = this match
    case v: Var        => Set(v)
    case Name(_, msgs) => msgs.map(_.vars).foldLeft(Set())(_ ++ _)
    case Func(_, msgs) => msgs.map(_.vars).foldLeft(Set())(_ ++ _)
  lazy val closed: Boolean = vars.isEmpty
  lazy val open = !closed

object Term:
  case class Var(id: Id) extends Term(id)
  case class Name private (ndef: Name.Def, msgs: List[Term])
      extends Term(ndef.id)
  object Name:
    case class Def(id: Id, arity: Int = 0):
      def apply(params: Term*): Term.Name =
        require(params.length == arity)
        Name(this, params.toList)
  // a function can only be instanciated via a def
  case class Func private (fdef: Func.Def, msgs: List[Term])
      extends Term(fdef.id)
  object Func:
    case class Def(id: Id, arity: Int):
      def apply(params: Term*): Term.Func =
        require(params.length == arity, "arity does not match")
        Func(this, params.toList)
