import Term._

type Id = String

sealed abstract class Term(id: Id):
  lazy val vars: Set[Var] = this match
    case v: Var        => Set(v)
    case Name(_, msgs) => msgs.map(_.vars).foldLeft(Set())(_ ++ _)
    case Func(_, msgs) => msgs.map(_.vars).foldLeft(Set())(_ ++ _)
  lazy val closed: Boolean = vars.isEmpty
  lazy val open = !closed

  override def toString(): String = this match
    case v: Var =>
      if v.version == 0 then v.id.toString()
      else s"${v.id.toString()}_${v.version}"
    case Name(ndef, msgs) =>
      val argsStr = msgs.map(_.toString()).mkString(",")
      s"${ndef.id}($argsStr)"
    case Func(fdef, msgs) =>
      val argsStr = msgs.map(_.toString()).mkString(",")
      s"${fdef.id}($argsStr)"

object Term:
  case class Var(id: Id, version: Int = 0) extends Term(id):
    def withVersion(newVersion: Int) = Var(id, newVersion)
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
