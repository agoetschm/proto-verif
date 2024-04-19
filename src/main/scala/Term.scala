import Term._

type Id = String

sealed abstract class Term(id: Id):
  lazy val vars: Set[Var] = this match
    case v: Var        => Set(v)
    case Name(_, args) => args.map(_.vars).foldLeft(Set())(_ ++ _)
    case Func(_, args) => args.map(_.vars).foldLeft(Set())(_ ++ _)
  lazy val closed: Boolean = vars.isEmpty
  lazy val open = !closed

  override def toString(): String = this match
    case v: Var =>
      if v.version == 0 then v.id.toString()
      else s"${v.id.toString()}_${v.version}"
    case Name(ndef, args) =>
      val argsStr = args.map(_.toString()).mkString(",")
      s"${ndef.id}($argsStr)"
    case Func(fdef, args) =>
      val argsStr = args.map(_.toString()).mkString(",")
      s"${fdef.id}($argsStr)"

object Term:
  case class Var(id: Id, version: Int = 0) extends Term(id):
    def withVersion(newVersion: Int) = Var(id, newVersion)
  case class Name private (ndef: Name.Def, args: List[Term])
      extends Term(ndef.id)
  object Name:
    def apply(id: Id): Name = Def(id)()
    case class Def(id: Id, arity: Int = 0):
      def apply(args: Term*): Term.Name =
        require(args.length == arity)
        Name(this, args.toList)
  // a function can only be instanciated via a def
  case class Func private (fdef: Func.Def, args: List[Term])
      extends Term(fdef.id)
  object Func:
    case class Def(id: Id, arity: Int):
      def apply(args: Term*): Term.Func =
        require(args.length == arity, "arity does not match")
        Func(this, args.toList)
