import Term._

case class Substitution(subs: Map[Var, Term]):
  def apply(t: Term): Term =
    t match
      case v: Var  => if (subs.contains(v)) subs(v) else v
      case n: Name => n.ndef(n.args.map(apply)*)
      case f: Func => f.fdef(f.args.map(apply)*)

  def apply(f: Fact): Fact = f.factDef(apply(f.msg))

  def apply(r: Clause): Clause = r.withSubstitution(this)

  def merge(that: Substitution): Either[Error, Substitution] =
    // val onlyThis = this.subs.keySet.diff(that.subs.keySet)
    val thisAndThat = this.subs.keySet.intersect(that.subs.keySet)
    // val onlyThat = that.subs.keySet.diff(this.subs.keySet)

    if !thisAndThat.forall(v => this.subs(v) == that.subs(v)) then
      Left(Error("two different substitutions for same variable"))
    else Right(Substitution(this.subs ++ that.subs))

  def extend(v: Var, t: Term): Either[Error, Substitution] =
    if subs.keySet.contains(v) then
      Left(
        Error("attempt to extend a substitution with a var already in its keys")
      )
    else
      val s2 = Substitution.single(v, t)
      Substitution(subs.mapValues(s2(_)).toMap).merge(s2)

object Substitution:
  val empty = Substitution(Map())
  def single(v: Term.Var, t: Term): Substitution = Substitution(Map((v, t)))

  /** Computes the substitution from one term to another, i.e. the mapping of
    * variables that transforms a term into another. This simple procedure works
    * only because the substitution is done on one side of the equation. The
    * more generic procedure is unification, see [[Unification]].
    *
    * @return
    *   a substitution s such that s(t1) = t2
    */
  def getSubstitution(t1: Term, t2: Term): Option[Substitution] =
    (t1, t2) match
      case (v: Var, t) => Some(Substitution.single(v, t))
      case (_, _: Var) => None
      case (Name(ndef1, args1), Name(ndef2, args2)) =>
        if ndef1 == ndef2 then
          args1
            .zip(args2) match
            case Nil => Some(Substitution.empty)
            case args =>
              args
                .map(getSubstitution)
                .reduce:
                  case (Some(s1), Some(s2)) => s1.merge(s2).toOption
                  case _                    => None
        else None
      case (Func(fdef1, args1), Func(fdef2, args2)) =>
        if fdef1 == fdef2 then
          args1
            .zip(args2) match
            case Nil => Some(Substitution.empty)
            case args =>
              args
                .map(getSubstitution)
                .reduce:
                  case (Some(s1), Some(s2)) => s1.merge(s2).toOption
                  case _                    => None
        else None
      case _ => None
