import Term._

case class Substitution(subs: Map[Var, Term]):
  def apply(t: Term): Term =
    t match
      case v: Var  => if (subs.contains(v)) subs(v) else v
      case n: Name => n.ndef(n.msgs.map(this(_))*)
      case f: Func => f.fdef(f.msgs.map(this(_))*)

  def apply(f: Fact): Fact = f.factDef(this(f.msg))

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

  /** @param t1
    * @param t2
    * @return
    *   a substitution s such that s(t1) = t2
    */
  def getSubstitution(t1: Term, t2: Term): Option[Substitution] =
    // println(s"getSubstitution($t1, $t2)")
    (t1, t2) match
      case (v @ Var(_), t) => Some(Substitution.single(v, t))
      case (_, Var(_))     => None
      case (Name(ndef1, msgs1), Name(ndef2, msgs2)) =>
        if ndef1 == ndef2 then
          msgs1
            .zip(msgs2) match
            case Nil => Some(Substitution.empty)
            case args =>
              args
                .map(getSubstitution)
                .reduce:
                  case (Some(s1), Some(s2)) => s1.merge(s2).toOption
                  case _                    => None
        else None
      case (Func(fdef1, msgs1), Func(fdef2, msgs2)) =>
        if fdef1 == fdef2 then
          msgs1
            .zip(msgs2) match
            case Nil => Some(Substitution.empty)
            case args =>
              args
                .map(getSubstitution)
                .reduce:
                  case (Some(s1), Some(s2)) => s1.merge(s2).toOption
                  case _                    => None
        else None
      case _ => None
