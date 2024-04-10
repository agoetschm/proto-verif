import Term._
import scala.annotation.tailrec

object Unification:
  def unifier(t1: Term, t2: Term): Option[Substitution] =
    unifier0(List((t1, t2)), Substitution.empty)

  // https://en.wikipedia.org/wiki/Unification_(computer_science)
  // Martelli & Montanari algo
  @tailrec def unifier0(
      ts: List[(Term, Term)],
      s: Substitution
  ): Option[Substitution] =
    // println(s"unifier0($ts, $s)")
    ts match
      case Nil => Some(s)
      case (t1, t2) :: ts =>
        (t1, t2) match
          // delete
          case (_, _) if t1 == t2 => unifier0(ts, s)
          case (Var(_), Var(_))   => None
          // swap
          case (_, Var(_)) => unifier0((t2, t1) :: ts, s)
          // eliminate
          case (v: Var, _) =>
            if t2.vars contains v then None
            else
              s.extend(v, t2) match
                case Left(_) => None
                case Right(extendedS) =>
                  val ss = Substitution.single(v, t2)
                  unifier0(ts.map((t1, t2) => (ss(t1), ss(t2))), extendedS)
          // decompose
          case (Name(ndef1, msgs1), Name(ndef2, msgs2)) if ndef1 == ndef2 =>
            unifier0(ts.appendedAll(msgs1.zip(msgs2)), s)
          case (Name(_, _), _) => None
          case (Func(fdef1, msgs1), Func(fdef2, msgs2)) if fdef1 == fdef2 =>
            unifier0(ts.appendedAll(msgs1.zip(msgs2)), s)
          case (Func(_, _), _) => None
