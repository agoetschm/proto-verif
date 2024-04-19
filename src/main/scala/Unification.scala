import Term._
import scala.annotation.tailrec

object Unification:
  def unify(t1: Term, t2: Term): Option[Substitution] =
    unify0(List((t1, t2)), Substitution.empty)

  // https://en.wikipedia.org/wiki/Unification_(computer_science)
  // Martelli & Montanari algo
  @tailrec def unify0(
      ts: List[(Term, Term)],
      s: Substitution
  ): Option[Substitution] =
    // println(s"unifier0($ts, $s)")
    ts match
      case Nil => Some(s)
      case (t1, t2) :: ts =>
        (t1, t2) match
          // delete
          case (_, _) if t1 == t2 => unify0(ts, s)
          // swap
          case (_: Name, _: Var) | (_: Func, _: Var) =>
            unify0((t2, t1) :: ts, s)
          // eliminate
          case (v: Var, _) =>
            if t2.vars contains v then None // no recursive term
            else
              s.extend(v, t2) match
                case Left(_) => None
                case Right(extendedS) =>
                  val ss = Substitution.single(v, t2)
                  unify0(ts.map((t1, t2) => (ss(t1), ss(t2))), extendedS)
          // decompose
          case (Name(ndef1, args1), Name(ndef2, args2)) if ndef1 == ndef2 =>
            unify0(args1.zip(args2) ::: ts, s)
          case (Name(_, _), _) => None
          case (Func(fdef1, args1), Func(fdef2, args2)) if fdef1 == fdef2 =>
            unify0(args1.zip(args2) ::: ts, s)
          case (Func(_, _), _) => None
