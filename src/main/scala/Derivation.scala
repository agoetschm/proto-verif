import Unification.unifier
import scala.annotation.tailrec
import Term._

object Derivation:

  def resolution(r1: Clause, r2: Clause): Option[Clause] =
    val concl1 = r1.concl.msg
    @tailrec def checkHypo(hyposLeft: Iterator[Fact]): Option[Clause] =
      hyposLeft.nextOption() match
        case None       => None
        case Some(hypo) =>
          // println(s"checkHypo($hypo)")
          unifier(hypo.msg, concl1) match
            case None        => checkHypo(hyposLeft)
            case Some(subst) =>
              // println(s"  unifier: $subst")
              val newHypos = (r1.hypos ++ (r2.hypos - hypo)).map(subst(_))
              val newConcl = subst(r2.concl)
              Some(Clause(newHypos, newConcl))
    checkHypo(r2.hypos.toIterator)

  // sel() function in paper
  def selectHypo(r: Clause): Option[Fact] =
    r.hypos.collectFirst: // first hypothesis which is not a variable
      case f @ Fact(_, Name(_, _)) => f
      case f @ Fact(_, Func(_, _)) => f

  def saturate(rs: Set[Clause]): Set[Clause] =
    // add a clause and remove subsumed ones
    def add(r: Clause, rs: Set[Clause]): Set[Clause] =
      if rs.exists(_.doesSubsume(r)) then rs
      else rs.filter(!r.doesSubsume(_)) + r

    val withoutSubsumed = rs.foldLeft(Set())((acc, r) => add(r, acc))

    // combine clauses to get as far as possible from variables only
    @tailrec def saturate0(
        rsSel: Set[Clause],
        rsFree: Set[Clause]
    ): Set[Clause] =
      // iterate over the clauses onces
      // explosion of complexity, but well...
      val rsNew = for
        rSel <- rsSel
        rFree <- rsFree
      yield resolution(rFree, rSel)
      val (rsSelNew, rsFreeNew) =
        rsNew.flatten.partition(selectHypo(_).isDefined)
      val rsSelNext = rsSelNew.foldRight(rsSel)(add)
      val rsFreeNext = rsFreeNew.foldRight(rsFree)(add)
      if rsSelNext == rsSel then rsFreeNext
      else saturate0(rsSelNext, rsFreeNext)

    saturate0(
      rsSel = rs.filter(selectHypo(_).isDefined),
      rsFree = rs.filter(selectHypo(_).isEmpty)
    )

  def isDerivable(f: Fact, cs: Set[Clause]): Boolean = ???
