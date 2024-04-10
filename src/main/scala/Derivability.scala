import Unification.unifier
import scala.annotation.tailrec
object Derivability:
  def saturate(rs: List[Clause]): List[Clause] = ???

  def add(r: Clause, rs: List[Clause]): List[Clause] =
    if rs.exists(_.doesSubsume(r)) then rs
    else r :: rs.filter(!r.doesSubsume(_))

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
