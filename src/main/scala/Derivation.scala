import Unification.unify
import scala.annotation.tailrec
import Term._

object Derivation:

  /** resolves two clause by selecting a hypothesis from r2 and unifiying it
    * with the conclusion of r1
    */
  def resolution(
      sel: Clause => Option[Fact] // selects one of the hypothesis, if any
  )(r1: Clause, r2: Clause): Option[Clause] =
    val r2p = r2.withVarsDifferentFrom(r1)
    // println(s"resolve: r1=$r1 r2=$r2p")
    for
      hypo <- sel(r2p)
      unifier <- unify(hypo.msg, r1.concl.msg)
    yield
      // println(s"  unifier: $unifier")
      val newHypos = (r1.hypos ++ (r2p.hypos - hypo)).map(unifier(_))
      val newConcl = unifier(r2p.concl)
      val resolutionOf = Some((r1, r2, unifier))
      Clause(newHypos, newConcl, resolutionOf)

  // selects the first hypothesis which is not a variable
  // corresponds to sel() function in Blanchet2011
  private def selectFirstHypo(r: Clause): Option[Fact] =
    r.hypos.collectFirst:
      case f @ Fact(_, Name(_, _)) => f
      case f @ Fact(_, Func(_, _)) => f

  def basicResolution = resolution(selectFirstHypo)

  /** adds a clause and removes subsumed ones */
  def add(r: Clause, rs: Set[Clause]): Set[Clause] =
    if rs.exists(_.subsumes(r)) then rs
    else rs.filter(!r.subsumes(_)) + r

  /** resolves clauses in a set of clauses and removes subsumed ones in order to
    * optimize derivability
    */
  def saturate(rs: Set[Clause]): Set[Clause] =
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
      yield basicResolution(rFree, rSel)
      // println()
      // println("------ saturate0: new resolutions")
      // rsNew.flatten.foreach(r => println(r.withoutResolution))
      // println("------ saturate0: end new resolutions")
      val (rsSelNew, rsFreeNew) =
        rsNew.flatten.partition(selectFirstHypo(_).isDefined)
      val rsSelNext = rsSelNew.foldRight(rsSel)(add)
      val rsFreeNext = rsFreeNew.foldRight(rsFree)(add)
      // println("------ saturate0: rsSelNext")
      // rsSelNext.foreach(r => println(r.withoutResolution))
      // println("------ saturate0: rsFreeNext")
      // rsFreeNext.foreach(r => println(r.withoutResolution))
      // when fixed point: return free rules
      if rsSelNext == rsSel && rsFreeNext == rsFree
      then
        // println("------ saturate resulting clauses:")
        // rsFreeNext.foreach(r => println(r.withoutResolution))
        rsFreeNext
      else saturate0(rsSelNext, rsFreeNext)

    saturate0(
      rsSel = rs.filter(selectFirstHypo(_).isDefined),
      rsFree = rs.filter(selectFirstHypo(_).isEmpty)
    )

  /** backward depth first search which checks whether a fact is derivable from
    * a set of rules
    */
  def derivable0(f: Fact, rules: List[Clause]): Boolean =

    def derive(
        r: Clause,
        history: List[Clause],
        depth: Int
    ): Set[Clause] =
      // println(s"-------------------------- derive depth=$depth")
      // println("history:")
      // history.foreach(println)
      if depth > 10 then Set()
      else if history.exists(_.subsumes(r)) then
        Set() // stop here because of subsumption loop
      else if selectFirstHypo(r).isEmpty then
        Set(r) // all hypothesis of are resolved
      else
        rules.foldLeft(Set(): Set[Clause])((acc, rp) =>
          if acc.size >= 1 then acc
          else
            basicResolution(rp, r)
              .map(resol => derive(resol, r :: history, depth = depth + 1))
              .getOrElse(Set())
        )

    val derivations = derive(Clause(Set(f), f), List(), depth = 0)
    println(s"-------------------------- derivation")
    derivations.foreach(r => println(r))
    println(s"--------------------------")
    derivations.nonEmpty

  // combine two functions above
  def derivable(f: Fact, rules: Set[Clause]): Boolean =
    val rs = saturate(rules).toList
    derivable0(f, rs)
