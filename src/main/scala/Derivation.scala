import Unification.unifier
import scala.annotation.tailrec
import Term._

object Derivation:

  def resolution(
      sel: Clause => Option[Fact] // selects one of the hypothesis, if any
  )(r1: Clause, r2: Clause): Option[Clause] =
    for
      hypo <- sel(r2)
      subst <- unifier(hypo.msg, r1.concl.msg)
    yield
      // println(s"  unifier: $subst")
      val newHypos = (r1.hypos ++ (r2.hypos - hypo)).map(subst(_))
      val newConcl = subst(r2.concl)
      Clause(newHypos, newConcl)

  // selects the first hypothesis which is not a variable
  // corresponds to sel() function in paper
  def selectFirstHypo(r: Clause): Option[Fact] =
    r.hypos.collectFirst:
      case f @ Fact(_, Name(_, _)) =>
        // println(s"sel name") // TODO remove
        // pprint.pprintln(r)
        // pprint.pprintln(f)
        f
      case f @ Fact(_, Func(_, _)) =>
        // println(s"sel func") // TODO remove
        // pprint.pprintln(r)
        // pprint.pprintln(f)
        f

  def basicResolution = resolution(selectFirstHypo)

  def saturate(rs: Set[Clause]): Set[Clause] =
    // add a clause and remove subsumed ones
    def add(r: Clause, rs: Set[Clause]): Set[Clause] =
      if rs.exists(_.subsumes(r)) then rs
      else rs.filter(!r.subsumes(_)) + r

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
      println("------ new resolutions")
      pprint.pprintln(rsNew.flatten)
      val (rsSelNew, rsFreeNew) =
        rsNew.flatten.partition(selectFirstHypo(_).isDefined)
      val rsSelNext = rsSelNew.foldRight(rsSel)(add)
      val rsFreeNext = rsFreeNew.foldRight(rsFree)(add)
      // when fixed point: return free rules
      if rsSelNext == rsSel && rsFreeNext == rsFree
      then rsFreeNext
      else saturate0(rsSelNext, rsFreeNext)

    saturate0(
      rsSel = rs.filter(selectFirstHypo(_).isDefined),
      rsFree = rs.filter(selectFirstHypo(_).isEmpty)
    )

  def derivable0(f: Fact, rules: List[Clause]): Boolean =

    def derive(
        r: Clause,
        history: List[Clause],
        depth: Int
    ): Set[Clause] =
      println(s"-------------------------- derive depth=$depth") // TODO remove
      println("history:")
      history.foreach(println)
      if depth > 10 then Set()
      else if history.exists(_.subsumes(r)) then
        println("  subsumption loop") // TODO remove
        println(s"    r: $r")
        Set() // stop here because subsumption loop
      else if selectFirstHypo(r).isEmpty then
        println(
          "  all hypothesis of are resolved"
        ) // TODO remove
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
    println(s"-------------------------- done") // TODO remove
    println(derivations)
    derivations.nonEmpty

  // combine two functions above
  def derivable(f: Fact, rules: Set[Clause]): Boolean =
    val rs = saturate(rules).toList
    pprint.pprintln(rs) // TODO remove
    derivable0(f, rs)
