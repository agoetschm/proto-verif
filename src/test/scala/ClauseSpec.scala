import Term._
import Substitution.getSubstitution

class ClauseSpec extends munit.FunSuite {
  test("subsumption") {
    val foo = Func.Def("foo", 1)
    val bar = Func.Def("bar", 1)
    val x = Var("x")
    val y = Var("y")
    val att: Fact.Def = Fact.Def("att")

    val r1 = Clause(hypos = Set(att(foo(x))), concl = att(x))
    val r2 = Clause(hypos = Set(att(foo(y)), att(bar(y))), concl = att(y))

    assert(r1.subsumes(r2))
    assert(!r2.subsumes(r1))
  }
}
