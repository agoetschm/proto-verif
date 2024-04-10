import Term._
import Derivability.resolution

class DerivabilityTest extends munit.FunSuite {
  test("resolution") {
    val f = Func.Def("f", 1)
    val g = Func.Def("g", 1)
    val x = Var("x")
    val y = Var("y")
    val att = Fact.Def("att")

    val r1 = Clause(Set(att(f(x))), att(x))
    val r2 = Clause(Set(att(y)), att(g(y)))
    val r3 = resolution(r1, r2).get

    assertEquals(r3, Clause(Set(att(f(x))), att(g(x))))
  }
}
