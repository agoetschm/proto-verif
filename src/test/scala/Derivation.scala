import Term._
import Derivation.resolution
import Derivation.saturate
import pprint.pprintln

class DerivationTest extends munit.FunSuite {
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

  test("saturate") {
    val f = Func.Def("f", 1)
    val g = Func.Def("g", 1)
    val x = Var("x")
    val y = Var("y")
    val att = Fact.Def("att")

    val r1 = Clause(Set(att(f(x))), att(x))
    val r2 = Clause(Set(att(y)), att(g(y)))

    val rs = saturate(Set(r1, r2))
    assertEquals(rs.size, 1)

    val r3 = Clause(Set(att(g(x))), att(f(y)))
    val rs2 = saturate(Set(r1, r2, r3))
    // pprintln(rs2)
    assertEquals(rs2.size, 2)
  }
}
