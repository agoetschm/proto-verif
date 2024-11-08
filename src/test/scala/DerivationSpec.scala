import Term._
import Derivation.resolution
import Derivation.saturate
import pprint.pprintln
import Derivation.derivable
import Derivation.basicResolution

class DerivationSpec extends munit.FunSuite {
  test("resolution") {
    val f = Func.Def("f", 1)
    val g = Func.Def("g", 1)
    val x = Var("x")
    val y = Var("y")
    val att = Fact.Def("att")

    val r1 = Clause(Set(att(x)), att(f(x)))
    val r2 = Clause(Set(att(f(y))), att(g(y)))
    val r3 = basicResolution(r1, r2).get

    assertEquals(r3.withoutResolution, Clause(Set(att(x)), att(g(x))))
  }

  test("resolution var collision") {
    val f = Func.Def("f", 1)
    val g = Func.Def("g", 1)
    val x = Var("x")
    val att = Fact.Def("att")

    val r1 = Clause(Set(att(x)), att(f(g(x))))
    val r2 = Clause(Set(att(f(x))), att(g(x)))
    val r3 = basicResolution(r1, r2).get

    assertEquals(r3.withoutResolution, Clause(Set(att(x)), att(g(g(x)))))
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

    val r3 = Clause(Set(att(g(x))), att(f(x)))
    val rs2 = saturate(Set(r1, r2, r3))
    assertEquals(rs2.size, 3)
  }

  test("saturate should apply sdec") {
    val senc = Func.Def("senc", 2)
    val sdec = Func.Def("sdec", 2)
    val att = Fact.Def("att")
    val x = Var("x")
    val y = Var("y")

    // rules
    val r1 = Clause(Set(att(x), att(y)), att(senc(x, y)))
    val r2 = Clause(Set(att(x), att(y)), att(sdec(x, y)))
    val r3 = Clause(Set(att(sdec(senc(x, y), y)), att(y)), att(x))

    val rs = saturate(Set(r1, r2, r3))
    assert(
      rs.exists(r => r.withoutResolution == Clause(Set(att(x), att(y)), att(x)))
    )
  }

  test("derivable0") {
    val senc = Func.Def("senc", 2)
    val sdec = Func.Def("sdec", 2)
    val att = Fact.Def("att")
    val x = Var("x")
    val y = Var("y")

    // rules
    val r1 = Clause(Set(att(x), att(y)), att(senc(x, y)))
    val r2 = Clause(Set(att(x), att(y)), att(sdec(x, y)))
    val r3 = Clause(Set(att(sdec(senc(x, y), y)), att(y)), att(x))
    // initial knowledge
    val m = Name.Def("m")()
    val k = Name.Def("k")()
    val k1 = Clause(Set(), att(k))
    val k2 = Clause(Set(), att(senc(m, k)))

    assert(Derivation.derivable0(att(m), List(r1, r2, r3, k1, k2)))
    assert(!Derivation.derivable0(att(m), List(r1, r2, r3, k2)))
  }

  test("derivable") {
    val senc = Func.Def("senc", 2)
    val sdec = Func.Def("sdec", 2)
    val att = Fact.Def("att")
    val x = Var("x")
    val y = Var("y")

    // rules
    val r1 = Clause(Set(att(x), att(y)), att(senc(x, y)))
    val r2 = Clause(Set(att(x), att(y)), att(sdec(x, y)))
    val r3 = Clause(Set(att(sdec(senc(x, y), y)), att(y)), att(x))
    // initial knowledge
    val m = Name.Def("m")()
    val k = Name.Def("k")()
    val k1 = Clause(Set(), att(k))
    val k2 = Clause(Set(), att(senc(m, k)))

    assert(derivable(att(m), Set(r1, r2, r3, k1, k2)))
    assert(!derivable(att(m), Set(r1, r2, r3, k2)))
  }
}
