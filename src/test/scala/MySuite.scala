import Term._

class MySuite extends munit.FunSuite {
  test("t1.getSubstitution(t2).apply(t2) == t2") {
    val senc = Func.Def("senc", 2)
    val foo = Func.Def("foo", 2)
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val t1 = senc(foo(x, y), z)
    val t2 = senc(foo(x, x), y)

    val s = Term.getSubstitution(t1, t2)
    assert(s.nonEmpty)

    assertEquals(s.get(t1), t2)
  }

  test("no substitution") {
    val senc = Func.Def("senc", 2)
    val foo = Func.Def("foo", 2)
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val t1 = senc(foo(x, y), z)
    val t2 = senc(x, foo(x, x))
    val s = Term.getSubstitution(t1, t2)
    assert(s.isEmpty)
  }

  test("no substitution tricky") {
    val senc = Func.Def("senc", 2)
    val foo = Func.Def("foo", 2)
    val x = Var("x")

    val t1 = senc(x, x)
    val t2 = senc(x, foo(x, x))
    val s = Term.getSubstitution(t1, t2)
    assert(s.isEmpty)
  }

  test("subsumption") {
    val foo = Func.Def("foo", 1)
    val bar = Func.Def("bar", 1)
    val x = Var("x")
    val y = Var("y")
    val att: Fact.Def = Fact.Def("att")

    val r1 = Clause(hypos = Set(att(foo(x))), concl = att(x))
    val r2 = Clause(hypos = Set(att(foo(y)), att(bar(y))), concl = att(y))

    assert(r1.doesSubsume(r2))
    assert(!r2.doesSubsume(r1))
  }
}
