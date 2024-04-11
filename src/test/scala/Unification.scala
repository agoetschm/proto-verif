import Term._

class UnificationTest extends munit.FunSuite {
  test("simple") {
    val a = Func.Def("a", 2)
    val b = Func.Def("b", 1)
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val s = Unification.unifier(a(x, b(x)), a(b(z), y)).get

    assertEquals(s(x), b(z))
    assertEquals(s(y), b(b(z)))
  }

  test("var renaming") {
    val x = Var("x")
    val y = Var("y")

    val s = Unification.unifier(x, y).get

    assertEquals(s(x), y)
  }

  test("fail") {
    val a = Func.Def("a", 2)
    val b = Func.Def("b", 1)
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val s = Unification.unifier(a(x, b(x)), a(b(x), y))
    assert(s.isEmpty)
  }
}
