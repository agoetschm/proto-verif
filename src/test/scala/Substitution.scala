import Term._
import Substitution.getSubstitution

class SubstitutionTest extends munit.FunSuite {
  test("t1.getSubstitution(t2).apply(t2) == t2") {
    val senc = Func.Def("senc", 2)
    val foo = Func.Def("foo", 2)
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val t1 = senc(foo(x, y), z)
    val t2 = senc(foo(x, x), y)

    val s = getSubstitution(t1, t2)
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
    val s = getSubstitution(t1, t2)
    assert(s.isEmpty)

    val t3 = senc(x, x)
    val t4 = senc(x, foo(x, x))
    val s2 = getSubstitution(t3, t4)
    assert(s2.isEmpty)
  }

  test("extend") {
    val f = Func.Def("f", 2)
    val g = Func.Def("g", 1)
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val s1 = Substitution.single(x, f(z, y))
    val s2 = s1.extend(y, g(z)).toOption.get

    assertEquals(s2, Substitution(Map((x, f(z, g(z))), (y, g(z)))))
  }
}
