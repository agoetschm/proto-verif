import Term._
import Derivation.resolution
import Derivation.saturate
import pprint.pprintln
import Derivation.derivable
import Derivation.basicResolution

class NeedhamSchroederLowe extends munit.FunSuite {
  test("not derivable lowe") {
    val pair = Func.Def("pair", 2)
    val aenc = Func.Def("aenc", 2)
    val pk = Func.Def("pk", 1)
    val na = Name.Def("na", arity = 1)
    val nb = Name.Def("nb", arity = 2)

    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    val ska = Name("ska")
    val skb = Name("skb")
    val skc = Name("skc")

    val att = Fact.Def("att")

    val rs = Set(
      // pair
      Clause(Set(att(x), att(y)), att(pair(x, y))),
      Clause(Set(att(pair(x, y))), att(x)),
      Clause(Set(att(pair(x, y))), att(y)),
      // aenc
      Clause(Set(att(x), att(y)), att(aenc(x, y))),
      Clause(Set(att(aenc(x, pk(y))), att(y)), att(x)),
      // pk
      Clause(Set(att(x)), att(pk(x))),
      // inital knowledge
      Clause(Set(), att(skc)),
      Clause(Set(), att(pk(ska))),
      Clause(Set(), att(pk(skb))),
      // protocol
      Clause(
        Set(att(x)),
        att(aenc(pair(pk(ska), na(x)), x)),
        label = Some("1")
      ),
      Clause(
        Set(att(aenc(pair(x, y), pk(skb)))),
        att(
          aenc(pair(y, pair(nb(y, x), pk(skb))), x)
        ), // <== that's where Lowe's fix is (adding Bob's public key)
        label = Some("2")
      ),
      Clause(
        Set(att(x), att(aenc(pair(na(x), y), z))),
        att(aenc(y, x)),
        label = Some("3")
      )
    )

    // TODO this fails
    assert(!derivable(att(nb(x, y)), rs))

    // derivation:

//    ┌{att(x)} => att(pk(x))
//    │ ┌{} => att(pk(skb()))
//    │ │ ┌{} => att(pk(skb()))
//    │ │ │┌{att(x),att(y)} => att(pair(x,y))
//    │ │ ││┌{att(x),att(y)} => att(aenc(x,y))
//    │ │ ││├{att(aenc(pair(x,y),pk(skb())))} => att(aenc(pair(y,pair(nb(y,x),pk(skb()))),x)) [2]
//    │ │ │├{att(pair(x_1,y_1)),att(pk(skb()))} => att(aenc(pair(y_1,pair(nb(y_1,x_1),pk(skb()))),x_1))
//    │ │ ├{att(x),att(y),att(pk(skb()))} => att(aenc(pair(y,pair(nb(y,x),pk(skb()))),x))
//    │ │┌{att(x),att(y)} => att(aenc(pair(y,pair(nb(y,x),pk(skb()))),x))
//    │ │├{att(aenc(pair(x,y),pk(skb())))} => att(aenc(pair(y,pair(nb(y,x),pk(skb()))),x)) [2]
//    │ ├{att(pk(skb())),att(y)} => att(aenc(pair(pair(nb(y,pk(skb())),pk(skb())),pair(nb(pair(nb(y,pk(skb())),pk(skb())),y),pk(skb()))),y))
//    │┌{att(y)} => att(aenc(pair(pair(nb(y,pk(skb())),pk(skb())),pair(nb(pair(nb(y,pk(skb())),pk(skb())),y),pk(skb()))),y))
//    │├{att(aenc(x,pk(y))),att(y)} => att(x)
//    ├{att(pk(y_1)),att(y_1)} => att(pair(pair(nb(pk(y_1),pk(skb())),pk(skb())),pair(nb(pair(nb(pk(y_1),pk(skb())),pk(skb())),pk(y_1)),pk(skb()))))
//   ┌{att(x)} => att(pair(pair(nb(pk(x),pk(skb())),pk(skb())),pair(nb(pair(nb(pk(x),pk(skb())),pk(skb())),pk(x)),pk(skb()))))
//   ├{att(pair(x,y))} => att(x)
//  ┌{att(x)} => att(pair(nb(pk(x),pk(skb())),pk(skb())))
//  ├{att(pair(x,y))} => att(x)
// ┌{att(x)} => att(nb(pk(x),pk(skb())))
// ├{att(nb(x,y))} => att(nb(x,y))
// {att(x)} => att(nb(pk(x),pk(skb())))

  }
}
