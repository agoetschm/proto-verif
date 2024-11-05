import Term._
import Derivation.resolution
import Derivation.saturate
import pprint.pprintln
import Derivation.derivable
import Derivation.basicResolution

class NeedhamSchroeder extends munit.FunSuite {
  test("derivable needham schroeder") {
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
        att(aenc(pair(y, nb(y, x)), x)),
        label = Some("2")
      ),
      Clause(
        Set(att(x), att(aenc(pair(na(x), y), z))),
        att(aenc(y, x)),
        label = Some("3")
      )
    )

    // expected attack:
    // C gets aenc(<pka, na>, pkc) with 1
    // C gets aenc(<na,nb>, pka) with 2
    // using previous, C gets aenc(nb, pkc)
    // C applies adec to get nb

    assert(derivable(att(nb(x, y)), rs))

//   ┌{att(x)} => att(pk(x))
//   │  ┌{att(x)} => att(pk(x))
//   │  │┌{att(x)} => att(aenc(pair(pk(ska()),na(x)),x)) [1]
//   │  │├{att(aenc(x,pk(y))),att(y)} => att(x)
//   │  ├{att(pk(y)),att(y)} => att(pair(pk(ska()),na(pk(y))))
//   │ ┌{att(x)} => att(pair(pk(ska()),na(pk(x))))
//   │ ├{att(pair(x,y))} => att(y)
//   │┌{att(x)} => att(na(pk(x)))
//   ││ ┌{att(x)} => att(pk(x))
//   ││ │ ┌{} => att(pk(skb()))
//   ││ │ │┌{att(x),att(y)} => att(pair(x,y))
//   ││ │ ││┌{att(x),att(y)} => att(aenc(x,y))
//   ││ │ ││├{att(aenc(pair(x,y),pk(skb())))} => att(aenc(pair(y,nb(y,x)),x)) [2]
//   ││ │ │├{att(pair(x_1,y_1)),att(pk(skb()))} => att(aenc(pair(y_1,nb(y_1,x_1)),x_1))
//   ││ │ ├{att(x),att(y),att(pk(skb()))} => att(aenc(pair(y,nb(y,x)),x))
//   ││ │┌{att(x),att(y)} => att(aenc(pair(y,nb(y,x)),x))
//   ││ │├{att(aenc(x,pk(y))),att(y)} => att(x)
//   ││ ├{att(pk(y_1)),att(y),att(y_1)} => att(pair(y,nb(y,pk(y_1))))
//   ││┌{att(x),att(y)} => att(pair(y,nb(y,pk(x))))
//   │││┌{att(x),att(y)} => att(aenc(x,y))
//   │││├{att(x),att(aenc(pair(na(x),y),z))} => att(aenc(y,x)) [3]
//   ││├{att(pair(na(x_1),y_1)),att(y),att(x_1)} => att(aenc(y_1,x_1))
//   │├{att(x),att(na(x_1)),att(y_2),att(x_1)} => att(aenc(nb(na(x_1),pk(x)),x_1))
//   ├{att(x),att(x_2),att(y_2),att(pk(x))} => att(aenc(nb(na(pk(x)),pk(x_2)),pk(x)))
//  ┌{att(x),att(x_2),att(y_2)} => att(aenc(nb(na(pk(x)),pk(x_2)),pk(x)))
//  ├{att(aenc(x,pk(y))),att(y)} => att(x)
// ┌{att(x),att(x_2),att(y_2)} => att(nb(na(pk(x)),pk(x_2)))
// ├{att(nb(x,y))} => att(nb(x,y))
// {att(x),att(x_2),att(y_2)} => att(nb(na(pk(x)),pk(x_2)))
  }
}
