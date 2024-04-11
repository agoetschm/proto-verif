@main def hello(): Unit =
  import Term._

  val senc = Func.Def("senc", 2)
  val m = Name.Def("m", 0)
  val att: Fact.Def = Fact.Def("att")

  val x = Var("x")
  val k = Var("k")
  val ability = Clause(hypos = Set(att(x), att(k)), concl = att(senc(x, k)))
  val mm = m()
  println(ability)
