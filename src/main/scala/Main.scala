@main def hello(): Unit =
  import Term._

  val senc = FuncDef("senc", 2)

  val att: Id = "att"
  val x = Var("x")
  val k = Var("k")
  val attX = Fact(att, Seq(x))
  val attK = Fact(att, Seq(k))
  val sencXK = senc(x, k)
  val attSencXK = Fact(att, Seq(sencXK))
  val ability = Clause(hypos = Seq(attX, attK), concl = attSencXK)
  val m = Name("m")
  println(ability)

def saturate(cs: Set[Clause]): Set[Clause] = ???

def isDerivable(f: Fact, cs: Set[Clause]): Boolean = ???
