object Derivability:
  def saturate(rs: Seq[Clause]): Seq[Clause] = ???

  def add(r: Clause, rs: Seq[Clause]): Seq[Clause] =
    if rs.exists(_.doesSubsume(r)) then rs
    else ???
