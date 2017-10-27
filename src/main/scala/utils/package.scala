package object utils {
  def tracing[A,B](lbl: String)(f: A => B): A => B = {
    (a: A) =>
      val out = f(a)
      println(s"$lbl (args: $a, out: $out)")
      out
  }

  val plusOne = tracing("plusOne")((a: Int) => a + 1)
  val lessThanThree = tracing("LessThanThree")((a: Int) => a < 3)
}
