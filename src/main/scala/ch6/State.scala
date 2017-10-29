package ch6

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State((s: S) => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((s: S) => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S1](s: S1): State[S1, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] =
    as.foldRight(unit[S, List[A]](List.empty[A])) { (action, acc) =>
      for {
        as <- acc
        a  <- action
      } yield a :: as
    }
}
