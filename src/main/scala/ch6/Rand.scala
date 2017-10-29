package ch6

object Rand {
  def unit[A](a: A): Rand[A] = rnd => (a, rnd)

  def map[A, B](r: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, nextRng) = r(rng)
    f(a) -> nextRng
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rngB) = ra(rng)
    val (b, rngC) = rb(rngB)

    (f(a, b), rngC)
  }

  def sequence[A](actions: List[Rand[A]]): Rand[List[A]] =
    actions.foldRight(unit(List.empty[A])) { (action, acc) =>
      map2(acc, action)((xs, a) => a :: xs)
    }

  def nextInt: Rand[Int] = _.nextInt

  //def nonNegativeInt: Rand[Int] = _.nonNegativeInt
  def nonNegativeInt: Rand[Int] = { rng =>
    val (n, nextRng) = rng.nextInt
    Math.abs(if (n == Int.MaxValue) n + 1 else n) -> nextRng
  }


  def double: Rand[Double] = RNG.double

  def intDouble: Rand[(Int, Double)] = RNG.intDouble

  def flatMap[A, B](action1: Rand[A])(f: A => Rand[B]): Rand[B] = { rng =>
    val (a, nextRng) = action1(rng)
    f(a)(nextRng)
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(nextInt))
}
