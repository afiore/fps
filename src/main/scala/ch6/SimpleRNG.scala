package ch6

import ch6.SimpleRNG.RNG

import scala.annotation.tailrec

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt0: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n       = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {
  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](r: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, nextRng) = r(rng)
    f(a) -> nextRng
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rngB) = ra(rng)
    val (b, rngC) = rb(rngB)

    (f(a, b), rngC)
  }

  def unit[A](a: A): Rand[A] = rnd => (a, rnd)

  def sequence[A](actions: List[Rand[A]]): Rand[List[A]] =
    actions.foldRight(unit(List.empty[A])) { (action, acc) =>
      map2(acc, action)((xs, a) => a :: xs)
    }

  def nextInt: Rand[Int] = _.nextInt0

  def nonNegativeInt: Rand[Int] = _.nonNegativeInt0

  def double: Rand[Double] = _.double0

  def intDouble: Rand[(Int, Double)] = _.intDouble0

  def flatMap[A, B](action1: Rand[A])(f: A => Rand[B]): Rand[B] = { rng =>
    val (a, nextRng) = action1(rng)
    f(a)(nextRng)
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(nextInt))

  trait RNG {
    def nextInt0: (Int, RNG)

    def nonNegativeInt0: (Int, RNG) = {
      val (n, rng) = nextInt0
      Math.abs(if (n == Int.MaxValue) n + 1 else n) -> rng
    }

    def double0: (Double, RNG) = {
      val (n, rng) = nonNegativeInt0
      (1.toDouble / n.toDouble, rng)
    }

    def intDouble0: ((Int, Double), RNG) = {
      val (n, _)   = nextInt0
      val (m, rng) = double0
      (n -> m, rng)
    }

    def ints0(count: Int): (List[Int], RNG) = {
      @tailrec
      def go(acc: List[Int], rng: RNG): (List[Int], RNG) =
        if (acc.size >= count) acc -> rng
        else {
          val (n, nextRng) = rng.nextInt0
          go(n :: acc, nextRng)
        }

      val (n, rng) = nextInt0
      go(List(n), rng)
    }
  }

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State[S, B]((s: S) => {
        val (a, s1) = run(s)
        (f(a), s1)
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State[S, B]((s: S) => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State[S, A]((s: S) => (a, s))

    def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] =
      as.foldRight(unit[S, List[A]](List.empty[A])) { (action, acc) =>
        for {
          as <- acc
          a  <- action
        } yield a :: as
      }
  }

}
