package ch6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def apply(seed: Long): RNG = new RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = RNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rngNext) = rng.nextInt
    Math.abs(if (n == Int.MaxValue) n + 1 else n) -> rngNext
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rngNext) = nonNegativeInt(rng)
    (1.toDouble / n.toDouble, rngNext)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng1)   = rng.nextInt
    val (m, rng2) = double(rng1)
    (n -> m, rng2)
  }

  def ints(rng: RNG)(count: Int): (List[Int], RNG) = {
    @tailrec
    def go(acc: List[Int], rng: RNG): (List[Int], RNG) =
      if (acc.size >= count)
        acc -> rng
      else {
        val (n, nextRng) = rng.nextInt
        go(n :: acc, nextRng)
      }

    val (n, rng1) = rng.nextInt
    go(List(n), rng1)
  }
}

