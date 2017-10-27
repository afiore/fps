package ch5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  def isEmpty: Boolean = this match {
    case Empty => true
    case _     => false
  }

  def take(n: Int): Stream[A] =
    if (n == 0) {
      Stream.empty
    } else {
      this match {
        case Cons(h, xs) => Cons(h, () => xs().take(n - 1))
        case Empty       => Stream.empty
      }
    }

  def tails: Stream[Stream[A]] = this match {
    case Cons(_, t) => Cons(() => t(), () => t().tails)
    case _          => empty
  }

  def toList: List[A] = this match {
    case Empty      => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  def map[B](f: A => B): Stream[B] = this match {
    case Empty      => Empty
    case Cons(h, t) => Cons(() => f(h()), () => t().map(f))
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty      => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight[Boolean](false)((a, z) => p(a) || z)

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) =>
        val head = h()
        Some((head, t())).filter(_ => p(head))
      case _ => None
    }

  def filter_(p: A => Boolean): Stream[A] = this match {
    case Empty => empty[A]
    case Cons(h, t) =>
      val head = h()
      if (p(head)) {
        cons(head, t().filter(p))
      } else empty[A].concat(t()).filter(p)
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, z) =>
    if (p(a)) cons(a, z) else z
  }

  def concat[B >: A](that: => Stream[B]): Stream[B] = (this, that) match {
    case (Empty, Empty)      => empty
    case (Cons(_, _), Empty) => this
    case (Empty, Cons(_, _)) => that
    case (Cons(h, t), _)     => Cons(h, () => t().concat(that))
  }

  @tailrec
  final def startsWith[B >: A](that: Stream[B]): Boolean = (this, that) match {
    case (Empty, Empty)      => true
    case (Empty, Cons(_, _)) => false
    case (Cons(_, _), Empty) => true
    case (Cons(h, t), Cons(h1, t1)) =>
      h() == h1() && t().startsWith(t1())
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = (this, that) match {
    case (Empty, Empty)      => empty
    case (Empty, Cons(_, _)) => that.map(a => (None, Some(a)))
    case (Cons(_, _), Empty) => this.map(a => (Some(a), None))
    case (Cons(h, t), Cons(h1, t1)) =>
      Cons(
        () => Some(h()) -> Some(h1()),
        () => t().zipAll(t1())
      )
  }

  def zipAllUnf[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty)      => None
      case (Empty, Cons(h, t)) => Some(None -> Some(h()), (empty, t()))
      case (Cons(h, t), Empty) => Some(Some(h()) -> None, (t(), empty))
      case (Cons(h, t), Cons(h1, t1)) =>
        Some(Some(h()) -> Some(h1()), (t(), t1()))
    }
}

object Stream {
  case object Empty                                   extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  def cons[A](hd: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(s: S): Stream[A] = f(s).fold(empty[A]) { case (a, s1) => cons(a, go(s1)) }
    go(z)
  }

  def fibs: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Stream.empty[A]
    else Stream.cons(as.head, Stream(as.tail: _*))
}
