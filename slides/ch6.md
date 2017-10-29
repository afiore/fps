---
title: FP in scala 
theme: black
revealOptions:
  transition: 'normal'
---
# FP in scala

## session 2

---
## Wrap up of chapter 5

```scala
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h, t)
      else t)
```
<ul>
<li class="fragment" data-fragment-index="1"> `t` is passed by name, so it's lazily evaluated</li>
<li class="fragment" data-fragment-index="2"> terminates on infinite streams</li>
</ul>
---

## Unfold and corecursion

```scala
def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
  def go(s: S): Stream[A] = f(s).fold(empty[A]) { 
    case (a, s1) => cons(a, go(s1)) 
  }
  go(z) 
}
```
<ul>
<li class="fragment" data-fragment-index="1"> recursive functions consume data, corecursive ones produce it</li>
<li class="fragment" data-fragment-index="2"> don't need to terminate, as long as they _stay productive_</li>
</ul>
---

### Usage of unfold 

```scala
def from(n: Int): Stream[Int] = 
  unfold(n)(n => Some((n, n+1)))
```
```scala
val fibonacci: Stream[Int] =
  unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }
```

Most recursive functions covered in ch.5 can also be re-written in terms of `unfold` (i.e. `map`, `zipAll`, etc.) 

---

# Chapter 6

## Purely functional state

---

## Try test this...

```scala
def rollDie: Int = {
  val rng = new scala.util.Random
  rng.nextInt(6)
}
```
<ul>
<li class="fragment"> _non deterministic:_ outcome depends on some hidden internal state of `rng` </li>
<li class="fragment"> off by one error  </li>
</ul>
---

## ...or perhaps this?

```scala
def rollDie(rng: scala.util.Random): Int =
  rng.nextInt(6)
```

<ul>
<li class="fragment"> hard to guarantee that `nextInt` hasn't been run</li>
<li class="fragment"> `rng` state is still invisible! </li>
</ul>

---

## PF pseudo random state

Interface:

```scala
trait RNG {
  def nextInt: (Int, RNG)
}
```

Implementation:

```scala
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
```

---

## Usage

```scala
scala>  SimpleRNG(1L).nextInt
res1: (Int, RNG) = (384748,SimpleRNG(25214903928))
```

Forall _n_ in _Long_
```scala
scala>  SimpleRNG(n).nextInt == SimpleRNG(n).nextInt
res3: Boolean = true
```
---

## Exercises:

Using `RNG`, implement:

```scala
def nonNegativeInt(rng: RNG): (Int, RNG)
```
```scala
def double(rng: RNG): (Double, RNG)
```
```scala
def ints(count: Int)(rng: RNG): (List[Int], RNG)
```
---

## General pattern

```scala
scala> RNG(1L).nextInt
res2: (Int, ch6.RNG) = (384748,ch6.RNG$$anon$1@7cc14ed4)

scala> RNG(1L).nextInt._2.nextInt
res3: (Int, ch6.RNG) = (-1151252339,ch6.RNG$$anon$1@2d1f13b)
```

<ul>
<li class="fragment"> `RNG` is never directly mutated, but a new generator is always returned 
alongside each method return type</li>
<li class="fragment"> slightly _awkward_! </li>
</ul>
---

### Dealing with the awkwardness...

```scala
type Rand[+A] = RNG => (A, RNG)
```

We scrap some boilerplate by introducing a type alias...

```scala
def nonNegativeInt: Rand[Int] = { rng =>
  val (n, nextRng) = rng.nextInt
  Math.abs(if (n == Int.MaxValue) n + 1 else n) -> nextRng
}
```

<div class="fragment">... but `RNG` is still present both as an argument and as a return value! </div>

--- 

### Abstracting over state actions

- Lifts a pure value into a `Rand`
```scala
  def unit[A](a: A): Rand[A]
```
- Map a pure function to a random action
```scala
  def map[A, B](r: Rand[A])(f: A => B)
    : Rand[B]
```
- Map a pure binary function to two random actions:
```scala
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
```
---

### Abstracting over state actions

- Map a random action to another
```scala
 def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B]
```
- Flatten a list of actions into an action of list
```scala
  def sequence[A](actions: List[Rand[A]]): Rand[List[A]] 
```
---

### A generic data type for state actions

We could generalise our `Rand[A]` into:

```scala
type State[S, A] = S => (A, S)
```

Or, better, written as a class:

```scala
case class State[S,+A](run: S => (A, S))
```

<div class="fragment"> `unit`, `map`, `flatMap`, `sequence` can now be implemented in terms of `State`</div>
---
### State combinators

```scala
def get[S]: State[S, S]
``` 

swaps the value of `A` with `S`

--- 
### State combinators

```scala
def set[S]: State[S, Unit]
```

Replaces the value of `S` with a new value

---
### State combinators


```scala
def modify[S](f: S => S): State[S, Unit] = {
  s <- get
  _ <- set(f(s))
} yield ()
```

Modifies the value of `S` by applying a function `f`

---
