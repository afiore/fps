package object ch6 {
  type Rand[+A] = RNG => (A, RNG)
}
