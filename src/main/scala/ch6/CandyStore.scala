package ch6

object CandyStore {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def insertCoin: Machine =
      if (!locked && candies > 0) copy(locked = true)
      else this

    def turnKnob: Machine =
      if (locked && candies > 0) copy(candies = candies - 1, coins = coins + 1)
      else this
  }
  object Machine {
    def apply(candies: Int, coins: Int): Machine =
      Machine(locked = true, candies, coins)
  }


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val actions = inputs.map {
      case Coin => State.modify[Machine](_.insertCoin)
      case Turn => State.modify[Machine](_.turnKnob)
    }

    for {
      _ <- State.sequence(actions)
      machine <- State.get[Machine]
    } yield (machine.coins, machine.candies)
  }
}

