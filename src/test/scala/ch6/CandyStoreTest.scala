package ch6

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FreeSpec
import org.scalatest.Matchers

class CandyStoreTest extends FreeSpec with TypeCheckedTripleEquals with Matchers {
  "CandyStore" - {
    import CandyStore._
    def buyCandies(n: Int): List[Input] =
      (1 to n).toList.flatMap(_ => List(Coin, Turn))

    "simulateMachine" - {
      val machine = Machine(5, 10)

      "handles a single transaction" in {
        val ((coins, candies), machineState) = simulateMachine(buyCandies(1)).run(machine)
        (coins, candies) should ===(11, 4)
      }

      "handles multiple transactions" in {
        val ((coins, candies), machineState) = simulateMachine(buyCandies(4)).run(machine)
        (coins, candies) should ===(14, 1)
      }

      "doesn't sell when locked"  in {
        val locked = machine.copy(locked = true)
        simulateMachine(List(Coin, Coin)).run(locked)._2 should ===(locked)
      }

      "does nothing once sold out" in {
        val ((coins, candies), machineState) = simulateMachine(buyCandies(6)).run(machine)
        (coins, candies) should ===(15, 0)
      }
    }
  }

}
