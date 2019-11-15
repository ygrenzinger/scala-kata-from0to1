package gameoflife

import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InfiniteGridSuite extends FunSuite with ScalaCheckPropertyChecks with Matchers {

  test("should return 8 alive neighbours at any position in a full infinite grid") {
    val gameOfLife = GameOfLife.create(3, alive, infinite = true)
    forAll { (i: Int, j: Int) =>
      gameOfLife.computeAliveNeighbours((i, j)) should be (8)
    }
  }

}
