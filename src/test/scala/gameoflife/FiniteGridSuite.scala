package gameoflife

import org.scalacheck.Gen
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FiniteGridSuite extends FunSuite with ScalaCheckPropertyChecks with Matchers {

  test("should return 0 alive neighbours at center in a empty finite grid") {
    val grid = FiniteGrid.create(3, dead)
    assert(grid.computeAliveNeighbours((1, 1)) == 0)
  }

  test("should return 8 alive neighbours at center in a full finite grid") {
    val grid = FiniteGrid.create(3, alive)
    assert(grid.computeAliveNeighbours((1, 1)) == 8)
  }

  test("should return 3 or 5 alive neighbours in a border positions in a full finite grid") {
    val grid = FiniteGrid.create(3, alive)
    val genPositions = for {
      i <- Gen.choose(0,2)
      j <- Gen.choose(0, 2)
    } yield (i,j)

    forAll (genPositions)  { (pos: (Int, Int)) =>

      whenever(pos != (1, 1)) {
        Some(grid.computeAliveNeighbours(pos)) should contain oneOf (3, 5)
      }
    }
  }

}
