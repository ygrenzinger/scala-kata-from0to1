package gameoflife

import org.scalatest.FunSuite

import CellState._

class CellStateTest extends FunSuite {
  test("Any live cell with fewer than two live neighbours dies, as if by underpopulation.") {
    assert(nextState(alive, 0) == dead)
    assert(nextState(alive,1) == dead)
  }

   test("Any live cell with two or three live neighbours lives on to the next generation.") {
    assert(nextState(alive, 2) == alive)
    assert(nextState(alive,3) == alive)
  }

   test("Any live cell with more than three live neighbours dies, as if by overpopulation.") {
     (4 to 8).foreach(x => assert(nextState(alive, x) == dead))
  }

  test("Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.") {
    assert(nextState(dead, 3) == alive)
  }

  test("Any dead cell stays dead if not exactly three live neighbours.") {
    (0 to 8).filter(_ != 3).foreach(x => assert(nextState(dead, x) == dead))
  }

}
