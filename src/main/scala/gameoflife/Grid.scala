package gameoflife

import scala.collection.immutable.Seq

abstract class Grid(size: Int, rows: Seq[Row]) {
  def computeAliveNeighbours(position: (Int, Int)): Int = (for {
      i <- Seq(-1, 0, 1).map(i => i + position._1)
      j <- Seq(-1, 0, 1).map(j => j + position._2)
      if (i, j) != position
    } yield (i, j)).count(cellAt(_) == alive)

  def cellAt(position: (Int, Int)): CellState =
    rows.zipWithIndex
      .find(_._2 == position._1)
      .map(_._1.cellAt(position._2))
      .getOrElse(dead)

  def changeCellAt(position: (Int, Int), state: CellState): FiniteGrid = {
    FiniteGrid(size, rows.zipWithIndex
      .map((t: (Row, Int)) => if (t._2 == position._1) {
        t._1.changeCellAt(position._2, state)
      } else {
        t._1
      }))
  }

}
