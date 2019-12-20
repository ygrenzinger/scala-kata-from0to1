package gameoflife

import scala.collection.immutable.Seq

trait World {
  def cellAt(position: (Int, Int)): CellState
  def changeCellAt(position: (Int, Int), state: CellState): World
  def mapCells(f: (CellState, (Int, Int)) => CellState): World
}

object World {
  def neighbours(position: (Int, Int)): Seq[(Int, Int)] = for {
    i <- Seq(-1, 0, 1).map(i => i + position._1)
    j <- Seq(-1, 0, 1).map(j => j + position._2)
  } yield (i, j)
}
