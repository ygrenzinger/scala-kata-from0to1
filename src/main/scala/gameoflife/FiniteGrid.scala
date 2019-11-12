package gameoflife

import scala.collection.immutable.Seq

case class FiniteGrid(size: Int, rows: Seq[Row]) extends Grid(size: Int, rows: Seq[Row])

object FiniteGrid {
  def create(size: Int, cellState: CellState): FiniteGrid =
    FiniteGrid(size, (1 to size).map(_ => Row.create(size, cellState)))
}
