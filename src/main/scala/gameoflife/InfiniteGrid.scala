package gameoflife

import scala.collection.immutable.Seq

case class InfiniteGrid(size: Int, rows: Seq[Row]) extends Grid(size: Int, rows: Seq[Row]) {
  override def computeAliveNeighbours(position: (Int, Int)): Int =
    super.computeAliveNeighbours(normalizePosition(position))

  override def cellAt(position: (Int, Int)): CellState =
    super.cellAt(normalizePosition(position))

  override def changeCellAt(position: (Int, Int), state: CellState): FiniteGrid =
    super.changeCellAt(normalizePosition(position), state)

  private def normalizePosition(position: (Int, Int)) =
    (normalizeUnit(position._1), normalizeUnit(position._2))

  private def normalizeUnit(x: Int) = {
    val n = x % size
    if (n >= 0) n else size - Math.abs(n)
  }
}

object InfiniteGrid {
  def create(size: Int, cellState: CellState): InfiniteGrid =
    InfiniteGrid(size, (1 to size).map(_ => Row.create(size, cellState)))
}
