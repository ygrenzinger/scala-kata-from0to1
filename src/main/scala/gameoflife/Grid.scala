package gameoflife

import scala.collection.immutable.Seq

case class Grid(size: Int, rows: Seq[Row], infinite: Boolean = false) {

  def cellAt(position: (Int, Int)): CellState = {
    val internalPosition = normalizePosition(position)
    rows.zipWithIndex
      .find(_._2 == internalPosition._1)
      .map(_._1.cellAt(internalPosition._2))
      .getOrElse(dead)
  }

  def changeCellAt(position: (Int, Int), state: CellState): Grid = {
    val internalPosition = normalizePosition(position)
    Grid(size, rows.zipWithIndex
      .map {
        case (row, i) if internalPosition._1 == i => row.changeCellAt(internalPosition._2, state)
        case (row, _) => row
      })
  }

  def mapCells(f: (CellState, (Int, Int)) => CellState): Grid = {
    val nextRows = rows.zipWithIndex.map {
      case (row, i) => Row(row.cells.zipWithIndex.map {
        case (cellState, j) => f(cellState, (i, j))
      })
    }
    Grid(size, nextRows)
  }

  private def normalizePosition(position: (Int, Int)) = {
    if (infinite)
      (normalizeUnit(position._1), normalizeUnit(position._2))
    else
      position
  }

  private def normalizeUnit(x: Int) = {
    val n = x % size
    if (n >= 0) n else size - Math.abs(n)
  }
}

object Grid {
  def create(size: Int, cellState: CellState, infinite: Boolean = false): Grid =
    Grid(size, (1 to size).map(_ => Row.create(size, cellState)), infinite)
}
