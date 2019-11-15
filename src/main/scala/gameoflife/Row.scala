package gameoflife

object Row {
  def create(size: Int, cellState: CellState): Row =
    Row((1 to size).map(_ => cellState))

  def parse(row: String): Row = Row(row.map(c => CellState.parse(c)))
}

case class Row(val cells: Seq[CellState]) {
  def cellAt(index: Int): CellState =
    cells.zipWithIndex.find(_._2 == index).map(_._1).getOrElse(dead)

  def changeCellAt(index: Int, newState: CellState): Row = {
    val cells = this.cells.zipWithIndex
      .map {
        case (_, i) if (index == i) => newState
        case (state, _) => state
      }
    Row(cells)
  }
}