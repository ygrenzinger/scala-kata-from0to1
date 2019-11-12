package gameoflife

object Row {
  def create(size: Int, cellState: CellState): Row =
    Row((1 to size).map(_ => cellState))

  def parse(row: String): Row = Row(row.map((c: Char) => CellState.parse(c)))
}

case class Row(row: Seq[CellState]) {
  def cellAt(index: Int): CellState =
    row.zipWithIndex.find(_._2 == index).map(_._1).getOrElse(dead)

  def changeCellAt(index: Int, state: CellState): Row = {
    val cells = row.zipWithIndex
      .map((t: (CellState, Int)) => if (t._2 == index) {
        state
      } else {
        t._1
      })
    Row(cells)
  }
}