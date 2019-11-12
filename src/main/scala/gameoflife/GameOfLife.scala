package gameoflife

case class GameOfLife(grid: FiniteGrid) {
  def nextGeneration(): GameOfLife = {
    val indices = 0 to grid.size
    val positions = for {
      i <- indices
      j <- indices
    } yield (i, j)
    val changeNextStateAtPosition = (g: FiniteGrid, pos: (Int, Int)) => {
      val nextState = CellState.nextState(grid.cellAt(pos), grid.computeAliveNeighbours(pos))
      g.changeCellAt(pos, nextState)
    }
    val nextGrid = positions.foldLeft(FiniteGrid.create(grid.size, dead))(changeNextStateAtPosition)
    GameOfLife(nextGrid)
  }

  def makeAlive(position: (Int, Int)) =
    GameOfLife(grid.changeCellAt(position, alive))

  def cellAt(position: (Int, Int)): CellState =
    grid.cellAt(position)

}

object GameOfLife {
  def createGameOfLife(size: Int, cellState: CellState = dead): GameOfLife =
    GameOfLife(FiniteGrid.create(size, cellState))

  def parse(schema: String): GameOfLife = {
    val rows = schema.split("\n")
    GameOfLife(FiniteGrid(rows.length, rows.map(Row.parse)))
  }
}
