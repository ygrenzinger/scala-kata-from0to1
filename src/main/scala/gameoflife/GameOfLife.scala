package gameoflife

import scala.collection.immutable.Seq

case class GameOfLife(grid: Grid) {

  def nextGeneration(): GameOfLife = {
    GameOfLife(grid.mapCells(
      (cellState, position) => CellState.nextState(cellState, computeAliveNeighbours(position))))
  }

  def computeAliveNeighbours(position: (Int, Int)): Int = (for {
    i <- Seq(-1, 0, 1).map(i => i + position._1)
    j <- Seq(-1, 0, 1).map(j => j + position._2)
    if (i, j) != position
  } yield (i, j)).count(cellAt(_) == alive)

  def makeAlive(position: (Int, Int)) =
    GameOfLife(grid.changeCellAt(position, alive))

  def cellAt(position: (Int, Int)): CellState =
    grid.cellAt(position)

}

object GameOfLife {
  def create(size: Int, cellState: CellState = dead, infinite: Boolean = false): GameOfLife =
    GameOfLife(Grid.create(size, cellState, infinite))

  def parse(schema: String, infinite: Boolean = false): GameOfLife = {
    val rows = schema.split("\n")
    GameOfLife(Grid(rows.length, rows.map(Row.parse), infinite))
  }
}
