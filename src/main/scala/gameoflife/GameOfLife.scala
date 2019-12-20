package gameoflife

import scala.collection.immutable.Seq

case class GameOfLife(world: World) {

  def nextGeneration(): GameOfLife = {
    GameOfLife(world.mapCells(
      (cellState, position) => CellState.nextState(cellState, computeAliveNeighbours(position))))
  }

  def computeAliveNeighbours(position: (Int, Int)): Int =
    World.neighbours(position)
      .filter(_ != position)
      .count(cellAt(_) == alive)

  def makeAlive(position: (Int, Int)): GameOfLife =
    GameOfLife(world.changeCellAt(position, alive))

  def cellAt(position: (Int, Int)): CellState =
    world.cellAt(position)

}

object GameOfLife {

  def create(size: Int, cellState: CellState = dead, infinite: Boolean = false): GameOfLife =
    GameOfLife(Grid.create(size, cellState, infinite))

  def parse(schema: String, infinite: Boolean = false): GameOfLife = {
    val rows = schema.split("\n")
    GameOfLife(Grid(rows.length, rows.map(Row.parse), infinite))
  }

  def createOptimized(): GameOfLife =
    GameOfLife(OptimizedGrid.create())

  def parseOptimized(schema: String, infinite: Boolean = false): GameOfLife = {
    val rows = schema.split("\n")

    def parseColumn(rowIndex: Int)(column: (Char, Int)): Option[(Int, Int)] =
      if (CellState.parse(column._1) == alive) {
        Some((rowIndex, column._2))
      } else None

    def parseRow(row: (String, Int)): Seq[Option[(Int, Int)]] = {
      val t: ((Char, Int)) => Option[(Int, Int)] = parseColumn(row._2)
      row._1.zipWithIndex.map(t)

    }

    val world = OptimizedGrid.create(rows.zipWithIndex.flatMap(parseRow).flatten.toSet)
    GameOfLife(world)
  }
}
