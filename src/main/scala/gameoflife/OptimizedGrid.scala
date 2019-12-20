package gameoflife

case class OptimizedGrid(aliveCells: Set[(Int, Int)]) extends World {

  def cellAt(position: (Int, Int)): CellState =
    aliveCells.find(_ == position).map(_ => alive).getOrElse(dead)

  def changeCellAt(position: (Int, Int), state: CellState): World =
    if (state == alive) {
      OptimizedGrid(aliveCells + position)
    } else {
      OptimizedGrid(aliveCells - position)
    }

  def mapCells(f: (CellState, (Int, Int)) => CellState): World =
    OptimizedGrid(aliveCells.flatMap(World.neighbours))
}

object OptimizedGrid {
  def create(aliveCells: Set[(Int, Int)] = Set()): OptimizedGrid = OptimizedGrid(aliveCells)
}

