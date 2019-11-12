package gameoflife

// with pattern matching
sealed trait CellState

case object alive extends CellState

case object dead extends CellState

object CellState {
  def nextState(state: CellState, numberOfAliveNeighbours: Int): CellState = {
    (state, numberOfAliveNeighbours) match {
      case (_, 3) => alive
      case (alive, 2) => alive
      case _ => dead
    }
  }

  def parse(c: Char): CellState = c match {
    case 'x' => alive
    case _ => dead
  }
}

// with Object Oriented Polymorphism
//
//sealed trait CellState {
//  def nextState(numberOfAliveNeighbours: Int): CellState
//}
//
//case object alive extends CellState {
//  override def nextState(numberOfAliveNeighbours: Int): CellState = numberOfAliveNeighbours match {
//    case 2 | 3 => alive
//    case _ => dead
//  }
//  override def toString: String = "alive"
//}
//
//case object dead extends CellState {
//  override def nextState(numberOfAliveNeighbours: Int): CellState = numberOfAliveNeighbours match {
//    case 3 => alive
//    case _ => dead
//  }
//  override def toString: String = "dead"
//}
