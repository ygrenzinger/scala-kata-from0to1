package gameoflife

import org.scalatest.FunSuite

class GameOfLifeSuite extends FunSuite {

  test("should create a game of life") {
    val gameOfLife = GameOfLife.create(2).makeAlive((0,0))
    assert(gameOfLife.cellAt((0, 0)) == alive)
    assert(gameOfLife.cellAt((1, 1)) == dead)
  }

  test("should parse graphical representation") {
   val gameOfLife: GameOfLife = GameOfLife.parse(
      """x_
        |__""".stripMargin)

    assert(gameOfLife.cellAt((0, 0)) == alive)
    assert(gameOfLife.cellAt((1, 1)) == dead)
  }

  test("should handle Still lifes like Block") {
    val gameOfLife: GameOfLife = GameOfLife.parse(
      """____
        |_xx_
        |_xx_
        |____""".stripMargin)
    assert(gameOfLife.nextGeneration() == gameOfLife)
  }

  test("should handle Oscillators like Blinker") {
    val gameOfLife: GameOfLife = GameOfLife.parse(
      """_____
        |__x__
        |__x__
        |__x__
        |_____""".stripMargin)
    val nextGameOfLife: GameOfLife = GameOfLife.parse(
      """_____
        |_____
        |_xxx_
        |_____
        |_____""".stripMargin)
    assert(gameOfLife.nextGeneration() == nextGameOfLife)
  }

}
