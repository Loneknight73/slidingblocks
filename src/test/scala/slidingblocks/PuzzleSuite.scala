package slidingblocks

class PuzzleSuite extends munit.FunSuite {

  import Puzzle.*
  import Block.*
  import Pos.*

  val miPuzzle: Puzzle = Puzzle(
    h = 5,
    w = 4,
    blocks = Vector(
      Block(Pos(0, 0), 2, 1),
      Block(Pos(0, 3), 2, 1),
      Block(Pos(2, 0), 2, 1),
      Block(Pos(2, 3), 2, 1),
      Block(Pos(1, 1), 2, 2),
      Block(Pos(3, 1), 1, 2),
      Block(Pos(4, 0), 1, 1),
      Block(Pos(4, 1), 1, 1),
      Block(Pos(4, 2), 1, 1),
      Block(Pos(4, 3), 1, 1)
    )
  )

}
