package slidingblocks

/**
 * The case class `Pos` encodes positions in the terrain.
 *
 * IMPORTANT NOTE
 *  - The `row` coordinate denotes the position on the vertical axis
 *  - The `col` coordinate is used for the horizontal axis
 *  - The coordinates increase when moving down and right
 *
 * Illustration:
 *
 * 0 1 2 3   <- col axis
 * 0 o o o o
 * 1 o o o o
 * 2 o # o o    # is at position Pos(2, 1)
 * 3 o o o o
 *
 * ^
 * |
 *
 * row axis
 */
case class Pos(row: Int, col: Int):
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)

enum Direction:
  case Left, Right, Up, Down

case class Block(ul: Pos, h: Int, w: Int) {

  def move(m: Direction): Block = {
    m match {
      case Direction.Left  => copy(ul = ul.deltaCol(-1))
      case Direction.Right => copy(ul = ul.deltaCol(1))
      case Direction.Up    => copy(ul = ul.deltaRow(-1))
      case Direction.Down  => copy(ul = ul.deltaRow(1))
    }
  }

  def overlap(other: Block): Boolean = {
    val r = (ul.row >= other.ul.row + other.h) ||
      (ul.row + h <= other.ul.row) ||
      (ul.col >= other.ul.col + other.w) ||
      (ul.col + w <= other.ul.col)
    !r
  }

}

// Move Block with index i in blocks in the specified direction
case class Move(bi: Int, d: Direction)

case class Puzzle(h: Int, w: Int, blocks: Vector[Block]) {
  val nblocks = blocks.size

  val moves: List[Move] = {
    val l = for {
      i <- 0 until nblocks
      d <- Direction.values
    } yield Move(i, d)
    l.toList
  }

  def isLegal(m: Move): Boolean = {
    val newblock = blocks(m.bi).move(m.d)
    val inTray = (newblock.ul.row >= 0) &&
                 (newblock.ul.col >= 0) &&
                 (newblock.ul.row + newblock.h <= h) &&
                 (newblock.ul.col + newblock.w <= w)
    var r = false
    if (inTray) {
      val x = blocks.zipWithIndex.dropWhile( (b, i) => i == m.bi || !newblock.overlap(b))
      if (x.size == 0) then
        r = true
    }
    r
  }

  def move(m: Move): Puzzle = {
    val newblocks = blocks.updated(m.bi, blocks(m.bi).move(m.d))
    copy(blocks = newblocks)
  }

  def legalNeighbors: List[(Puzzle, Move)] = {
    for {
      m <- moves
      p = move(m) if isLegal(m)
    } yield (p, m)
  }
}

object Main extends App {

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

//  val n = miPuzzle.legalNeighbors
//  println(n)
//  println(n.size)

  class miSolver extends Solver {
    override val startPuzzle: Puzzle = miPuzzle
    override val goal: List[Block] = List(
      Block(Pos(3, 1), 2, 2)
    )
  }

  val s = miSolver()
  print(s.solution)
}