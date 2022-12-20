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

enum Move:
  case Left, Right, Up, Down

case class Block(ul: Pos, h: Int, w: Int) {
  def move(m: Move): Block = {
    m match {
      case Left  => copy(ul = ul.deltaCol(-1))
      case Right => copy(ul = ul.deltaCol(1))
      case Up    => copy(ul = ul.deltaRow(-1))
      case Down  => copy(ul = ul.deltaRow(1))
    }
  }
}

case class Puzzle(h: Int, w: Int, blocks: Vector[Block])