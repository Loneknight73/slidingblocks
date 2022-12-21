package slidingblocks

/**
 * This component implements the solver for the sliding blocks puzzle
 */
trait Solver:

  val startPuzzle: Puzzle
  val goal: List[Block]

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(p: Puzzle): Boolean =
    println(p)
    val x = goal.dropWhile(g => p.blocks.contains(g))
    x.size == 0

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a lazy list of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(p: Puzzle, history: List[Move]): LazyList[(Puzzle, List[Move])] = {
    for {
      (n, m) <- p.legalNeighbors.to(LazyList)
    } yield (n, m :: history)
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: LazyList[(Puzzle, List[Move])],
                       explored: Set[Puzzle]): LazyList[(Puzzle, List[Move])] = {
    val nn = neighbors.filter(x => !explored.contains(x._1))
    nn
  }

  /**
   * The function `from` returns the lazy list of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * lazy list.
   *
   * The blocks in the lazy list `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the lazy list.
   *
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * lazy list `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   *
   * The resulting lazy list should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the lazy list.
   *
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted lazy list.
   */
  def from(initial: LazyList[(Puzzle, List[Move])],
           explored: Set[Puzzle]): LazyList[(Puzzle, List[Move])] = {
    val newPuzzles: LazyList[(Puzzle, List[Move])] = for {
      (puzzle, history) <- initial
      newPuzzle <- newNeighborsOnly(neighborsWithHistory(puzzle, history), explored)
    } yield newPuzzle
//    println(s"newsize = ${newPuzzles.size}")
//    println(s"expsize = ${explored.size}")
    newPuzzles.headOption match {
      case None => initial
      case _ => {
//        val x = newPuzzles.map(_._1).toSet
//        val y = explored ++ x
        initial #::: newPuzzles #::: from(newPuzzles, explored ++ newPuzzles.toSet.map(_._1))
      }
    }
  }

  /**
   * The lazy list of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: LazyList[(Puzzle, List[Move])] =
    from(LazyList((startPuzzle, List())), Set.empty)

  /**
   * Returns a lazy list of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: LazyList[(Puzzle, List[Move])] =
    val x = pathsFromStart
    for {
      path <- pathsFromStart
      if (done(path._1))
    } yield path

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = {
    if (pathsToGoal.isEmpty) then
      Nil
    else
      pathsToGoal.head._2.reverse
  }

