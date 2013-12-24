package test.scala

import org.scalatest._

class FunConwaySpec extends FlatSpec {
  type Cell = (Int, Int)


  "FunctionalConway" should "generate neighbors for a cell" in {
    val initialCell = (1,1)
    val expectedCells = List((0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2))
    assert(neighbors(initialCell).toSet === expectedCells.toSet)

  }

  it should "generate a list of all potential cells" in {
    val initialCells = List((1,1), (1,2))
    val expectedCells = List((0,0),(0,1),(0,2),(0,3),
      (1,0),(1,1),(1,2),(1,3),
      (2,0),(2,1),(2,2), (2,3))
    assert(candidateCells(initialCells).toSet === expectedCells.toSet)
  }

  it should "count live neighbors" in {
    val liveCells = List((1,1), (1,2))
    assert(neighborCount((0,1), liveCells) === 2)
    assert(neighborCount((1,1), liveCells) === 1)
    assert(neighborCount((5,5), liveCells) === 0)
  }

  it should "augment cells with count and liveness" in {
    val liveCells = List((1,1), (1,2))
    assert(augmentCell((0,1), liveCells) === ((0,1),2,false))
    assert(augmentCell((1,1), liveCells) === ((1,1),1,true))
    assert(augmentCell((5,5), liveCells) === ((5,5),0,false))
  }

  it should "kill underpopulated cells" in {
    val cell = ((1,1), 1, true)
    assert(shouldLive(cell) === false)
  }

  it should "kill overpopulated cells" in {
    val cell = ((1,1), 4, true)
    assert(shouldLive(cell) === false)
  }

  it should "allow normal cells to live" in {
    val cell = ((1,1), 2, true)
    assert(shouldLive(cell) === true)
  }

  it should "reproduce cells" in {
    val cell = ((1,1), 3, false)
    assert(shouldLive(cell) === true)
  }

  it should "maintain the block" in {
    val liveCells = List((1,1), (1,2), (2,1), (2,2))
    assert(evolve(liveCells).toSet === liveCells.toSet)
  }

  it should "blink the blinker" in {
    val liveCells = List((1,0), (1,1), (1,2))
    val expectedCells = List((0,1), (1,1), (2,1))
    assert(evolve(liveCells).toSet === expectedCells.toSet)
  }

  it should "glide the glider" in {
    val liveCells = List((0,2),(1,0),(1,2),(2,1),(2,2))
    val expectedCells = List((0,2),(1,3),(2,1),(2,2),(2,3))
    assert(evolve(evolve(liveCells)).toSet === expectedCells.toSet)
  }

  def neighbors(cell: Cell) = cell match {
    case (x, y) =>
      for {
        nx <- (x - 1) to (x + 1)
        ny <- (y - 1) to (y + 1)
        if !(nx == x && ny == y)
      } yield (nx, ny)
  }

  def candidateCells(cells: List[Cell]) = {
    cells.toSet.flatMap((cell: Cell) => neighbors(cell)) ++ cells
  }

  def neighborCount(cell: Cell, liveCells: List[Cell]) = {
    neighbors(cell).filter(c => liveCells.contains(c)).size
  }

  def augmentCell(cell: Cell, liveCells: List[Cell]) = {
    (cell, neighborCount(cell, liveCells), liveCells.contains(cell))
  }

  def shouldLive(augmentedCell: (Cell, Int, Boolean)) = {
    augmentedCell match {
      case (c, n, true) if n < 2 => false
      case (c, n, true) if n > 3 => false
      case (c, n, true) => true
      case (c, n, false) if n == 3 => true
      case _ => false
    }
  }

  def evolve(cells: List[Cell]) = {
    val candidates = candidateCells(cells)
    val augmentedCandidates = candidates.map(c => augmentCell(c, cells))
    val newgeneration = augmentedCandidates.filter(c => shouldLive(c)).map{ac => ac._1}
    newgeneration.toList
  }
}

class FunConway {

}
