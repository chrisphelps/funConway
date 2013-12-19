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
}

class FunConway {

}
