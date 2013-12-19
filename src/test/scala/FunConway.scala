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
    assert(getCandidateCells(initialCells).toSet === expectedCells.toSet)
  }



  def neighbors(cell: Cell) = cell match {
    case (x, y) =>
      for {
        nx <- (x - 1) to (x + 1)
        ny <- (y - 1) to (y + 1)
        if !(nx == x && ny == y)
      } yield (nx, ny)
  }

  def getCandidateCells(cells: List[Cell]) = {
    cells.toSet.flatMap((cell: Cell) => neighbors(cell)) ++ cells
  }

}

class FunConway {

}
