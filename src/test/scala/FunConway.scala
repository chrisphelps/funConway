package test.scala

import org.scalatest._

class FunConwaySpec extends FlatSpec {
  type Cell = (Int, Int)


  "FunctionalConway" should "generate neighbors for a cell" in {
    val initialCell = (1,1)
    val expectedCells = List((0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2))
    assert(neighbors(initialCell).toSet === expectedCells.toSet)

  }


  def neighbors(cell: Cell) = cell match {
    case (x, y) =>
      for {
        nx <- (x - 1) to (x + 1)
        ny <- (y - 1) to (y + 1)
        if !(nx == x && ny == y)
      } yield (nx, ny)
  }

}

class FunConway {

}
