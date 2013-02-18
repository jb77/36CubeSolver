package cube36

import org.junit.Assert._
import org.junit.Test

class Tests {

  @Test
  def checkBoard {
    val p = CubePuzzle()
    assertTrue(p.board.row1Heights.sum == 15)
    assertTrue(p.board.row2Heights.sum == 15)
    assertTrue(p.board.row3Heights.sum == 15)
    assertTrue(p.board.row4Heights.sum == 15)
    assertTrue(p.board.row5Heights.sum == 15)
    assertTrue(p.board.row6Heights.sum == 15)

    val rowsOfcols = p.board.contours
    assertTrue(rowsOfcols.map(row => row(0)).sum == 15)
    assertTrue(rowsOfcols.map(row => row(1)).sum == 15)
    assertTrue(rowsOfcols.map(row => row(2)).sum == 15)
    assertTrue(rowsOfcols.map(row => row(3)).sum == 15)
    assertTrue(rowsOfcols.map(row => row(4)).sum == 15)
    assertTrue(rowsOfcols.map(row => row(5)).sum == 15)
    
    val nums = p.board.contours.flatten
    assertTrue(nums.sum == 90)
    assertTrue(nums.count(_ == 0) == 6)
    assertTrue(nums.count(_ == 1) == 6)
    assertTrue(nums.count(_ == 2) == 6)
    assertTrue(nums.count(_ == 3) == 6)
    assertTrue(nums.count(_ == 4) == 6)
    assertTrue(nums.count(_ == 5) == 6)    
  }

  @Test
  def testCorrectHeight {
    val p = CubePuzzle()
    val suitable = p.board.suitable((0, 0), Piece(Green, 5))
    assertTrue(suitable)
  }

  @Test
  def testWrongHeight {
    val p = CubePuzzle()
    val suitable = p.board.suitable((0, 0), Piece(Yellow, 4))
    assertFalse(suitable)
  }

  @Test
  def testColClash {
    val p = CubePuzzle()
    val p2 = p.addPiece(0, 0, Piece(Yellow, 5))
    val suitable = p2.board.suitable((1, 0), Piece(Yellow, 4))
    assertFalse(suitable)
  }

  @Test
  def testNoClash {
    val p = CubePuzzle()
    val p2 = p.addPiece(0, 0, Piece(Yellow, 5))
    val suitable = p2.board.suitable((1, 0), Piece(Red, 4))
    assertTrue(suitable)
  }

  @Test
  def testRowClash {
    val p = CubePuzzle()
    val p2 = p.addPiece(0, 0, Piece(Yellow, 5))
    val suitable = p2.board.suitable((0, 1), Piece(Yellow, 3))
    assertFalse(suitable)
  }

  @Test
  def testSpaces {
    val p = CubePuzzle()
    val p2 = p.addPiece(0, 0, Piece(Green, 5))
    val p3 = p2.addPiece(1, 0, Piece(Red, 4))
    val p4 = p3.addPiece(5, 5, Piece(Red, 1))
    assertTrue(p4.board.spaces.size == 31)
  }

  @Test
  def testSolve {
    val p = CubePuzzle()
    val solutions = p.solve()
    assertTrue(solutions.size == 96)
  }

}