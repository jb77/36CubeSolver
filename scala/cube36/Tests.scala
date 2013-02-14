package cube36

import org.junit._
import org.junit.rules.ExpectedException
import org.junit.Assert._

class Tests {
  
  @Test
  def checkBoard {
    val p=CubePuzzle()
    Assert.assertTrue(p.board.row1.sum==15)
    Assert.assertTrue(p.board.row2.sum==15)
    Assert.assertTrue(p.board.row3.sum==15)
    Assert.assertTrue(p.board.row4.sum==15)
    Assert.assertTrue(p.board.row5.sum==15)
    Assert.assertTrue(p.board.row6.sum==15)
    
    val nums=p.board.contours.flatten
    assertTrue(nums.sum==90)
    assertTrue(nums.count(n=>n==0)==6)
    assertTrue(nums.count(n=>n==1)==6)
    assertTrue(nums.count(n=>n==2)==6)
    assertTrue(nums.count(n=>n==3)==6)
    assertTrue(nums.count(n=>n==4)==6)
    assertTrue(nums.count(n=>n==5)==6)
    
    val c=p.board.contours
    assertTrue(c.map(row=>row(0)).sum==15)
    assertTrue(c.map(row=>row(1)).sum==15)
    assertTrue(c.map(row=>row(2)).sum==15)
    assertTrue(c.map(row=>row(3)).sum==15)
    assertTrue(c.map(row=>row(4)).sum==15)
    assertTrue(c.map(row=>row(5)).sum==15)    
  }
  
  @Test
  def testCorrectHeight {
    val p=CubePuzzle()
    p.board.addPiece(0, 0, Piece(Yellow,5))
  }
  
  @Test(expected=classOf[IllegalArgumentException])
  def testWrongHeight {
    val p=CubePuzzle()
    p.addPiece(0, 0, Piece(Yellow,4))
  }
  
  @Test(expected=classOf[IllegalArgumentException])
  def testColClash {
    val p=CubePuzzle()
    val p2=p.addPiece(0, 0, Piece(Yellow,5))
    p2.addPiece(1, 0, Piece(Yellow,4))
  } 
  
  @Test
  def testNoClash {
    val p=CubePuzzle()
    val p2=p.addPiece(0, 0, Piece(Yellow,5))
    p2.addPiece(1, 0, Piece(Red,4))
  } 
  
  @Test(expected=classOf[IllegalArgumentException])
  def testRowClash {
    val p=CubePuzzle()
    val p2=p.addPiece(0, 0, Piece(Yellow,5))
    p2.addPiece(0, 1, Piece(Yellow,3))
  }  
  
  @Test
  def testSpaces {
    val p=CubePuzzle()
    val p2=p.addPiece(0, 0, Piece(Yellow,5))
    val p3=p2.addPiece(1, 0, Piece(Red,4))
    val p4=p3.addPiece(5, 5, Piece(Red,1))
    Assert.assertTrue(p4.board.spaces.size==33)
  }   
  
  @Test
  def testSolve {
    val p=CheatCubePuzzle()
    val solution=p.solve
    println(solution)
    
  }

}