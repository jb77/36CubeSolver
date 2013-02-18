package cube36

class Board(val placedPieces: IndexedSeq[IndexedSeq[Option[Piece]]] /*rows of cols*/ ) {
  
  val row1 = List[Int](1, 3, 4, 5, 2, 0)
  val row2 = List[Int](2, 5, 0, 4, 1, 3)
  val row3 = List[Int](0, 1, 3, 2, 5, 4)
  val row4 = List[Int](5, 4, 1, 3, 0, 2)
  val row5 = List[Int](4, 2, 5, 0, 3, 1)
  val row6 = List[Int](3, 0, 2, 1, 4, 5)
  val contours = List[List[Int]](row1, row2, row3, row4, row5, row6)

  def spaces: Seq[(Int, Int)] = {  
    (for (row <- 0 to 5; col <- 0 to 5;if(placedPieces(row)(col)).isEmpty) yield (row, col))
  }

  def suitable(space: (Int, Int), piece: Piece): Boolean = {
    val spaceEmpty=placedPieces(space._1)(space._2).isEmpty     
    if(!spaceEmpty) return false;

    val height = contours(space._1)(space._2)
    val rightHeight=height + piece.size == 6     
    if(!rightHeight) return false;

    val colour = piece.colour
    val cl = placedPieces.map(row => row(space._2))
    val matches = cl.map(x => x.map(p => p.colour == colour))
    val colourAlreadyInRow = matches.contains(Some(true))    
    if(colourAlreadyInRow) return false;
    
    val rw = placedPieces(space._1)
    val rmatches = rw.map(x => x.map(p => p.colour == colour))
    val rcolourAlreadyInRow = rmatches.contains(Some(true))    
    if(rcolourAlreadyInRow) return false;

    true
  }

  def addPiece(rowNum: Int, colNum: Int, piece: Piece,checkSuitable:Boolean=false): Board = {
    if(checkSuitable && !suitable((rowNum,colNum),piece)) throw new IllegalArgumentException("Not suitable for this poition")
      
    val newRow=placedPieces(rowNum).updated(colNum, Some(piece))
    new Board(placedPieces.updated(rowNum, newRow))
  }

  def print:Unit={
    println("============")
    for(row<-placedPieces) {
      for(piece<-row) {
        piece match {
          case Some(p) => System.out.print(" " + p + " ")
          case None => System.out.print(" _ ")
        }    	  
      }
      println
    }
    println("============")
  }
  
}

object Board {
  def apply(): Board = {
    // Have to put two pieces in 'special' positions, where they wouldn't be expected to fit.
    // No solution otherwise.
    val bRow = IndexedSeq[Option[Piece]](None, None, Some(Piece(Yellow, 5)), None, None, None)
    val dRow = IndexedSeq[Option[Piece]](None, None, Some(Piece(Orange, 6)), None, None, None)
    val emptyRow = IndexedSeq[Option[Piece]](None, None, None, None, None, None)
    val emptyPieces = IndexedSeq(emptyRow, bRow, emptyRow, dRow, emptyRow, emptyRow)
    new Board(emptyPieces)
  }
}  