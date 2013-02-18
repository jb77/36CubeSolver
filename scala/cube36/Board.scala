package cube36

class Board(val placedPieces: IndexedSeq[IndexedSeq[Option[Piece]]] /*rows of cols*/ ) {

  val row1 = List[Int](1, 3, 4, 5, 2, 0)
  val row2 = List[Int](2, 5, 0, 4, 1, 3)
  val row3 = List[Int](0, 1, 3, 2, 5, 4)
  val row4 = List[Int](5, 4, 1, 3, 0, 2)
  val row5 = List[Int](4, 2, 5, 0, 3, 1)
  val row6 = List[Int](3, 0, 2, 1, 4, 5)
  val contours = List[List[Int]](row1, row2, row3, row4, row5, row6)

  private[cube36] def spaces: Seq[(Int, Int)] = for (row <- 0 to 5; col <- 0 to 5; if (placedPieces(row)(col)).isEmpty) yield (row, col)

  private[cube36] def suitable(space: (Int, Int), piece: Piece): Boolean = {
    lazy val spaceEmpty = placedPieces(space._1)(space._2).isEmpty

    lazy val height = contours(space._1)(space._2)
    lazy val rightHeight = height + piece.size == 6

    lazy val colour = piece.colour
    lazy val cl = placedPieces.map(row => row(space._2))
    lazy val matches = cl.map(x => x.map(p => p.colour == colour))
    lazy val colourAlreadyInCol = matches.contains(Some(true))

    lazy val rw = placedPieces(space._1)
    lazy val rmatches = rw.map(x => x.map(p => p.colour == colour))
    lazy val colourAlreadyInRow = rmatches.contains(Some(true))

    val unsuitable = (!spaceEmpty) || (!rightHeight) || colourAlreadyInCol || colourAlreadyInRow
    !unsuitable
  }

  private[cube36] def addPiece(rowNum: Int, colNum: Int, piece: Piece, checkSuitable: Boolean = true): Board = {
    if (checkSuitable && !suitable((rowNum, colNum), piece)) throw new IllegalArgumentException("Not suitable for this poition")

    val newRow = placedPieces(rowNum).updated(colNum, Some(piece))
    new Board(placedPieces.updated(rowNum, newRow))
  }
  
  private[this] def rowString(row:IndexedSeq[Option[Piece]]):String = {
    val r=for(p<-row) yield p match {
          case Some(pc) => "|" + pc 
          case None => " _ "
        }
    r.foldLeft("")((a,b)=>a+b) + "|"
  }

  override def toString: String = {
    val border = "==================="
    val content = for (row <- placedPieces) yield rowString(row) 
    
    val folded=content.foldLeft(border)((a,b)=> a +"\n" +  b)
    folded + "\n" + border
  }

}

object Board {
  def apply(): Board = {
    val emptyRow = IndexedSeq[Option[Piece]](None, None, None, None, None, None)
    val emptyPieces = IndexedSeq(emptyRow, emptyRow, emptyRow, emptyRow, emptyRow, emptyRow)
    new Board(emptyPieces)
  }
}  