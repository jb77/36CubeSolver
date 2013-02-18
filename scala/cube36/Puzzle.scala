package cube36

sealed trait Colour;
case object Red extends Colour;
case object Purple extends Colour;
case object Blue extends Colour;
case object Green extends Colour;
case object Yellow extends Colour;
case object Orange extends Colour;


case class Piece(val colour: Colour, val size: Int) {
  require(size<=6 && size>=1)
  
  override def toString: String = colour.toString().head.toString + size.toString  
}


class CubePuzzle(val board: Board, val availablePieces: List[Piece]) {

  def addPiece(rowNum: Int, colNum: Int, piece: Piece): CubePuzzle = {
    val newBoard = board.addPiece(rowNum, colNum, piece)
    new CubePuzzle(newBoard, availablePieces.filterNot(_ == piece))
  }

  def solve(solutionsSoFar: List[Board]): List[Board] = {
    if (this.availablePieces.size == 0) { this.board :: solutionsSoFar }
    else {
      val nextPiece = availablePieces.head
      val availableSpots = board.spaces.filter(space => board.suitable(space, nextPiece))
      val sols = for (spot <- availableSpots) yield addPiece(spot._1, spot._2, nextPiece).solve(solutionsSoFar)
      sols.flatten.toList
    }
  }

}


object CubePuzzle {

  val startingBoard = Board()
  val availablePieces = (List[Piece]() ++
    getPieces(Yellow, 6) ++
    getPieces(Red, 6) ++
    getPieces(Purple, 6) ++
    getPieces(Blue, 6) ++
    getPieces(Green, 6) ++
    getPieces(Orange, 6)).
    filterNot(_ == Piece(Yellow, 5)).
    filterNot(_ == Piece(Orange, 6))

  def getPieces(colour: Colour, maxSize: Int): Set[Piece] = (for (x <- 1 to 6) yield Piece(colour, x)).toSet
    
  def apply() = new CubePuzzle(startingBoard, availablePieces)
  
}