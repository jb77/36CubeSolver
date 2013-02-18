package cube36

/**
 * An immutable class representing the state of a 36 Cube puzzle
 */
case class CubePuzzle(val board: Board, val availablePieces: List[Piece]) {

  /**
   * Add a piece to this puzzle, to create new instance of the puzzle
   * @param rowNum row to add piece to
   * @param colNum column to add piece to
   * @param piece piece to add
   * @param checkSuitable check if the position will accept this piece (default true if omitted)
   * @return a new puzzle instance, with the specified piece added
   */
  def addPiece(rowNum: Int, colNum: Int, piece: Piece, checkSuitable: Boolean = true): CubePuzzle = {
    val newBoard = board.addPiece(rowNum, colNum, piece,checkSuitable)
    CubePuzzle(newBoard, availablePieces.filterNot(_ == piece))
  }

  /**
   * Find solutions to the puzzle which satisfy the rules
   * @param solutionsSoFar
   * @return a list of all valid solutions
   */
  def solve(solutionsSoFar: List[Board]=List[Board]()): List[Board] = {
    if (this.availablePieces.size == 0) { this.board :: solutionsSoFar }
    else {
      val nextPiece = availablePieces.head
      val availableSpots = board.spaces.filter(space => board.suitable(space, nextPiece))
      val sols = for (spot <- availableSpots) yield addPiece(spot._1, spot._2, nextPiece,false).solve(solutionsSoFar)
      sols.flatten.toList
    }
  }

}

object CubePuzzle {
  // Have to put two pieces in 'special' positions, where they wouldn't be expected to fit.
  // No solution otherwise.
  private val startingBoard = Board().addPiece(1, 2, Piece(Yellow, 5),false).addPiece(3, 2, Piece(Orange, 6),false)
  private val availablePieces = (List[Piece]() ++
    getPieces(Yellow, 6) ++
    getPieces(Red, 6) ++
    getPieces(Purple, 6) ++
    getPieces(Blue, 6) ++
    getPieces(Green, 6) ++
    getPieces(Orange, 6)).
    filterNot(_ == Piece(Yellow, 5)).
    filterNot(_ == Piece(Orange, 6))

  private def getPieces(colour: Colour, maxSize: Int): Set[Piece] = (for (x <- 1 to 6) yield Piece(colour, x)).toSet

  def apply() = new CubePuzzle(startingBoard, availablePieces)

}

sealed trait Colour;
case object Red extends Colour;
case object Purple extends Colour;
case object Blue extends Colour;
case object Green extends Colour;
case object Yellow extends Colour;
case object Orange extends Colour;

case class Piece(val colour: Colour, val size: Int) {
  require(size <= 6 && size >= 1,"Piece size out of range")

  override def toString: String = colour.toString().head.toString + size.toString
}