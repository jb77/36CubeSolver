package cube36

sealed trait Colour;
case object Red extends Colour;
case object Purple extends Colour;
case object Blue extends Colour;
case object Green extends Colour;
case object Yellow extends Colour;
case object Orange extends Colour;

case class Piece(val colour: Colour, val size: Int) {
  
  override def toString:String = {
    colour.toString().head.toString + size.toString    
  }
  
}

class CubePuzzle(val board: Board, val availablePieces: Set[Piece]) {

  def addPiece(rowNum: Int, colNum: Int, piece: Piece): CubePuzzle = {
    val newBoard = board.addPiece(rowNum, colNum, piece)
    new CubePuzzle(newBoard, availablePieces - piece)
  }

  def softAddPiece(rowNum: Int, colNum: Int, piece: Piece): Option[CubePuzzle] = {
    try {
    	val newBoard = board.addPiece(rowNum, colNum, piece)
    	Some(new CubePuzzle(newBoard, availablePieces - piece))
    }
    catch {
        case _:Exception => None
    }
    
  }

  override def toString: String = {
    board.placedPieces.toString + "\n\n" + availablePieces
  }
  
  
  def solve(solutionsSoFar:List[Board]):List[Board] = {    
    if(this.availablePieces.size==0) {(this.board.print);this.board::solutionsSoFar}
    else {
      val nextPiece=availablePieces.head
      val availableSpots=board.spaces.filter(space=>board.suitable(space,nextPiece))
      val sols=for(spot<-availableSpots) yield softAddPiece(spot._1, spot._2, nextPiece).get.solve(solutionsSoFar)
      sols.flatten.toList      
    }    
  }

}

object CubePuzzle {
  val startingBoard = Board()
  val availablePieces = Set[Piece]() ++
    getPieces(Yellow, 6) ++
    getPieces(Red, 6) ++
    getPieces(Purple, 6) ++
    getPieces(Blue, 6) ++
    getPieces(Green, 6) ++
    getPieces(Orange, 6)

  def getPieces(colour: Colour, maxSize: Int): Set[Piece] = {
    val p = for (x <- 1 to 6) yield Piece(colour, x)
    p.toSet
  }

  def apply() = {
    new CubePuzzle(startingBoard, availablePieces)
  }

}

object CheatCubePuzzle {
  
  var solvingCount=0;
  var minSoFar=34;
  
  val startingBoard = CheatBoard()
  val availablePieces = Set[Piece]() ++
    getPieces(Yellow, 6) ++
    getPieces(Red, 6) ++
    getPieces(Purple, 6) ++
    getPieces(Blue, 6) ++
    getPieces(Green, 6) ++
    getPieces(Orange, 6)

  def getPieces(colour: Colour, maxSize: Int): Set[Piece] = {
    val p = for (x <- 1 to 6) yield Piece(colour, x)
    p.toSet
  }

  def apply() = {
    new CubePuzzle(startingBoard, (availablePieces-Piece(Yellow,5))-Piece(Orange,6))
  }

}