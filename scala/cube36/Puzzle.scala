package cube36

sealed trait Colour;
case object Red extends Colour;
case object Purple extends Colour;
case object Blue extends Colour;
case object Green extends Colour;
case object Yellow extends Colour;
case object Orange extends Colour;

case class Piece(val colour: Colour, val size: Int)

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
  
  def qSolve = {
   /* val startingPuzzle=this.addPiece(0, 0, Piece(Yellow,5)).
    	addPiece(0, 1, Piece(Red,3)).
    	addPiece(0, 2, Piece(Green,2)).
    	addPiece(0, 3, Piece(Blue,1)).
    	addPiece(0, 4, Piece(Purple,4)).
    	addPiece(0, 5, Piece(Orange,6))*/
    
    val startingPuzzle=this.addPiece(0, 5, Piece(Yellow,6)).
    	addPiece(1, 2, Piece(Red,6)).
    	addPiece(2, 0, Piece(Green,6)).
    	addPiece(3, 4, Piece(Blue,6)).
    	addPiece(4, 3, Piece(Purple,6)).
    	addPiece(5, 1, Piece(Orange,6))  
    	
    	startingPuzzle.solve
  }

  def solve: List[CubePuzzle] = {
    		      
    if(this.availablePieces.size==0) {println(this.board)}//; System.exit(1)}
      
    val spaceToPossPiecesMap=availablePieces.groupBy(piece=>board.spaces.filter(space=>board.suitable(space,piece)))
    val unplayableSquareExists=spaceToPossPiecesMap.exists(p=>p._2.isEmpty || p._1.isEmpty)
    if(unplayableSquareExists) return List()
    
    
    CheatCubePuzzle.solvingCount=CheatCubePuzzle.solvingCount+1
    if(CheatCubePuzzle.minSoFar>availablePieces.size) {
      CheatCubePuzzle.minSoFar=availablePieces.size
      println(this.board)
    }
    if(CheatCubePuzzle.solvingCount % 1000 ==0) println(CheatCubePuzzle.solvingCount + " : " + CheatCubePuzzle.minSoFar + " , " + availablePieces.size)

    val nextPuzzles = for ( space <- spaceToPossPiecesMap.keys; piece <-spaceToPossPiecesMap.get(space).get  ) yield softAddPiece(space.head._1, space.head._2, piece)
    //val nextPuzzlesOrig = for (piece <- availablePieces; space <- board.spaces; if(board.suitable(space,piece)) ) yield softAddPiece(space._1, space._2, piece)
    
    val solution=nextPuzzles.flatten.find(p => p.availablePieces.isEmpty)
    if(solution.isDefined) {println(solution); List(solution.get)}
    else nextPuzzles.flatten.map(p => p.solve).flatten.toList

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