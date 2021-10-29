module BoardFactory (createBoard) where
import Domain
import Data.Map.Internal
import Data.Map
import Data.Maybe
import Prelude


createBoard :: Board
createBoard = {tiles: createBoardTiles, previousMove: Nothing}


createBoardTiles :: Map Position Tile
createBoardTiles = unions ([ tilesWithNoPieces
           , tilesWithBlackPawns
           , tilesWithWhitePawns
           , tilesWithBlackBishops
           , tilesWithWhiteBishops
           , tilesWithWhiteRooks
           , tilesWithBlackRooks
           , tilesWithWhiteKnights
           , tilesWithBlackKnights
           , tileWithBlackKing
           , tileWithWhiteKing
           , tileWithBlackQueen
           , tileWithWhiteQueen
           ])

tilesWithBlackBishops :: Map Position Tile
tilesWithBlackBishops =
    let  firstPosition = Position {file: C, rank: Eight }
         bishop = Piece {pieceType: Bishop, color: BlackPiece}
         secondPosition = Position {file: F, rank: Eight }
         firstTile = Tile {position: firstPosition, currentPiece: Just bishop, numberOfMoves: 0}
         secondTile = Tile {position: secondPosition, currentPiece: Just bishop, numberOfMoves: 0}
    in ((insert firstPosition firstTile) >>> (insert secondPosition secondTile)) empty

tilesWithWhiteBishops :: Map Position Tile
tilesWithWhiteBishops =
    let  firstPosition = Position {file: C, rank: One }
         bishop = Piece {pieceType: Bishop, color: WhitePiece}
         secondPosition = Position {file: F, rank: One }
         firstTile = Tile {position: firstPosition, currentPiece: Just bishop, numberOfMoves: 0}
         secondTile = Tile {position: secondPosition, currentPiece: Just bishop, numberOfMoves: 0}
    in ((insert firstPosition firstTile) >>> (insert secondPosition secondTile)) empty

tilesWithWhiteKnights :: Map Position Tile
tilesWithWhiteKnights =
    let  firstPosition = Position {file: B, rank: One }
         knight = Piece {pieceType: Knight, color: WhitePiece}
         secondPosition = Position {file: G, rank: One }
         firstTile = Tile {position: firstPosition, currentPiece: Just knight, numberOfMoves: 0}
         secondTile = Tile {position: secondPosition, currentPiece: Just knight, numberOfMoves: 0}
    in ((insert firstPosition firstTile) >>> (insert secondPosition secondTile)) empty

tilesWithBlackKnights :: Map Position Tile
tilesWithBlackKnights =
    let  firstPosition = Position {file: B, rank: Eight }
         knight = Piece {pieceType: Knight, color: BlackPiece}
         secondPosition = Position {file: G, rank: Eight }
         firstTile = Tile {position: firstPosition, currentPiece: Just knight, numberOfMoves: 0}
         secondTile = Tile {position: secondPosition, currentPiece: Just knight, numberOfMoves: 0}
    in ((insert firstPosition firstTile) >>> (insert secondPosition secondTile)) empty

tilesWithBlackRooks :: Map Position Tile
tilesWithBlackRooks =
    let  firstPosition = Position {file: A, rank: Eight }
         rook = Piece {pieceType: Rook, color: BlackPiece}
         secondPosition = Position {file: H, rank: Eight }
         firstTile = Tile {position: firstPosition, currentPiece: Just rook, numberOfMoves: 0}
         secondTile = Tile {position: secondPosition, currentPiece: Just rook, numberOfMoves: 0}
    in ((insert firstPosition firstTile) >>> (insert secondPosition secondTile)) empty

tilesWithWhiteRooks :: Map Position Tile
tilesWithWhiteRooks =
    let  firstPosition = Position {file: A, rank: One }
         rook = Piece {pieceType: Rook, color: WhitePiece}
         secondPosition = Position {file: H, rank: One }
         firstTile = Tile {position: firstPosition, currentPiece: Just rook, numberOfMoves: 0}
         secondTile = Tile {position: secondPosition, currentPiece: Just rook, numberOfMoves: 0}
    in ((insert firstPosition firstTile) >>> (insert secondPosition secondTile)) empty

tileWithWhiteKing :: Map Position Tile
tileWithWhiteKing=
    let  firstPosition = Position {file: E, rank: One }
         king = Piece {pieceType: King, color: WhitePiece}
         firstTile = Tile {position: firstPosition, currentPiece: Just king, numberOfMoves: 0}
    in insert firstPosition firstTile empty

tileWithBlackKing :: Map Position Tile
tileWithBlackKing =
    let  firstPosition = Position {file: E, rank: Eight }
         king = Piece {pieceType: King, color: BlackPiece}
         firstTile = Tile {position: firstPosition, currentPiece: Just king, numberOfMoves: 0}
    in insert firstPosition firstTile empty

tileWithBlackQueen :: Map Position Tile
tileWithBlackQueen =
    let  firstPosition = Position {file: D, rank: Eight }
         queen = Piece {pieceType: Queen, color: BlackPiece}
         firstTile = Tile {position: firstPosition, currentPiece: Just queen, numberOfMoves: 0}
    in insert firstPosition firstTile empty

tileWithWhiteQueen :: Map Position Tile
tileWithWhiteQueen =
    let  firstPosition = Position {file: D, rank: One }
         queen = Piece {pieceType: Queen, color: WhitePiece}
         firstTile = Tile {position: firstPosition, currentPiece: Just queen, numberOfMoves: 0}
    in insert firstPosition firstTile empty

tilesWithWhitePawns :: Map Position Tile
tilesWithWhitePawns = createTiles Two $ Just $ Piece {pieceType: Pawn, color: WhitePiece}

tilesWithBlackPawns :: Map Position Tile
tilesWithBlackPawns = createTiles Seven $ Just (Piece {pieceType: Pawn, color: BlackPiece})

tilesWithNoPieces :: Map Position Tile
tilesWithNoPieces = tilesWithNoPiecesAux $ Just Three

tilesWithNoPiecesAux :: Maybe Rank -> Map Position Tile
tilesWithNoPiecesAux Nothing = empty
tilesWithNoPiecesAux (Just Seven) = empty
tilesWithNoPiecesAux (Just Eight) = empty
tilesWithNoPiecesAux (Just rank) =
    union (createTiles rank Nothing) $ tilesWithNoPiecesAux (nextRank rank)

createTiles :: Rank -> Maybe Piece -> Map Position Tile
createTiles = createTilesInFile (Just A)

createTilesInFile :: Maybe File -> Rank -> Maybe Piece ->  Map Position Tile
createTilesInFile Nothing _ _ = empty
createTilesInFile (Just file) rank piece =
    let position = Position {file: file, rank: rank}
        tile = Tile {position: position, currentPiece: piece, numberOfMoves: 0}
    in  insert position tile $ createTilesInFile (nextFile file) rank piece


