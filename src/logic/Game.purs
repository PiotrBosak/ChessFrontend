module Game  where
import Domain
import Debug
import Rules
import Data.Tuple
import Data.Map as M
import Control.MonadZero
import CheckRules
import Data.Array
import Data.Newtype
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import BoardFactory
import Data.Maybe
import Prelude

data Turn = WhiteTurn | BlackTurn
derive instance eqTurn :: Eq Turn
derive instance genericTurn :: Generic Turn _
instance showTurn :: Show Turn where
  show = genericShow

newtype Game = Game { previousBoard :: Maybe Board
            , currentBoard :: Board
            , turn :: Turn
            , gameStatus :: GameStatus
            }

makeGame :: Game
makeGame = Game { previousBoard: Nothing
                , currentBoard: createBoard
                , turn: WhiteTurn
                , gameStatus: Cont
                }
derive instance newtypeGame :: Newtype Game _
instance showGame :: Show Game where
    show (Game g) = show g.turn

data GameStatus = Cont | Finished | Checking
derive instance eqGameStatus :: Eq GameStatus
derive instance genericGameStatus :: Generic GameStatus _
instance showGameStatus :: Show GameStatus where
    show = genericShow

isKingMated :: Game -> PieceColor -> Boolean
isKingMated (Game game) kingColor =
    isKingChecked (game.currentBoard) kingColor && cannotBeDefended (wrap game) kingColor

updateCheckOrMate :: Game -> Game
updateCheckOrMate (Game game) =
    if game.gameStatus == Checking
    then
        let kingMate = isKingMated (wrap game) (if game.turn == WhiteTurn then WhitePiece else BlackPiece )
            (Game newGame) = Game $ game {gameStatus = if kingMate then Finished else Cont }
        in (wrap newGame)
    else (wrap game)

move :: Position -> Position -> Game -> Maybe Game
move from to (Game game) =
   do
      guard $ game.gameStatus /= Finished
      let currentBoard = game.currentBoard
      let (Tile tileFromTest) = tileAt from currentBoard
      (Piece tileFromPiece) <- tileFromTest.currentPiece
      guard $ if game.turn == WhiteTurn then tileFromPiece.color == WhitePiece else tileFromPiece.color == BlackPiece
      let moves = allMoves currentBoard from
      (Tile tileTo) <- Just $ tileAt to currentBoard
      {position: position, moveType: moveType} <- find (\r -> tileTo.position == r.position) moves
      let (Tile tileFrom) = tileAt from currentBoard
      (Piece attackingPiece) <- tileFrom.currentPiece
      guard $ if attackingPiece.color == WhitePiece then game.turn == WhiteTurn else game.turn == BlackTurn
      let move = Move { from: tileFrom.position
                      , to: tileTo.position
                      , moveType: moveType
                      }

      newBoard <- boardAfterMove move (wrap game)
      let newTurn = if game.turn == WhiteTurn then BlackTurn else WhiteTurn
      pure $ Game { previousBoard: Just currentBoard
             , currentBoard: newBoard
             , turn: newTurn
             , gameStatus: Checking
             }

moveNoTurn :: Position -> Position -> Game -> Maybe Game
moveNoTurn from to (Game game) =
   do
      let currentBoard = game.currentBoard
      let moves = allMoves currentBoard from
      (Tile tileTo) <- Just $ tileAt to currentBoard
      {position: position, moveType: moveType} <- find (\r -> tileTo.position == r.position) moves
      let (Tile tileFrom) = tileAt from currentBoard
      let move = Move { from: tileFrom.position
                      , to: tileTo.position
                      , moveType: moveType
                      }

      newBoard <- boardAfterMove move (wrap game)
      let newTurn = if game.turn == WhiteTurn then BlackTurn else WhiteTurn
      pure $ Game { previousBoard: Just currentBoard
             , currentBoard: newBoard
             , turn: newTurn
             , gameStatus: Cont
             }

boardAfterMove :: Move -> Game -> Maybe Board
boardAfterMove (Move move) game =
    do
        board <- case move.moveType of
                    Normal -> normalMove (wrap move) game
                    TwoTileMove -> normalMove (wrap move) game
                    Castling -> castlingMove (wrap move) game
                    LePassant -> lePassantMove (wrap move) game

        let (Game g) = game
        let currentBoard = g.currentBoard
        let (Tile t) = tileAt move.from currentBoard
        (Piece piece) <- t.currentPiece
        guard $ not $ isKingChecked board piece.color
        pure $ board

normalMove :: Move -> Game -> Maybe Board
normalMove (Move move) (Game game) =
    do
       let (Position fromPosition ) = move.from
       let board = game.currentBoard
       let (Tile tileFrom) = tileAt move.from board
       let (Tile tileTo) = tileAt move.to board
       attackingPiece <- tileFrom.currentPiece
       let updatedTileFrom = Tile $ tileFrom {currentPiece = Nothing,numberOfMoves = tileFrom.numberOfMoves + 1}
       let updatedTileTo = Tile $ tileTo {currentPiece = Just attackingPiece ,numberOfMoves = tileTo.numberOfMoves + 1}
       pure $ (updateBoard updatedTileFrom (wrap move) >>> updateBoard updatedTileTo (wrap move)) board

castlingMove :: Move -> Game -> Maybe Board
castlingMove (Move move) (Game game) =
    do
        let (Position positionTo) = move.to
        let (Position positionFrom) = move.from
        let currentBoard = game.currentBoard
        let (Tile oldTileTo) = tileAt (wrap positionTo) currentBoard
        let (Tile oldTileFrom) = tileAt (wrap positionFrom) currentBoard
        let kingNewFile = positionTo.file
        let rookOldFile = if kingNewFile == B then A else H
        let rookNewFile = if kingNewFile == B then C else F
        let (Tile oldRookTile) = tileAt (Position positionFrom {file = rookOldFile }) currentBoard
        let tileAfterKing = Tile $ oldTileFrom { currentPiece = Nothing, numberOfMoves = oldTileFrom.numberOfMoves + 1 }
        let tileAfterRook = Tile $ oldRookTile { currentPiece = Nothing , numberOfMoves = oldRookTile.numberOfMoves + 1 }
        kingPiece <- oldTileFrom.currentPiece
        rookPiece <- oldRookTile.currentPiece
        let kingNewTile = Tile $ oldTileFrom { currentPiece = Just kingPiece, numberOfMoves = oldTileFrom.numberOfMoves + 1}
        let newRookPosition = positionFrom { file = rookNewFile }
        let (Tile tileForRook) = tileAt (wrap newRookPosition) currentBoard
        let newRookTile = Tile $ tileForRook { currentPiece = Just rookPiece, numberOfMoves = tileForRook.numberOfMoves + 1}
        pure ( (updateBoard tileAfterKing (wrap move)
        >>>  updateBoard kingNewTile (wrap move) >>> updateBoard (wrap oldRookTile) (wrap move) >>> updateBoard newRookTile (wrap move)) currentBoard)



lePassantMove :: Move -> Game -> Maybe Board
lePassantMove (Move move) (Game game) =
    do
        let board = game.currentBoard
        let (Tile attackingTile) = tileAt move.from board
        let (Tile attackedTile) = tileAt move.to board
        (Piece attackingPiece) <- attackingTile.currentPiece
        let difference = if attackingPiece.color == WhitePiece then -1 else 1
        let newTileFrom = Tile $ attackingTile {currentPiece = Nothing, numberOfMoves = attackingTile.numberOfMoves + 1 }
        let newTileTo = Tile $ attackedTile { currentPiece = Just (Piece attackingPiece), numberOfMoves = attackedTile.numberOfMoves + 1}
        capturedPosition <- movePosition move.to (Tuple 0 difference)
        let (Tile capturedTile) = tileAt capturedPosition board
        let tileAfterCapturing = Tile $ capturedTile { currentPiece = Nothing }
        pure $ (updateBoard newTileFrom (wrap move) >>> updateBoard newTileTo (wrap move) >>> updateBoard tileAfterCapturing (wrap move)) board

updateBoard :: Tile -> Move -> Board -> Board
updateBoard tile move board =
    let afterFirst = M.alter (\_ -> Just tile) (position tile) board.tiles
    in board { tiles = afterFirst, previousMove = Just move }

cannotBeDefended :: Game -> PieceColor -> Boolean
cannotBeDefended (Game game) color =
       let board = game.currentBoard
           tiles = fromFoldable $ M.values board.tiles
           tilesWithPieces = mapMaybe (tileWithPiece color) tiles
           allScenarios = do
               move <- map (allPossibleMoves board) tilesWithPieces
               let from = move.from
               to <- move.to
               maybe [] (\x -> [x]) $ moveNoTurn from to (wrap game)
       in all (\(Game game) -> isKingChecked game.currentBoard color) allScenarios


allPossibleMoves :: Board -> Tile -> {from:: Position, to :: Array Position}
allPossibleMoves board (Tile tile) =
    {from: tile.position,to : map (_.position) $ allMoves board tile.position}

tileWithPiece :: PieceColor -> Tile -> Maybe Tile
tileWithPiece color (Tile tile) =
  do
    (Piece piece) <- tile.currentPiece
    guard $  piece.color == color
    pure $ wrap tile
