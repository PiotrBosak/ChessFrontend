module Game where
import Domain
import Rules
import Data.Tuple
import Data.Map as M
import Control.MonadZero
import CheckAndMateRules
import Data.Array
import MaybeUtils as MU
import Data.Newtype
import Data.Maybe
import Prelude

data Turn = WhiteTurn | BlackTurn
derive instance eqTurn :: Eq Turn

newtype Game = Game { previousBoard :: Maybe Board
            , currentBoard :: Board
            , turn :: Turn
            }
derive instance newtypeGame :: Newtype Game _

move :: Position -> Position -> Game -> Maybe Game
move from to (Game game) =
   do
      let currentBoard = game.currentBoard
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
       let updatedTileTo = Tile $ tileTo {currentPiece = Just attackingPiece }
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
        let tileAfterKing = Tile $ oldTileFrom { currentPiece = Nothing }
        let tileAfterRook = Tile $ oldRookTile { currentPiece = Nothing }
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
        let difference = if attackingPiece.color == WhitePiece then 1 else -1
        let newTileFrom = Tile $ attackingTile {currentPiece = Nothing, numberOfMoves = attackingTile.numberOfMoves + 1 }
        let newTileTo = Tile $ attackedTile { currentPiece = Just (Piece attackingPiece) }
        capturedPosition <- movePosition move.to (Tuple 0 difference)
        let (Tile capturedTile) = tileAt capturedPosition board
        let tileAfterCapturing = Tile $ capturedTile { currentPiece = Nothing }
        pure $ (updateBoard newTileFrom (wrap move) >>> updateBoard newTileTo (wrap move) >>> updateBoard tileAfterCapturing (wrap move)) board

updateBoard :: Tile -> Move -> Board -> Board
updateBoard tile move board =
    let afterFirst = M.alter (\_ -> Just tile) (position tile) board.tiles
    in board { tiles = afterFirst, previousMove = Just move }
