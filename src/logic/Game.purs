module Game where
import Domain
import Rules
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
        guard $ isKingChecked board piece.color
        pure $ board

normalMove :: Move -> Game -> Maybe Board
normalMove move game = Nothing

castlingMove :: Move -> Game -> Maybe Board
castlingMove move game = Nothing

lePassantMove :: Move -> Game -> Maybe Board
lePassantMove move game = Nothing




