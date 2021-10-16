module KnightAndKingRules(moves,attacks) where
import Data.Tuple
import Data.Array
import Domain
import Control.MonadZero
import Data.Maybe
import Prelude


moves :: Board -> Position -> Array (Tuple Int Int) -> Array Position
moves board position combinations =
    do
        possiblePosition <- mapMaybe (movePosition position) combinations
        guard $ isEmpty $ tileAt possiblePosition board
        pure possiblePosition

attacks :: Board -> Position -> Array (Tuple Int Int) -> Array Position
attacks board position combinations = case (pieceAt position board) of
    Just (Piece attackingPiece) -> do
     possiblePosition <- mapMaybe (movePosition position) combinations
     guard $ hasEnemyPiece board possiblePosition attackingPiece.color
     pure possiblePosition
    Nothing -> []

hasEnemyPiece :: Board -> Position -> PieceColor -> Boolean
hasEnemyPiece board position attackingColor =
    let
        differentColor = do
            let (Tile attackedTile) = tileAt position board
            (Piece piece) <- attackedTile.currentPiece
            pure $ attackingColor /= piece.color
    in
        maybe false identity differentColor




