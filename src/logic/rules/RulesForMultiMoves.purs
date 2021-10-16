module RulesForMultiMoves(moves,attacks) where
import Domain
import Data.Maybe
import Data.Array
import Data.Tuple
import Prelude


moves :: Board -> Position -> Array (Tuple Int Int) -> Array { position :: Position, moveType :: MoveType }
moves board position combinations= do
    tuple <- combinations
    pos <- moves' board position (fst tuple) (snd tuple)
    pure $ { position: pos, moveType: Normal}

attacks :: Board -> Position -> Array (Tuple Int Int) -> Array { position :: Position, moveType :: MoveType }
attacks board position combinations = do
    (Piece piece ) <- toArray $ pieceAt position board
    tuple <- combinations
    pos <- toArray $ attack board  position (fst tuple) (snd tuple) piece.color
    pure $ { position: pos, moveType: Normal}


moves' :: Board -> Position -> Int -> Int -> Array Position
moves' board position rankDifference fileDifference =
    let
        tile = tileAt position board
    in
        if hasPiece tile then [] else
            let
                nextPosition = movePosition position (Tuple rankDifference fileDifference)
            in
                case nextPosition of
                    Nothing -> []
                    Just next -> cons next $ moves' board next rankDifference fileDifference

attack :: Board -> Position -> Int -> Int -> PieceColor -> Maybe Position
attack board position rankDifference fileDifference color =
        case (movePosition position (Tuple fileDifference rankDifference)) of
            Just next ->
                let
                    (Tile tile) = tileAt position board
                in
                    case (tile.currentPiece) of
                       Just (Piece piece) -> if piece.color /= color then Just next else Nothing
                       Nothing -> attack board next rankDifference fileDifference color

            Nothing -> Nothing

toArray :: forall a. Maybe a -> Array a
toArray = case _ of
    Nothing -> []
    Just x -> [x]
