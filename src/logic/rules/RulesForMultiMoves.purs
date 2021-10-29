module RulesForMultiMoves(moves,attacks) where
import Domain
import Data.Maybe
import Data.Array
import Data.Tuple
import Prelude
import Debug
import Data.Newtype

moves :: Board -> Position -> Array (Tuple Int Int) -> Array { position :: Position, moveType :: MoveType }
moves board position combinations= do
    tuple <- combinations
    let one = trace (show $ moves' board position (fst tuple) (snd tuple)) \_ -> 1
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
        nextPosition = movePosition position (Tuple fileDifference rankDifference)
        nextTile = map (\p -> tileAt p board) nextPosition
    in
        if maybe false hasPiece nextTile then [] else
                case nextPosition of
                    Nothing -> trace ("Nothing" <> show nextPosition) \_ ->[]
                    Just next -> trace ("Here" <> show nextPosition) \_ -> cons next $ moves' board next rankDifference fileDifference

attack :: Board -> Position -> Int -> Int -> PieceColor -> Maybe Position
attack board position rankDifference fileDifference color =
        case (movePosition position (Tuple fileDifference rankDifference)) of
            Just next ->
                let
                    (Tile tile) = tileAt next board
                in
                    case (tile.currentPiece) of
                       Just (Piece piece) -> if piece.color /= color then Just next else Nothing
                       Nothing -> attack board next rankDifference fileDifference color

            Nothing -> Nothing

toArray :: forall a. Maybe a -> Array a
toArray = case _ of
    Nothing -> []
    Just x -> [x]
