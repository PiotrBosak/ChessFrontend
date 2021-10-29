module CheckRules(isKingChecked) where
import Domain
import Control.MonadZero
import Data.Maybe as MA
import Data.Tuple
import Rules as R
import Data.Map as M
import Data.Newtype
import Data.Array
import Rules
import Domain
import Prelude


isKingChecked :: Board -> PieceColor -> Boolean
isKingChecked board kingColor =
    let
        m = do
           (Tile tileWithKing) <- (head <<< filter (findKingTile kingColor)) $ (M.values >>> fromFoldable) board.tiles
           (Piece piece) <- tileWithKing.currentPiece
           let attacks = possibleAttacks board piece.color
           pure $ elem tileWithKing.position attacks
    in
        MA.maybe false identity m

findKingTile :: PieceColor -> Tile -> Boolean
findKingTile color (Tile tile) =
    exists (\(Piece p) -> (wrap >>> isKing) p && (p.color == color)) tile.currentPiece

isKing :: Piece -> Boolean
isKing (Piece piece) = case piece.pieceType of
    King -> true
    _ -> false

possibleAttacks :: Board -> PieceColor -> Array Position
possibleAttacks board kingColor =
    do
        let posistions = (M.values >>> fromFoldable) board.tiles
        tileAndPiece <- mapMaybe (hasEnemyPiece kingColor) posistions
        let (Tile tile)  = snd tileAndPiece
        let (Piece piece) = fst tileAndPiece
        map (\r -> r.position) $ R.attacks board tile.position (piece.pieceType)

hasEnemyPiece :: PieceColor -> Tile -> MA.Maybe (Tuple Piece Tile)
hasEnemyPiece color (Tile tile) =
    do
        (Piece piece) <- tile.currentPiece
        guard $ piece.color /= color
        pure $ Tuple (Piece piece) (wrap tile)

exists :: forall a. (a -> Boolean) -> MA.Maybe a -> Boolean
exists f m = case m of
    MA.Just x -> f x
    MA.Nothing -> false
