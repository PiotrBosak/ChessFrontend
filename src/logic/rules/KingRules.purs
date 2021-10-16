module KingRules where

import Control.MonadZero
import Data.Array
import Data.Maybe
import Data.Newtype
import Data.Ord
import Data.Tuple
import Domain
import Prelude

import Data.List as L
import Data.Map as M
import KnightAndKingRules as KK

combinations :: Array (Tuple Int Int)
combinations = [ (Tuple 0 1)
               , (Tuple 0 (-1))
               , (Tuple 1 1)
               , (Tuple 1 0)
               , (Tuple 1 (-1))
               , (Tuple (-1) 1)
               , (Tuple (-1) 0 )
               , (Tuple (-1) (-1))
               ]

attacks :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
attacks board position = map { position : _, moveType: Normal } $ KK.attacks board position combinations

moves :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
moves board position =
    let
        castlings = mapMaybe (\f -> f board position) [castlingMove true, castlingMove false]
        normalMoves = map { position : _, moveType: Normal } $ KK.moves board position combinations
    in
        append castlings normalMoves


castlingMove :: Boolean -> Board -> Position ->  Maybe { position :: Position, moveType :: MoveType }
castlingMove isLeftRook board position  =
    do
       let rookFile = if isLeftRook then A else H
       let newKingFile = if isLeftRook then B else G
       (Tile kingTile) <- Just $ tileAt position board
       let (Position kingPosition) = kingTile.position
       guard $ hasMoved $ wrap kingTile
       let (Position rookPosition) = Position $ {file: rookFile, rank: kingPosition.rank}
       (Tile rookTile) <- getRookTile board kingTile.position rookPosition.file
       guard $ areTilesClear board kingTile.position rookTile.position
       let newPosition = Position $ { file: newKingFile, rank: rookPosition.rank}
       pure $ {position: newPosition, moveType: Castling}


getRookTile :: Board -> Position -> File -> Maybe Tile
getRookTile board (Position position) file =
    do
        let rookPosition = Position $ position { file = file }
        tile <- Just $ tileAt rookPosition board
        guard $ hasMoved tile
        pure tile

areTilesClear :: Board -> Position -> Position -> Boolean
areTilesClear board from to =
    all hasPiece $ filter (\t -> isTileClear t from to) $ (M.values >>> fromFoldable) board.tiles


isTileClear :: Tile -> Position -> Position -> Boolean
isTileClear (Tile tile) (Position from) (Position to) =
    let
        (Position p) = tile.position
    in
        p.rank == from.rank && (isBetween (wrap from) (wrap to) (fileToNumber p.file))

isBetween :: Position -> Position -> Int -> Boolean
isBetween (Position from) (Position to) file =
    let
        smaller = min (rankToNumber from.rank) (rankToNumber to.rank)
        bigger = max (rankToNumber from.rank) (rankToNumber to.rank)
    in
        file >= smaller && file <= bigger
