module Rules where

import Prelude
import Data.Map as M
import Data.List
import Data.Maybe as MA
import Type.Proxy
import PawnRules as PR
import BishopRules as BR
import KnightRules as KNR
import RookRules as RR
import QueenRules as QR
import KingRules as KR
import Domain

moves :: Board -> Position -> PieceType -> Array { position :: Position, moveType :: MoveType }
moves board position pieceType = case pieceType of
    Pawn -> PR.moves board position
    Bishop -> BR.moves board position
    Knight -> KNR.moves board position
    Rook -> RR.moves board position
    Queen -> QR.moves board position
    King -> KR.moves board position

attacks :: Board -> Position -> PieceType -> Array { position :: Position, moveType :: MoveType }
attacks board position pieceType = case pieceType of
    Pawn -> PR.attacks board position
    Bishop -> BR.attacks board position
    Knight -> KNR.attacks board position
    Rook -> RR.attacks board position
    Queen -> QR.attacks board position
    King -> KR.attacks board position

allMoves :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
allMoves board position =
   let
       m = do
              let (Tile tile) = tileAt position board
              (Piece piece) <- tile.currentPiece
              let possibleMoves = [moves,attacks] >>= (\f -> f board position piece.pieceType)
              pure $ possibleMoves
   in
       MA.maybe [] identity m



