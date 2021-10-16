module Rules where

import Prelude
import Data.List
import Type.Proxy
import PawnRules as PR
import Domain

moves :: Board -> Position -> PieceType -> Array { position :: Position, moveType :: MoveType }
moves board position pieceType = case pieceType of
    Pawn -> PR.moves
    Bishop -> []
    Knight -> []
    Rook -> []
    Queen -> []
    King -> []
attacks :: Board -> Position -> PieceType -> Array { position :: Position, moveType :: MoveType }
attacks board position pieceType = case pieceType of
    Pawn -> PR.attacks
    Bishop -> []
    Knight -> []
    Rook -> []
    Queen -> []
    King -> []
