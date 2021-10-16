module RookRules where

import Data.Array
import Data.Maybe
import Data.Tuple
import Domain
import Prelude

import RulesForMultiMoves as MM

combinations :: Array (Tuple Int Int)
combinations = [ (Tuple 1 0)
               , (Tuple 0 1)
               , (Tuple (-1) 0)
               , (Tuple 0 (-1))
               ]

moves :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
moves board position = MM.moves board position combinations

attacks :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
attacks board position = MM.attacks board position combinations
