module BishopRules(moves,attacks) where
import Data.Tuple
import Domain
import Data.Array
import Data.Maybe
import RulesForMultiMoves as MM
import Prelude

combinations :: Array (Tuple Int Int)
combinations = [ (Tuple 1 (-1))
               , (Tuple 1 1)
               , (Tuple (-1) 1)
               , (Tuple (-1) (-1))
               ]

moves :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
moves board position = MM.moves board position combinations

attacks :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
attacks board position = MM.attacks board position combinations
