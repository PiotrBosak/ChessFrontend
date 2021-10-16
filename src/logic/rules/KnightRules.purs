module KnightRules where
import Data.Tuple
import Prelude
import Domain
import KnightAndKingRules as KK


combinations :: Array (Tuple Int Int)
combinations = [ Tuple 2 1
               , Tuple 1 2
               , Tuple 2 (-1)
               , Tuple 1 (-2)
               , Tuple (-2) 1
               , Tuple (-2) (-1)
               , Tuple (-1) (-2)
               , Tuple (-1) 2
               ]

moves :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
moves board position = map { position : _, moveType: Normal } $ KK.moves board position  combinations

attacks :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
attacks board position = map { position : _, moveType: Normal } $ KK.attacks board position  combinations
