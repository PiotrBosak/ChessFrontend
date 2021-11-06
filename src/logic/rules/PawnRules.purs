module PawnRules (moves,attacks) where
import Domain
import Debug
import Data.Tuple
import Debug
import Data.Maybe
import Data.Array
import Data.Newtype (wrap,unwrap)
import Control.MonadZero (guard)
import Prelude

moves :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
moves board position = mapMaybe identity [oneTileMove board position,twoTileMove board position]

oneTileMove :: Board -> Position -> Maybe { position :: Position, moveType :: MoveType }
oneTileMove board position = map { position : _, moveType: Normal } $ move board position 1 (\_ -> true)

twoTileMove :: Board -> Position -> Maybe { position :: Position, moveType :: MoveType }
twoTileMove board position = map { position : _, moveType: TwoTileMove } $ move board position 2 (unwrap >>> \t -> t.numberOfMoves == 0)

move :: Board -> Position -> Int -> (Tile -> Boolean) -> Maybe Position
move board position distance predicate =
    do
        guard $ predicate $ tileAt position board
        (Piece currentPiece) <- pieceAt position board
        (Tile nextTile) <- nextPawnTile board position currentPiece.color distance
        guard $ isNothing nextTile.currentPiece
        pure nextTile.position


nextPawnTile :: Board -> Position -> PieceColor -> Int -> Maybe Tile
nextPawnTile board (Position currentPosition) color distance =
    let
        op = case color of
            WhitePiece -> (+)
            BlackPiece -> (-)
    in
        do
            newRank <- numberToRank $ op (rankToNumber currentPosition.rank) distance
            let newPosition = wrap $ currentPosition { rank = newRank }
            pure $ tileAt newPosition board

attacks :: Board -> Position -> Array { position :: Position, moveType :: MoveType }
attacks board position = mapMaybe identity $ map (\f -> f board position) [leftAttack,rightAttack,lePassant]

leftAttack :: Board -> Position -> Maybe { position :: Position, moveType :: MoveType }
leftAttack board position = map { position : _, moveType: Normal } $ attackPosition board position leftAttackDifference

rightAttack :: Board -> Position -> Maybe { position :: Position, moveType :: MoveType }
rightAttack board position = map { position : _, moveType: Normal } $ attackPosition board position rightAttackDifference

leftAttackDifference :: PieceColor -> Tuple Int Int
leftAttackDifference = case _ of
    WhitePiece -> Tuple (-1) 1
    BlackPiece -> Tuple (-1) (-1)

rightAttackDifference :: PieceColor -> Tuple Int Int
rightAttackDifference = case _ of
    WhitePiece -> Tuple 1 1
    BlackPiece -> Tuple 1 (-1)

attackPosition :: Board -> Position -> (PieceColor -> Tuple Int Int) -> Maybe Position
attackPosition board position f =
        do
            let (Tile tile) = tileAt position board
            pieceColor <- map (unwrap >>> \p -> p.color) tile.currentPiece
            let difference = f pieceColor
            toPosition <- movePosition position difference
            let (Tile tileTo) = tileAt toPosition board
            (Piece toPiece) <-  tileTo.currentPiece
            guard $ toPiece.color /= pieceColor
            pure toPosition


lePassant :: Board -> Position -> Maybe { position :: Position, moveType :: MoveType }
lePassant board position = head $ mapMaybe (\b -> lePassant' board position b) [true,false]


lePassant' :: Board -> Position -> Boolean -> Maybe { position :: Position, moveType :: MoveType }
lePassant' board position isLeft =
    let
        difference = if isLeft then -1 else 1
    in
        map { position : _, moveType: LePassant } $ positionWithEnemyPiece board position difference


positionWithEnemyPiece :: Board -> Position -> Int -> Maybe Position
positionWithEnemyPiece board (Position attackingPosition) difference =
    do
        withEnemyFile <- numberToFile $ fileToNumber attackingPosition.file + difference
        let withEnemyPiece = attackingPosition { file = withEnemyFile }
        previousMove <- board.previousMove
        (Piece attackingPiece) <- pieceAt (wrap attackingPosition) board
        guard $ isLePassantPossible board previousMove attackingPiece.color (wrap attackingPosition) withEnemyFile
        let rankDifference = if attackingPiece.color == WhitePiece then 1 else -1
        newRank <- numberToRank $ rankToNumber withEnemyPiece.rank + rankDifference
        pure $ wrap $ withEnemyPiece { rank = newRank }


isLePassantPossible :: Board -> Move -> PieceColor -> Position -> File -> Boolean
isLePassantPossible board (Move previousMove) color (Position attackerPosition) withEnemyFile=
    let
           (Position from ) = previousMove.from
           (Position to ) = previousMove.to
           startingEnemyRank = if color == WhitePiece then Seven else Two
           isTheSameRank = to.rank == attackerPosition.rank
           isPreviousStartCorrect = from.rank == startingEnemyRank
           isFileTheSame = to.file == withEnemyFile
           isEnemyPawnMovedLast = enemyPawnMovedLast board color (wrap previousMove)
        in
        isTheSameRank && isPreviousStartCorrect && isFileTheSame && isEnemyPawnMovedLast

enemyPawnMovedLast :: Board -> PieceColor -> Move -> Boolean
enemyPawnMovedLast board color (Move m) =
    let
       (Tile tile) = (tileAt m.to board)
    in
       maybe false (\(Piece p) -> p.pieceType == Pawn && p.color /= color) tile.currentPiece
