module Domain where
import Data.Maybe
import Effect.Unsafe (unsafePerformEffect)
import Data.Tuple
import Data.Ord
import Effect.Exception.Unsafe
import Data.Map
import Data.Newtype (class Newtype, over2, wrap)
import Prelude

data PieceType
        = Pawn
        | Bishop
        | Knight
        | Rook
        | Queen
        | King
derive instance eqPieceType :: Eq PieceType
derive instance ordPieceType :: Ord PieceType
data PieceColor = BlackPiece | WhitePiece
derive instance eqPieceColor :: Eq PieceColor

data Rank
        = One
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight

derive instance eqRank :: Eq Rank
derive instance ordRank :: Ord Rank

rankToNumber :: Rank -> Int
rankToNumber = case _ of
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8

numberToRank :: Int -> Maybe Rank
numberToRank = case _ of
    1 -> Just One
    2 -> Just Two
    3 -> Just Three
    4 -> Just Four
    5 -> Just Five
    6 -> Just Six
    7 -> Just Seven
    8 -> Just Eight
    _ -> Nothing

data File
        = A
        | B
        | C
        | D
        | E
        | F
        | G
        | H

derive instance eqFile :: Eq File
derive instance ordFile :: Ord File

fileToNumber :: File -> Int
fileToNumber = case _ of
    A -> 1
    B -> 2
    C -> 3
    D -> 4
    E -> 5
    F -> 6
    G -> 7
    H -> 8

numberToFile :: Int -> Maybe File
numberToFile  = case _ of
   1 -> Just A
   2 -> Just B
   3 -> Just C
   4 -> Just D
   5 -> Just E
   6 -> Just F
   7 -> Just G
   8 -> Just H
   _ -> Nothing

newtype Piece = Piece { pieceType :: PieceType
                      , color :: PieceColor
                      }

derive instance newtypePiece:: Newtype Piece _
data TileColor = WhiteTile | BlackTile

newtype Position = Position { file :: File
                            , rank :: Rank
                            }
derive instance newtypePosition :: Newtype Position _
instance eqPosition :: Eq Position where
   eq :: Position -> Position -> Boolean
   eq (Position fst) (Position snd) = fst.file == snd.file && fst.rank == snd.rank

instance ordPosition :: Ord Position where
    compare (Position fst) (Position snd) =
        let
            compareFiles = compare fst.file snd.file
        in
            if compareFiles == EQ then compare fst.rank snd.rank else compareFiles


movePosition :: Position -> Tuple Int Int -> Maybe Position
movePosition (Position position) (Tuple file rank) = do
    newFile <- numberToFile $ fileToNumber position.file + file
    newRank <- numberToRank $ rankToNumber position.rank + rank
    pure $ wrap { file: newFile , rank: newRank }

newtype Tile = Tile { position :: Position
                    , currentPiece :: Maybe Piece
                    , numberOfMoves :: Int
                    }

derive instance newtypeTile :: Newtype Tile _
data Player = WhitePlayer | BlackPlayer

data MoveType
           = Normal
           | TwoTileMove
           | Castling
           | LePassant

type Move = { from :: Position
            , to :: Position
            , moveType :: MoveType
            }
type Board = { tiles :: Map Position Tile
             , previousMove :: Maybe Move
             }

tileAt :: Position -> Board -> Tile
tileAt position board = case lookup position board.tiles of
    Just x -> x
    Nothing -> unsafeThrow "should never happen that there is an empty key in map"

pieceAt :: Position -> Board -> Maybe Piece
pieceAt position board =
    let
        (Tile t) = (tileAt position board)
    in
        t.currentPiece

