module Domain where
import Data.Maybe
import Data.Generic.Rep (class Generic)
import Control.Comonad.Store
import Control.Comonad
import Data.Show.Generic (genericShow)
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
derive instance genericRank :: Generic Rank _
instance showRank :: Show Rank where
  show = genericShow

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

nextRank :: Rank -> Maybe Rank
nextRank rank = numberToRank $ (rankToNumber rank) + 1
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
derive instance genericFile :: Generic File _
instance showFile :: Show File where
  show = genericShow

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

nextFile :: File -> Maybe File
nextFile file = numberToFile $ (fileToNumber file) + 1

newtype Piece = Piece { pieceType :: PieceType
                      , color :: PieceColor
                      }

derive instance newtypePiece:: Newtype Piece _
instance eqPiece :: Eq Piece where
    eq (Piece fst) (Piece snd) = (eq fst.pieceType snd.pieceType) && (eq fst.color snd.color)


data TileColor = WhiteTile | BlackTile
derive instance genericTileColor :: Generic TileColor _
instance showTileColor :: Show TileColor where
  show = genericShow
newtype Position = Position { file :: File
                            , rank :: Rank
                            }
tileColor :: Tile -> TileColor
tileColor (Tile tile) =
    let (Position position) = tile.position
    in
        if ((rankToNumber position.rank) + (fileToNumber position.file)) `mod` 2 == 1
        then WhiteTile
        else BlackTile

instance showPosition :: Show Position where
    show (Position position) = show position.file <> " " <> show position.rank

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

instance ordTile :: Ord Tile where
    compare (Tile fst) (Tile snd) = compare fst.position snd.position

instance eqTile :: Eq Tile where
    eq (Tile fst) (Tile snd) = (eq fst.position snd.position) && (eq fst.currentPiece snd.currentPiece)

instance showTile :: Show Tile where
    show (Tile tile) = (show tile.position) <> (show $ tileColor (wrap tile))

hasPiece :: Tile -> Boolean
hasPiece (Tile t) = isJust t.currentPiece
position :: Tile -> Position
position (Tile t) = t.position
isEmpty :: Tile -> Boolean
isEmpty (Tile t) = isNothing t.currentPiece
derive instance newtypeTile :: Newtype Tile _
data Player = WhitePlayer | BlackPlayer

data MoveType
           = Normal
           | TwoTileMove
           | Castling
           | LePassant

newtype Move = Move { from :: Position
            , to :: Position
            , moveType :: MoveType
            }

derive instance newtypeMove :: Newtype Move _

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
hasMoved :: Tile -> Boolean
hasMoved (Tile t) = t.numberOfMoves >= 0
