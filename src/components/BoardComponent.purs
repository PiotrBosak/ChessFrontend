module BoardComponent where
import Domain as D
import Affjax as AX
import Effect.Timer
import Data.Newtype
import Utils
import Data.Newtype as DN
import Halogen.HTML.CSS
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import RNG (randomComponent)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Subscriptions (component) as S
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Effect.Class.Console (log)
import Web.Event.Event as Event
import BoardFactory
import MapUtils as MU
import Data.Map.Internal as M
import Data.List as L
import Data.Maybe
import Domain
import CSS as C
import Halogen.HTML.CSS as HC
import Data.Array
import Game
import Control.Plus
import Data.Int
import Prelude
import Debug
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)

type State = { game :: Game
             , selectedPosition :: Maybe Position
             }

data Action = Click D.Position | IsCheckOrMate
someAction = IsCheckOrMate
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
        }


initialState :: forall input. input -> State
initialState _ = {game: makeGame, selectedPosition : Nothing }


render :: forall m . State -> H.ComponentHTML Action () m
render { game: (Game game)} =
    let
        currentBoard = game.currentBoard
        groupedByRank = MU.groupByKeys (\(D.Position p) -> p.rank) currentBoard.tiles
        sortedRanks = L.reverse $ L.sort $ M.keys groupedByRank
        getUnderRank = \r -> maybe [] identity $ M.lookup r groupedByRank
    in HH.form_
        [HH.div_
            [ HH.h1_ [HH.text "aaa"],
              (HH.table [HC.style (C.borderSpacing (C.nil))] $ map (renderRank <<< getUnderRank) $ fromFoldable sortedRanks)
        ]
        ]

renderRank :: forall m.  Array D.Tile -> H.ComponentHTML Action () m
renderRank tiles = HH.tr [HC.style (C.borderSpacing (C.nil) )] $ map renderTile (sort tiles)

renderTile (D.Tile tile) =
    let color = D.tileColor $ DN.wrap tile
    in HH.td [(square color), (HE.onClick (\_ -> Click tile.position))] [maybeImage (wrap tile)]


maybeImage (Tile tile) =
    let piece = tile.currentPiece
    in maybe (HH.text "") identity $ map (\p -> HH.img [(pieceImage p), pieceStyle]) piece


pieceImage (Piece p) = HP.src ("pieces/" <> (imageName {pieceType: p.pieceType, color: p.color}))
pieceStyle = HC.style $ C.width (C.pct  (toNumber 85)) *> C.display C.block

imageName :: {pieceType :: PieceType, color :: PieceColor } -> String
imageName = case _ of
    {pieceType : Pawn, color: WhitePiece} -> "whitePawn.png"
    {pieceType : Bishop, color: WhitePiece} -> "whiteBishop.png"
    {pieceType : Knight, color: WhitePiece} -> "whiteKnight.png"
    {pieceType : Rook, color: WhitePiece} -> "whiteRook.png"
    {pieceType : Queen, color: WhitePiece} -> "whiteQueen.png"
    {pieceType : King, color: WhitePiece} -> "whiteKing.png"
    {pieceType : Pawn, color: BlackPiece} -> "blackPawn.png"
    {pieceType : Bishop, color: BlackPiece} -> "blackBishop.png"
    {pieceType : Knight, color: BlackPiece} -> "blackKnight.png"
    {pieceType : Rook, color: BlackPiece} -> "blackRook.png"
    {pieceType : Queen, color: BlackPiece} -> "blackQueen.png"
    {pieceType : King, color: BlackPiece} -> "blackKing.png"


square = case _ of
    D.WhiteTile -> HC.style $ C.height (C.px $ toNumber 50) *> C.width (C.px $ toNumber 50) *> C.backgroundColor (C.rgb 238 238 210) *>  C.display C.inlineBlock *> C.border C.solid (C.px $ toNumber 1) C.black
    D.BlackTile -> HC.style $ C.height (C.px $ toNumber 50) *> C.width (C.px $ toNumber 50) *> C.backgroundColor (C.rgb 118 150 86) *>  C.display C.inlineBlock *> C.border C.solid (C.px $ toNumber 1) C.black


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    Click to ->
        do
            state <- H.get
            let newState = case state.selectedPosition of
                    Nothing -> state {selectedPosition = Just to }
                    Just from ->
                     let afterMove = move from to state.game
                     in maybe (state {selectedPosition = Just to}) (\g -> state { game = g, selectedPosition = Nothing }) afterMove
            H.put newState
            handleAction IsCheckOrMate

    IsCheckOrMate -> H.modify_ \s -> s {game = updateCheckOrMate (s.game)}


