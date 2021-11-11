module BoardComponent where
import Domain as D
import Affjax as AX
import Halogen.Store.Monad (class MonadStore)
import Data.Newtype
import Slug (Slug)
import Halogen.Store.Connect (Connected, connect)
import Utils
import Data.Newtype as DN
import Halogen.HTML.CSS
import Conduit.Store as Store
import Halogen.Store.Select (selectEq)
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
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
import CSS.Common
import Halogen.HTML.CSS as HC
import Data.Array
import Game
import Conduit.Capability.Resource.Article (class ManageArticle, getArticles)
import Conduit.Capability.Navigate
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
component
    :: forall q o m
       . MonadAff m
      => MonadStore Store.Action Store.Store m
      => Navigate m
      => ManageArticle m
      => H.Component q Unit o m
component =
    connect (selectEq _.currentUser) $ H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
        }


initialState :: forall input. input -> State
initialState _ = {
    game: makeGame,
    selectedPosition : Nothing
     }

render :: forall m . State -> H.ComponentHTML Action () m
render {selectedPosition : selected,  game: (Game game)} =
    let
        currentBoard = game.currentBoard
        groupedByRank = MU.groupByKeys (\(D.Position p) -> p.rank) currentBoard.tiles
        sortedRanks = L.reverse $ L.sort $ M.keys groupedByRank
        getUnderRank = \r -> maybe [] identity $ M.lookup r groupedByRank
    in HH.form
        [HC.style $ (C.display C.block) *> (C.marginLeft auto) *> (C.marginRight auto)]
        [HH.div
        [HC.style $ (C.display C.block)]
            [ HH.h1_ [HH.text "aaa"],
              (HH.table
              [HC.style $ (C.borderSpacing (C.nil)) *> (C.marginLeft auto) *> (C.marginRight auto)]
              $ map ((\a -> renderRank a selected game.turn) <<< getUnderRank) $ fromFoldable sortedRanks)
        ]
        ]

renderRank :: forall m. Array D.Tile -> Maybe Position -> Turn ->  H.ComponentHTML Action () m
renderRank tiles selected turn = HH.tr [HC.style (C.borderSpacing (C.nil) )] $ map (\t -> renderTile t selected turn)(sort tiles)

renderTile (D.Tile tile) selected turn =
    let
        color = D.tileColor $ DN.wrap tile
        isHisMove = case turn of
            WhiteTurn -> maybe false (\(Piece p) -> p.color == WhitePiece) tile.currentPiece
            BlackTurn -> maybe false (\(Piece p) -> p.color == BlackPiece) tile.currentPiece
        boardTileColor = if (maybe false (eq tile.position) selected) && isHisMove
                         then trace (show selected) \_ -> Selected
                         else case color of
                                WhiteTile -> White
                                BlackTile -> Black
    in
        HH.td [(square boardTileColor), (HE.onClick (\_ -> Click tile.position))] [maybeImage (wrap tile)]

data BoardTileColor = White | Black | Selected

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
    White -> HC.style $ C.height (C.px $ toNumber 50) *> C.width (C.px $ toNumber 50) *> C.backgroundColor (C.rgb 245 245 245) *>  C.display C.inlineBlock
    Black -> HC.style $ C.height (C.px $ toNumber 50) *> C.width (C.px $ toNumber 50) *> C.backgroundColor (C.rgb 32 32 32) *>  C.display C.inlineBlock
    Selected -> HC.style $ C.height (C.px $ toNumber 50) *> C.width (C.px $ toNumber 50) *> C.backgroundColor (C.rgb 0 127 255) *>  C.display C.inlineBlock

selectedColor = C.rgb 246 248 121


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    Click to ->
        do
            state <- H.get
            let newState = case state.selectedPosition of
                    Nothing -> state {selectedPosition = Just to}
                    Just from ->
                     if (eq from to)
                     then state {selectedPosition = Nothing }
                     else
                        let afterMove = move from to state.game
                        in maybe (state {selectedPosition = Just to}) (\g -> state { game = g, selectedPosition = Nothing}) afterMove
            H.put newState
            handleAction IsCheckOrMate

    IsCheckOrMate -> H.modify_ \s -> s {game = updateCheckOrMate (s.game)}


