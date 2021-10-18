module Main where

import Prelude


import Affjax as AX
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
import Data.Map.Internal

main :: Effect Unit
main =
    do
       let board = createBoard
       log $ show $ size board.tiles


data IsLoading = Loading | NotLoading
isLoadingToBoolean :: IsLoading -> Boolean
isLoadingToBoolean Loading = true
isLoadingToBoolean NotLoading = false


newtype UserName = UserName String
instance showUserName :: Show UserName where
    show (UserName name) = name


newtype Result = Result String

type State =
    { loading :: IsLoading
    , username :: UserName
    , result :: Maybe Result
    }

data Action
        = SetUsername UserName
        | MakeRequest Event

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

initialState :: forall input. input -> State
initialState _ = { loading: NotLoading, username: UserName "", result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
    HH.form
        [ HE.onSubmit MakeRequest ]
        [ HH.h1_ [ HH.text "Look up Github user" ]
        , HH.label_
            [ HH.div_ [ HH.text "Enter username: " ]
            , HH.input
                [ HP.value $ show st.username
                , HE.onValueInput \str -> SetUsername $ UserName str
                ]
            ]
        , HH.button
            [ HP.disabled $ isLoadingToBoolean st.loading
            , HP.type_ HP.ButtonSubmit
            ]
            [ HH.text "Fetch info" ]
        , HH.p_
            [ HH.text $ case st.loading of
                               Loading -> "Working..."
                               NotLoading -> "" ]
        , HH.div_
            case st.result of
                Nothing -> []
                Just (Result result) ->
                    [ HH.h2_
                        [ HH.text "Response" ]
                    , HH.pre_
                        [ HH.code_ [ HH.text result ] ]
                    ]
        ]


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    SetUsername (UserName name) -> do
        H.modify_ _ { username = UserName name, result = Nothing }

    MakeRequest event -> do
        H.liftEffect $ Event.preventDefault event
        username <- H.gets _.username
        H.modify_ _ { loading = Loading }
        response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> show username)
        H.modify_ _ { loading = NotLoading, result = map (\r -> Result r.body) (hush response) }
