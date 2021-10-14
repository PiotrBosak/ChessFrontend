module RNG where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
--import Web.Event.Event (Event)
--import Web.Event.Event as Event
newtype RandomNumber = RandomNumber Number
instance showRandomNumber :: Show RandomNumber where
    show (RandomNumber rng ) = show rng
type State = Maybe RandomNumber

data Action = Generate

randomComponent :: forall query input output m. MonadEffect m => H.Component query input output m
randomComponent =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }

        }





initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render st = do
    let value = maybe "" show st
    HH.form
        [ HE.onSubmit \_ -> Generate ]
        [ HH.label_
            [ HH.text "Generate random number"
            , HH.br_
            ]
        , HH.p_
            [ HH.text ("current value " <> value) ]
        , HH.button_
            [ HH.text "generate"]

        ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Generate -> do
--    H.liftEffect $ Event.preventDefault event
    newNumber <- H.liftEffect random
    H.put $ Just $ RandomNumber newNumber

