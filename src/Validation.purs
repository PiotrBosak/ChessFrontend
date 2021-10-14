module Validation where

import Prelude

import Control.Applicative
import Data.Maybe
import Data.String
import Data.Semigroup
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Data.Semiring
import Halogen.HTML.Properties as HP
import Data.Validation.Semigroup (V, invalid,validation)
newtype City = City String

newtype Error = Error String
type Errors = Array Error
newtype Street = Street String


newtype Address = Address
                { city ::  City
                , street :: Street
                }
address :: City -> Street -> Address
address city street = Address { city, street }

getCity :: String -> V Errors City
getCity str = if (length str) > 3 then pure (City str) else invalid ([Error "lol"])

getStreet :: String -> V Errors Street
getStreet str = if (length str) > 3 then pure (Street str) else invalid ([Error "lol"])


getCityMaybe :: String -> Maybe City
getCityMaybe str = Just $ City str
getStreetMaybe :: String -> Maybe Street
getStreetMaybe str = Nothing

--createAddressMaybe :: String -> String -> Maybe Address
--createAddressMaybe a b = do
--                city <- getCityMaybe a
--                street <- getStreetMaybe b
--                pure (Address city street)



data Action = Increment | Decrement

newtype Name = Name String
type Person =
            { name :: Name
            , address :: Maybe Address
            }
instance showCity :: Show City where
    show (City c) = c
instance showStreet :: Show Street where
    show (Street s) = s

instance showAddress :: Show Address where
    show :: Address -> String
    show (Address {city : City c , street : Street s}) = show c <> " " <> show s

names = ["Hello", "World"]
component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0
  render state =
    HH.div_
    [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.div_ [ HH.text $ show state ]
    , HH.div_ $ map HH.text names
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    ]
  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1

maybeElem :: forall w i a. Maybe a -> (a -> HH.HTML w i) -> HH.HTML w i
maybeElem elem f = case elem of
        Just x -> f x
        Nothing -> HH.text ""

primaryButton :: forall w i. String -> HH.HTML w i
primaryButton label =
    HH.button
        [ HP.classes  [ HH.ClassName "primary" ] ]
        [ HH.text label ]

type Input = Unit
type State = Int
myInitialState :: Input -> State
myInitialState _ = 0

myHandleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
myHandleAction = case _ of
    Decrement ->
        H.modify_ (_ - 1)

    Increment ->
        H.modify_ (_ + 1)

myRender :: forall m. State -> H.ComponentHTML Action () m
myRender state =
    HH.div_
        [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
        , HH.text (show state)
        , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
        ]
