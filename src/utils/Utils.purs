module Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
-- | a few characters all over our HTML.
css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName
