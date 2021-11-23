module Data.Experience where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe
import Data.Argonaut.Decode.Error
import Data.Argonaut.Decode.Class
import Data.Argonaut.Decode.Decoders
import Data.Generic.Rep (class Generic)
import Data.Codec.Argonaut.Sum as CAS
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Prelude
import Data.Either
import Data.Show.Generic (genericShow)

data Experience = Beginner | Intermediate | Expert

derive instance eqExperience :: Eq Experience
derive instance genericExperience :: Generic Experience _
instance showExperience :: Show Experience where
  show = genericShow
derive instance ordExperience :: Ord Experience

codec :: JsonCodec Experience
codec = CAS.enumSum show fromString

fromString = case _ of
                 "beginner" -> Just Beginner
                 "intermediate" -> Just Intermediate
                 "expert" -> Just Expert
                 _ -> Nothing

instance decodeExperience :: DecodeJson Experience where
    decodeJson json = decodeString json >>= (note (TypeMismatch "String") <$> fromString)

