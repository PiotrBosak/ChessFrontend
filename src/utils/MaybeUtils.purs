module MaybeUtils where
import Data.Maybe
import Prelude

filter :: forall a. (a -> Boolean) -> Maybe a -> Maybe a
filter f m = case m of
    Just x -> if f x then Just x else Nothing
    Nothing -> Nothing

