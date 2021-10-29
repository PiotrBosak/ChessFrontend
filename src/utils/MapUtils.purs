module MapUtils where
import Data.Map.Internal as M
import Data.Tuple
import Data.Array
import Data.Maybe
import Prelude


groupByKeys :: forall k v a. Ord k => Ord a => (k -> a) -> M.Map k v -> M.Map a (Array v)
groupByKeys f map =
    let
        arr = toArray map
    in foldl (\m tuple -> updateResult tuple f m) M.empty arr


updateResult :: forall k v a. Ord k => Ord a => Tuple k v -> (k -> a) -> M.Map a (Array v) -> M.Map a (Array v)
updateResult tuple f map =
    let
       a = (f <<< fst) tuple
       lu = M.lookup a map
    in maybe (M.insert a [snd tuple] map) (\arr -> M.insert a (cons (snd tuple) arr) map) lu

toArray :: forall k v. M.Map k v -> Array (Tuple k v)
toArray m = zip (fromFoldable (M.keys m)) (fromFoldable (M.values m))
