module Dict
  ( Dict (..),
    createDict,
    Dict.insert,
    Dict.delete,
    Dict.find,
    Dict.mapDict,
    Dict.foldlDict,
    Dict.foldrDict,
    Dict.filterDict,
  )
where

import RBTree
  ( Color (..),
    RBDictionary (..),
    delete',
    filter',
    foldl'',
    foldr'',
    fromList',
    insert',
    lookup',
    map',
  )

newtype Dict a b = Dict (RBDictionary a b)

instance (Show a, Show b) => Show (Dict a b) where
  show (Dict m) = show m

instance (Ord a, Eq a, Eq b) => Eq (Dict a b) where
  (==) (Dict m1) (Dict m2) = m1 == m2

instance (Ord a) => Semigroup (Dict a b) where
  (<>) (Dict m1) (Dict m2) = Dict (m1 <> m2)

instance (Ord a) => Monoid (Dict a b) where
  mempty = createDict

createDict :: Dict a b
createDict = Dict Leaf

insert :: (Ord a) => a -> b -> Dict a b -> Dict a b
insert k vl (Dict dict) = Dict (insert' k vl dict)

delete :: (Ord a) => a -> Dict a b -> Dict a b
delete k (Dict dict) = Dict (delete' k dict)

find :: (Ord a) => a -> Dict a b -> Maybe b
find k (Dict dict) = lookup' k dict

mapDict :: (Ord a) => (b -> c) -> Dict a b -> Dict a c
mapDict f (Dict dict) = Dict (map' f dict)

foldlDict :: (Ord a) => ((a, b) -> c -> c) -> c -> Dict a b -> c
foldlDict f acc (Dict dict) = foldl'' f acc dict

foldrDict :: (Ord a) => ((a, b) -> c -> c) -> c -> Dict a b -> c
foldrDict f acc (Dict dict) = foldr'' f acc dict

filterDict :: (Ord a) => (b -> Bool) -> Dict a b -> Dict a b
filterDict p (Dict dict) = Dict (filter' p dict)
