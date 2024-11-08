{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RBTree
  ( RBDictionary (..),
    Color (..),
    insert',
    remove',
    map',
    foldl'',
    foldr'',
    lookup',
    fromList',
    filter',
  )
where

data Color = Red | Black deriving (Show, Eq)

data RBDictionary a b
  = Leaf
  | Node
      { key :: a,
        value :: b,
        color :: Color,
        leftChild ::
          RBDictionary
            a
            b,
        rightChild ::
          RBDictionary
            a
            b
      }
  deriving (Show)

instance (Eq b, Ord a) => Eq (RBDictionary a b) where
  (==) a b =
    and $
      zipWith
        (\(k1, v1) (k2, v2) -> k1 == k2 && v1 == v2)
        (foldr'' join [] a)
        (foldr'' join [] b)
    where
      join (k, v) acc = (k, v) : acc

-- Monoid implementation
instance (Ord a) => Semigroup (RBDictionary a b) where
  (<>) = foldr'' (\(k, vl) acc -> insert' k vl acc)

instance (Ord a) => Monoid (RBDictionary a b) where
  mempty = Leaf

fromList' :: (Ord a) => [(a, b)] -> RBDictionary a b
fromList' ls = go ls Leaf
  where
    go [] d = d
    go (x : xs) d = go xs $ uncurry insert' x d

lookup' :: (Ord a) => a -> RBDictionary a b -> Maybe b
lookup' _ Leaf = Nothing
lookup' a Node {key = nk, value = vl, leftChild = left, rightChild = right}
  | a == nk = Just vl
  | a < nk = lookup' a left
  | otherwise = lookup' a right

insert' :: (Ord a) => a -> b -> RBDictionary a b -> RBDictionary a b
insert' k v d = makeBlack $ insertHelper k v d

insertHelper :: (Ord a) => a -> b -> RBDictionary a b -> RBDictionary a b
insertHelper k v Leaf = Node k v Red Leaf Leaf
insertHelper k v node@Node {key = nk, leftChild = left, rightChild = right}
  | k < nk = balance (node {leftChild = insertHelper k v left})
  | k > nk = balance (node {rightChild = insertHelper k v right})
  | otherwise = node {value = v}

balance :: RBDictionary a b -> RBDictionary a b
balance (Node z zv Black (Node y yv Red (Node x xv Red a b) c) d) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance (Node z zv Black (Node x xv Red a (Node y yv Red b c)) d) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance (Node x xv Black a (Node z zv Red (Node y yv Red b c) d)) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance (Node x xv Black a (Node y yv Red b (Node z zv Red c d))) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance n = n

balance' :: RBDictionary a b -> RBDictionary a b
balance' Leaf = Leaf
balance' (Node x xv clr left right) = balance (Node x xv clr left right)

makeBlack :: RBDictionary a b -> RBDictionary a b
makeBlack (Node k v _ left right) = Node k v Black left right
makeBlack Leaf = Leaf

remove' :: (Ord a) => a -> RBDictionary a b -> RBDictionary a b
remove' k dict = makeBlack $ del k dict

del :: (Ord a) => a -> RBDictionary a b -> RBDictionary a b
del _ Leaf = Leaf
del k node@Node {key = nk, leftChild = left, rightChild = right}
  | k < nk = delL k node
  | k > nk = delR k node
  | otherwise = fuse left right

balL :: RBDictionary a b -> RBDictionary a b
balL (Node y yv Black (Node x xv Red t1 t2) t3) = Node y yv Red (Node x xv Black t1 t2) t3
balL (Node y yv Black t1 (Node z zv Black t2 t3)) = balance' (Node y yv Black t1 (Node z zv Red t2 t3))
balL (Node y yv Black t1 (Node z zv Red (Node u uv Black t2 t3) (Node t tval Black l r))) = Node u uv Red (Node y yv Black t1 t2) (balance' (Node z zv Black t3 (Node t tval Red l r)))
balL node = node

balR :: RBDictionary a b -> RBDictionary a b
balR (Node y yv Black t1 (Node x xv Red t2 t3)) = Node y yv Red t1 (Node x xv Black t2 t3)
balR (Node y yv Black (Node z zv Black t1 t2) t3) = balance' (Node y yv Black (Node z zv Red t1 t2) t3)
balR (Node y yv Black (Node z zv Red (Node t tv Black l r) (Node u uv Black t2 t3)) t4) = Node u uv Red (balance' (Node z zv Black (Node t tv Red l r) t2)) (Node y yv Black t3 t4)
balR node = node

delL :: (Ord a) => a -> RBDictionary a b -> RBDictionary a b
delL k (Node y yv _ t1@(Node _ _ Black _ _) t2) = balL $ Node y yv Black (del k t1) t2
delL k (Node y yv _ t1 t2) = Node y yv Red (del k t1) t2
delL _ Leaf = Leaf

delR :: (Ord a) => a -> RBDictionary a b -> RBDictionary a b
delR k (Node y yv _ t1 t2@(Node _ _ Black _ _)) = balR $ Node y yv Black t1 (del k t2)
delR k (Node y yv _ t1 t2) = Node y yv Red t1 (del k t2)
delR _ Leaf = Leaf

fuse :: RBDictionary a b -> RBDictionary a b -> RBDictionary a b
fuse Leaf t = t
fuse t Leaf = t
fuse t1@(Node _ _ Black _ _) (Node y yv Red t3 t4) = Node y yv Red (fuse t1 t3) t4
fuse (Node x xv Red t1 t2) t3@(Node _ _ Black _ _) = Node x xv Red t1 (fuse t2 t3)
fuse (Node x xv Red t1 t2) (Node y yv Red t3 t4) =
  let s = fuse t2 t3
   in case s of
        (Node z zv Red s1 s2) -> Node z zv Red (Node x xv Red t1 s1) (Node y yv Red s2 t4)
        (Node _ _ Black _ _) -> Node x xv Red t1 (Node y yv Red s t4)
        Leaf -> Node x xv Red t1 (Node y yv Red Leaf t4)
fuse (Node x xv Black t1 t2) (Node y yv Black t3 t4) =
  let s = fuse t2 t3
   in case s of
        (Node z zv Red s1 s2) -> Node z zv Red (Node x xv Black t1 s1) (Node y yv Black s2 t4)
        (Node _ _ Black _ _) -> balL (Node x xv Black t1 (Node y yv Black s t4))
        Leaf -> balL (Node x xv Black t1 (Node y yv Red Leaf t4))

map' :: (Ord a) => (b -> c) -> RBDictionary a b -> RBDictionary a c
map' _ Leaf = Leaf
map' f node@Node {value = vl, leftChild = left, rightChild = right} = node {value = f vl, leftChild = map' f left, rightChild = map' f right}

foldl'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBDictionary a b -> c
foldl'' _ acc Leaf = acc
foldl'' f acc (Node {key = k, value = vl, leftChild = left, rightChild = right}) = foldl'' f (f (k, vl) (foldl'' f acc left)) right

foldr'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBDictionary a b -> c
foldr'' _ acc Leaf = acc
foldr'' f acc (Node {key = k, value = vl, leftChild = left, rightChild = right}) = foldr'' f (f (k, vl) (foldr'' f acc right)) left

filter' :: (Ord a) => (b -> Bool) -> RBDictionary a b -> RBDictionary a b
filter' p = foldr'' (\(k, vl) d -> if p vl then insert' k vl d else d) (fromList' [])
