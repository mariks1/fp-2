{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
module PropertyTestRBTree (properties) where

import RBTree
import Test.Tasty
import Test.Tasty.QuickCheck as QC

properties :: TestTree
properties =
  testGroup
    "RBTree properties-tests"
    [ QC.testProperty "Test monoid property" (testMempty :: RBDictionary Int Int -> Bool),
      QC.testProperty "Test monoid associativity" (monoidAssoc :: RBDictionary Int Int -> RBDictionary Int Int -> RBDictionary Int Int -> Bool),
      QC.testProperty "Test color validation" (testColor :: RBDictionary Int Int -> Bool)
    ]

instance (Ord a, Eq a, Arbitrary a, Arbitrary b) => Arbitrary (RBDictionary a b) where
  arbitrary = do
    pairs <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ fromList' pairs

testMempty :: (Ord a, Eq b) => RBDictionary a b -> Bool
testMempty m = (m <> mempty) == m && (mempty <> m) == m

monoidAssoc :: (Ord a, Eq b) => RBDictionary a b -> RBDictionary a b -> RBDictionary a b -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

testColor :: (Ord a) => RBDictionary a b -> Bool
testColor Leaf = True
testColor node@Node {color = Black} = go node
  where
    go Leaf = True
    go Node {color = Black, leftChild = left, rightChild = right} = go left && go right
    go Node {color = Red, leftChild = left@Node {color = Black}, rightChild = right@Node {color = Black}} = go left && go right
    go Node {color = Red, leftChild = left@Node {color = Black}, rightChild = Leaf} = go left
    go Node {color = Red, leftChild = Leaf, rightChild = right@Node {color = Black}} = go right
    go Node {color = Red, leftChild = Leaf, rightChild = Leaf} = True
    go _ = False
testColor _ = False
