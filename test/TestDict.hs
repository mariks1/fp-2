module TestDict (tests) where

import Dict
  ( Dict (..),
    createDict,
    delete,
    filterDict,
    find,
    foldlDict,
    foldrDict,
    insert,
    mapDict,
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Dict unit-tests"
    [ testCase "Create dictionary" testCreate,
      testCase "Insert 1 elem in dictionary" testInsert,
      testCase "Multiple insertation" testInsertMultiple,
      testCase "Delete the only 1 elem" testDeleteOnlyElem,
      testCase "Delete elem" testDelete,
      testCase "Map with increment" testMapInc,
      testCase "Map with reverse Strings" testMapStrings,
      testCase "Foldl with plus" testFoldlPlus,
      testCase "Foldr with minus" testFoldrMinus,
      testCase "Foldr with String" testFoldrString,
      testCase "Foldl with String" testFoldlString,
      testCase "Filter even" testFilterEven
    ]

testCreate :: Assertion
testCreate = do
  let dict = (createDict :: Dict Int Int)
  dict @?= createDict

testInsert :: Assertion
testInsert = do
  let dict = find 1 (insert 1 10 (createDict :: Dict Int Int))
  dict @?= Just 10

testInsertMultiple :: Assertion
testInsertMultiple = do
  let dict = createDict :: Dict Int String
  let dict' = foldr (\(k, v) acc -> insert k v acc) dict [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
  find 1 dict' @?= Just "one"
  find 2 dict' @?= Just "two"
  find 3 dict' @?= Just "three"
  find 4 dict' @?= Just "four"
  find 5 dict' @?= Just "five"

testDeleteOnlyElem :: Assertion
testDeleteOnlyElem = do
  let dict = insert 1 10 (createDict :: Dict Int Int)
  let dict' = delete 1 dict
  find 1 dict' @?= Nothing
  dict' @?= createDict

testDelete :: Assertion
testDelete = do
  let dict = insert 2 20 (insert 3 30 (insert 4 40 (insert 1 10 (createDict :: Dict Int Int))))
  let dict' = delete 3 dict
  find 3 dict' @?= Nothing
  find 2 dict' @?= Just 20
  find 4 dict' @?= Just 40
  find 1 dict' @?= Just 10

testMapInc :: Assertion
testMapInc = do
  let dict = insert 2 20 (insert 3 30 (insert 4 40 (insert 1 10 (createDict :: Dict Int Int))))
  let dict' = mapDict (+ 1) dict
  find 3 dict' @?= Just 31
  find 2 dict' @?= Just 21
  find 4 dict' @?= Just 41
  find 1 dict' @?= Just 11

testMapStrings :: Assertion
testMapStrings = do
  let dict = insert 2 "two" (insert 3 "three" (insert 4 "four" (insert 1 "one" (createDict :: Dict Int String))))
  let dict' = mapDict reverse dict
  find 3 dict' @?= Just "eerht"
  find 2 dict' @?= Just "owt"
  find 4 dict' @?= Just "ruof"
  find 1 dict' @?= Just "eno"

testFoldlPlus :: Assertion
testFoldlPlus = do
  let dict = insert 2 20 (insert 3 30 (insert 4 40 (insert 1 10 (createDict :: Dict Int Int))))
  let folded = foldlDict (\(_, vl) acc -> acc + vl) 0 dict
  folded @?= 100

testFoldrMinus :: Assertion
testFoldrMinus = do
  let dict = insert 2 20 (insert 3 30 (insert 4 40 (insert 1 10 (createDict :: Dict Int Int))))
  let folded = foldrDict (\(_, vl) acc -> vl - acc) 0 dict
  folded @?= -20

testFoldrString :: Assertion
testFoldrString = do
  let dict = insert 2 "two" (insert 3 "three" (insert 4 "four" (insert 1 "one" (createDict :: Dict Int String))))
  let folded = foldrDict (\(_, vl) acc -> vl ++ acc) "lol" dict
  folded @?= "onetwothreefourlol"

testFoldlString :: Assertion
testFoldlString = do
  let dict = insert 2 "two" (insert 3 "three" (insert 4 "four" (insert 1 "one" (createDict :: Dict Int String))))
  let folded = foldlDict (\(_, vl) acc -> acc ++ vl) "" dict
  folded @?= "onetwothreefour"

testFilterEven :: Assertion
testFilterEven = do
  let dict = insert 2 20 (insert 3 31 (insert 4 40 (insert 1 11 (createDict :: Dict Int Int))))
  let filtered = filterDict even dict
  find 2 filtered @?= Just 20
  find 4 filtered @?= Just 40
  find 1 filtered @?= Nothing
  find 3 filtered @?= Nothing