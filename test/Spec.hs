import qualified PropertyTestRBTree (properties)
import Test.Tasty
import qualified TestDict (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All tests"
    [ TestDict.tests,
      PropertyTestRBTree.properties
    ]
