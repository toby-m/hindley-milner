import Test.HUnit
import qualified Parse_Tests as Parse
import qualified Expression_Tests as Expression
import qualified Unification_Tests as Unification

main = runTestText (PutText printOut 0) $ TestList 
  [ Parse.tests
  , Expression.tests
  , Unification.tests
  ]

printOut :: String -> Bool -> Int -> IO Int
printOut s p _ = if p then putStrLn s >> return 0 else return 0
