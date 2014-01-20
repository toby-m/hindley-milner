import Test.HUnit
import qualified Parse.Tests as Parse
import qualified Expression.Tests as Expression
import qualified Unification.Tests as Unification

main = runTestText (PutText printOut 0) $ TestList 
  [ Parse.tests
  , Expression.tests
  , Unification.tests
  ]

printOut :: String -> Bool -> Int -> IO Int
printOut s p _ = if p then putStrLn s >> return 0 else return 0
