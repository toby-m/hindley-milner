import Test.HUnit
import qualified Parse.Tests as Parse
import qualified Expression.Tests as Expression

main = runTestText (PutText printOut 0) $ TestList 
  [ Parse.tests
  , Expression.tests
  ]

printOut :: String -> Bool -> Int -> IO Int
printOut s p _ = if p then putStrLn s >> return 0 else return 0
