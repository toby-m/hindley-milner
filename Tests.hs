import Test.HUnit
import qualified Parse_Tests as Parse
import qualified Types_Tests as Types
import qualified Unification_Tests as Unification
import qualified Inference_Tests as Inference

main = runTestText (PutText printOut 0) $ TestList 
  [ Parse.tests
  , Types.tests
  , Unification.tests
  , Inference.tests
  ]

printOut :: String -> Bool -> Int -> IO Int
printOut s p _ = if p then putStrLn s >> return 0 else return 0
