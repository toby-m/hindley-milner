import Expression
import Parse (readExpr)
import Control.Monad.State
import Data.Char  (isAsciiUpper)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit

testSimpleLambdaParsing = TestCase $ assertEqual
  "Simple lambda parsing is correct" exp (readExpr "(lambda (x) x)")
  where exp = Abstraction "x" (Variable "x")

printOut :: String -> Bool -> Int -> IO Int
printOut s p _ = if p then putStrLn s >> return 0 else return 0

main = runTestText (PutText printOut 0)$ TestList
        [ testSimpleLambdaParsing
        ]
