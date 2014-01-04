import Expression
import Control.Monad.State
import Data.Char (isAsciiUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit

emptyEnvironment :: Environment
emptyEnvironment = (Map.empty, [])

simpleEnv :: Id -> Type -> Environment
simpleEnv i t = (Map.singleton i t, [])

alphabet = map (:[]) ['a'..'z']
runInference env vars e = snd . fst $ runState (w env e) vars
runEmpty     = runInference emptyEnvironment alphabet
runBound :: Id -> Type -> Expression -> Type
runBound i t = runInference (simpleEnv i t) alphabet

fn :: [Id] -> Type
fn = foldl1 TFunction . map get
  where get id@(i:is) | isAsciiUpper i = TConcrete id
                      | otherwise      = TVar id
q :: Id -> Type -> Type
q = TQuantified

exampleId = Abstraction "id" (Variable "id")
exampleBoundId = Let "id" exampleId (Variable "id")

testSimpleAbstraction = TestCase $ assertEqual
  "Simple abstract typing works" (fn ["a", "a"]) (runEmpty exampleId)

testLetPolymorphism = TestCase $ assertEqual
  "Let binding should give quantified types" (q "b" $ fn ["b", "b"]) (runEmpty exampleBoundId)

testTypeLookup = TestCase $ assertEqual
  "Bound type should be looked up in variable" bound (runBound "bound" bound (Variable "bound"))
  where bound = TVar "bound"

testFtvLookupInEnv = TestCase $ assertEqual
  "Environment with bound type should return it" (Set.singleton "bound") (ftv (simpleEnv "bound" bound))
  where bound = TVar "bound"

testBoundNotQuantified = TestCase $ assertEqual
  "Bound type should not be quantified in let binding" (q "b" $ fn ["b", "bound"]) (runBound "bound" bound exp)
  where
  bound = TVar "bound"
  exp   = Let "toplevel" (Abstraction "x" (Variable "bound")) (Variable "toplevel")

testBoolNotFree = TestCase $ assertEqual
  "Literal integer should not be free" (q "b" $ fn ["b", "Bool"]) (runEmpty exp)
  where exp = Let "toplevel" (Abstraction "x" (Literal $ LBool True)) (Variable "toplevel")

testIntNotFree = TestCase $ assertEqual
  "Literal integer should not be free" (q "b" $ fn ["b", "Int"]) (runEmpty exp)
  where exp = Let "toplevel" (Abstraction "x" (Literal $ LInt 5)) (Variable "toplevel")

main = runTestTT $ TestList
        [ testSimpleAbstraction
        , testLetPolymorphism
        , testTypeLookup
        , testBoundNotQuantified
        , testBoolNotFree
        , testIntNotFree
        ]
