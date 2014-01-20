module Expression_Tests (tests) where
import Expression
import Control.Monad.State
import Data.Char  (isAsciiUpper)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

simpleEnv :: Id -> Type -> Environment
simpleEnv i t = Map.singleton i (Scheme [] t)

alphabet = map (:[]) ['a'..'z']
evaluateType env vars e = evalState (w env e) vars
runInference env vars e = snd $ evaluateType env vars e 
getSubs e = fst $ evaluateType emptyEnvironment alphabet e 
runEmpty     = runInference emptyEnvironment alphabet
runBound :: Id -> Type -> Expression -> Type
runBound i t = runInference (simpleEnv i t) alphabet

fn :: [Id] -> Type
fn = foldl1 TFunction . map get
  where get id@(i:is) | isAsciiUpper i = TConcrete id
                      | otherwise      = TVar id
exampleId = Abstraction "id" (Variable "id")
exampleBoundId = Let "id" exampleId (Variable "id")

testSimpleAbstraction = TestCase $ assertEqual
  "Simple abstract typing works" (fn ["a", "a"]) (runEmpty exampleId)

testTypeLookup = TestCase $ assertEqual
  "Bound type should be looked up in variable" bound (runBound "bound" bound (Variable "bound"))
  where bound = TVar "bound"

testFtvLookupInEnv = TestCase $ assertEqual
  "Environment with bound type should return it" (Set.singleton "bound") (ftv (simpleEnv "bound" bound))
  where bound = TVar "bound"

testBoolNotFree = TestCase $ assertEqual
  "Literal integer should not be free" (fn ["b", "Bool"]) (runEmpty exp)
  where exp = Let "toplevel" (Abstraction "x" (Literal $ LBool True)) (Variable "toplevel")

testIntNotFree = TestCase $ assertEqual
  "Literal integer should not be free" (fn ["b", "Int"]) (runEmpty exp)
  where exp = Let "toplevel" (Abstraction "x" (Literal $ LInt 5)) (Variable "toplevel")

testVariableNames = TestCase $ assertEqual
  "Should select var name if available in list" (TVar "x") (runEmpty (Variable "x"))

testSubstitionApplication1 = TestCase $ assertEqual
  "Should apply substitution correctly" [("a", "b"), ("c", "d")] (unpackSubs $ apply s1 s2)
  where
  s1 = Map.singleton "a" (TVar "b")
  s2 = Map.singleton "c" (TVar "d")

testSubstitionApplication2 = TestCase $ assertEqual
  "Substitution application overrides" [("a", "b")] (unpackSubs $ apply s2 s1)
  where
  s1 = Map.singleton "a" (TVar "g")
  s2 = Map.singleton "a" (TVar "b")

testSubstitionApplication3 = TestCase $ assertContains
  "Substitution application overrides inner type" ("a", "g") (unpackSubs $ apply s2 s1)
  where
  s1 = Map.singleton "a" (TVar "b")
  s2 = Map.singleton "b" (TVar "g")

unpackSubs = Map.toList . Map.map getName
  where
  getName t = case t of 
    (TVar i) -> i
    (TConcrete i) -> i

assertContains :: (Show a, Eq a) => String -> (a, a) -> [(a, a)] -> Assertion
assertContains s (a, b) xs = assertBool pprint (matchingTypes a b xs)
  where pprint = s ++ "\n" ++ "Expected " ++ show (a,b) ++ " in " ++ show xs

matchingTypes :: (Eq a) => a -> a -> [(a, a)] -> Bool
matchingTypes a b xs = (a, b) `elem` xs || (b, a) `elem` xs || matching
  where matching = isJust (lookup a xs) && lookup a xs == lookup b xs

testWUnifiesIfBranches = TestCase $ assertContains
  "If expression should have both branches unified" ("t", "e") (unpackSubs $ getSubs exp)
  where exp = If (Variable "c") (Variable "t") (Variable "e")

testWUnifiesThenAndWhole = TestCase $ assertContains
  "If expression should have type unified with then branch" ("a", "t") (unpackSubs $ getSubs exp)
  where exp = If (Variable "c") (Variable "t") (Variable "e")

testWUnifiesIfCondAndBool = TestCase $ assertContains
  "If expression should have condition being bool" ("c", "Bool") (unpackSubs $ getSubs exp)
  where exp = If (Variable "c") (Variable "t") (Variable "e")

tests = "Expression" ~: TestList
        [ testSimpleAbstraction
        , testTypeLookup
        , testBoolNotFree
        , testIntNotFree
        , testVariableNames 
        , testSubstitionApplication1
        , testSubstitionApplication2
        , testSubstitionApplication3
        , testWUnifiesIfBranches 
        , testWUnifiesThenAndWhole 
        , testWUnifiesIfCondAndBool 
        ]
