module Inference_Tests (tests) where
import Inference
import Types
import Parse
import Control.Monad.State
import Control.Arrow (second)
import Data.Char  (isAsciiLower)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit

emptyEnv :: Environment
emptyEnv = Map.empty

simpleEnv :: Id -> Type -> Environment
simpleEnv i t = Map.singleton i (Scheme [] t)

evaluateType env e = evalState (w env e) alphabet
  where alphabet = map (:[]) ['a'..'z']

substitutions = unpackSubs . fst . evaluateType emptyEnv . readExpr
  where
  unpackSubs = Map.toList . Map.map getName
  getName t = case t of
    (TVar i) -> i
    (TConcrete i) -> i

inferWithEnv :: Environment -> String -> Type
inferWithEnv env = runInference env . readExpr
  where runInference env e = snd $ evaluateType env e

inferWith :: Id -> Type -> String -> Type
inferWith i t = inferWithEnv (simpleEnv i t)

infer :: String -> Type
infer = inferWithEnv emptyEnv

mk v@(x:_) = if isAsciiLower x then TVar v else TConcrete v
fn :: [Id] -> Type
fn = foldl1 TFunction . map mk
var = Variable

simpleTypingTests = "Simple Typing" ~:
  [ "Abstraction" ~: fn ["a", "a"] ~=? infer "(lambda (x) x)"
  , "Lookup"      ~: mk "x"        ~=? inferWith "b" (mk "x") "b"
  , "Literals"    ~:
    [ "Bool"      ~: mk "Bool"     ~=? infer "#t"
    , "Int"       ~: mk "Int"      ~=? infer "7"
    , "String"    ~: mk "String"   ~=? infer "\"Hi\""
    , "Char"      ~: mk "Char"     ~=? infer "'f'"
    ]
  ]

variableNamingTests = "Variable Naming" ~:
  [ "id in range"     ~: infer "x"   ~?= mk "x"
  , "id not in range" ~: infer "hi"  ~?= mk "a"
  ]

contains :: (Show a, Eq a) => [(a, a)] -> (a, a) -> Assertion
contains xs (a, b) = assertBool "" (matchingTypes a b xs)
  where pprint = "Expected " ++ show (a,b) ++ " in " ++ show xs

matchingTypes :: (Eq a) => a -> a -> [(a, a)] -> Bool
matchingTypes a b xs = (a, b) `elem` xs || (b, a) `elem` xs || matching
  where matching = isJust (lookup a xs) && lookup a xs == lookup b xs

ifInference = "If Statement" ~:
  [ "Branches"  ~: ifStatementSubs `contains` ("t", "e")
  , "Return"    ~: ifStatementSubs `contains` ("a", "t")
  , "Condition" ~: ifStatementSubs `contains` ("c", "Bool")
  ]
  where ifStatementSubs = substitutions "(if c t e)"

tests = "Expression" ~:
        [ simpleTypingTests
        , variableNamingTests
        , ifInference
        ]
