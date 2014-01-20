module Unification_Tests (tests) where
import Test.HUnit
import Expression
import Data.Char (isAsciiLower)
import Control.Arrow (second)
import qualified Data.Map as Map

pairs = Map.fromList . map (second mk)
empty = Map.empty
single = Map.singleton
mk v@(x:_) = if isAsciiLower x then TVar v else TConcrete v
ft = TFunction
f a b = ft (mk a) (mk b)

simpleUnifyTests = "Simple" ~: TestList
  [ "Variables"     ~: unify (mk "a") (mk "b")     ~?= single "a" (mk "b")
  , "Var & Conc"    ~: unify (mk "a") (mk "B")     ~?= single "a" (mk "B")
  , "Conc & Var"    ~: unify (mk "A") (mk "b")     ~?= single "b" (mk "A")
  , "Matching Conc" ~: unify (mk "A") (mk "A")     ~?= empty
  , "Funcs Var"     ~: unify (f "a" "b") (mk "x")   ~?= single "x" (f "a" "b")
  , "Funcs Vars"    ~: unify (f "a" "b") (f "c" "d") ~?= pairs [("a", "c"), ("b", "d")]
  , "Funcs Concs"   ~: unify (f "A" "B") (f "c" "d") ~?= pairs [("c", "A"), ("d", "B")]
  ]

functionUnifyTests = "Nested Functions" ~:
  [ "Simple"    ~:  unify (ft (mk "a") (f "b" "c")) (mk "d")
                ~?= single "d" (ft (mk "a") (f "b" "c"))
  , "Matching"  ~:  unify (ft (ft (f "a" "b") (f "c" "d")) (f "e" "f"))
                          (ft (ft (f "g" "h") (f "i" "j")) (f "k" "h"))
                ~?= pairs [("a", "g"), ("b", "h"), ("c", "i"), ("d", "j"), ("e", "k"), ("f", "h")]
  , "Unmatched" ~:  unify (f "a" "b") (ft (f "c" "d") (mk "e"))
                ~?= Map.fromList [("a", f "c" "d"), ("b", mk "e")]
  ]

tests = "Unification" ~: TestList
  [ simpleUnifyTests
  , functionUnifyTests
  ]
