module Unification_Tests (tests) where
import Test.HUnit
import TestHelpers
import Types
import Unification
import Control.Arrow (second)
import qualified Data.Map as Map

empty = Map.empty
single = Map.singleton
pairs = Map.fromList . map (second mk)

simpleUnifyTests = "Simple" ~:
  [ "Variables"     ~: unify (mk "a") (mk "b")         ~?= single "a" (mk "b")
  , "Var & Conc"    ~: unify (mk "a") (mk "B")         ~?= single "a" (mk "B")
  , "Conc & Var"    ~: unify (mk "A") (mk "b")         ~?= single "b" (mk "A")
  , "Matching Conc" ~: unify (mk "A") (mk "A")         ~?= empty
  , "Funcs Var"     ~: unify (fn "a" "b") (mk "x")     ~?= single "x" (fn "a" "b")
  , "Funcs Vars"    ~: unify (fn "a" "b") (fn "c" "d") ~?= pairs [("a", "c"), ("b", "d")]
  , "Funcs Concs"   ~: unify (fn "A" "b") (fn "c" "D") ~?= pairs [("c", "A"), ("b", "D")]
  ]

functionUnifyTests = "Nested Functions" ~:
  [ "Simple"    ~:  unify (ft (mk "a") (fn "b" "c")) (mk "d")
                ~?= single "d" (ft (mk "a") (fn "b" "c"))
  , "Matching"  ~:  unify (ft (ft (fn "a" "b") (fn "c" "d")) (fn "e" "f"))
                          (ft (ft (fn "g" "h") (fn "i" "j")) (fn "k" "h"))
                ~?= pairs [("a", "g"), ("b", "h"), ("c", "i"), ("d", "j"), ("e", "k"), ("f", "h")]
  , "Unmatched" ~:  unify (fn "a" "b") (ft (fn "c" "d") (mk "e"))
                ~?= Map.fromList [("a", fn "c" "d"), ("b", mk "e")]
  ]

tests = "Unification" ~:
  [ simpleUnifyTests
  , functionUnifyTests
  ]
