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

paramUnifyTests = "Parameterised Types" ~:
  [ "Self"        ~:  unify (param ["a"]) (param ["a"]) ~?= empty
  , "Param & Var" ~:  unify (param ["a"]) (mk "b")      ~?= single "b" (param ["a"])
  , "Var & Param" ~:  unify (mk "b") (param ["a"])      ~?= single "b" (param ["a"])
  , "Simple"      ~:  unify (param ["a"]) (param ["b"]) ~?= single "a" (mk "b")
  , "Concrete"    ~:  unify (param ["A"]) (param ["b"]) ~?= single "b" (mk "A")
  , "Long"        ~:  unify (param ["a", "b", "c"]) (param ["d", "e", "f"])
                  ~?= pairs [("a", "d"), ("b", "e"), ("c", "f")]
  , "Ignored"     ~:  unify (param ["a", "b"]) (param ["d", "b"])
                  ~?= pairs [("a", "d")]
  , "Chained"     ~:  unify (param ["a", "b"]) (param ["b", "c"])
                  ~?= pairs [("a", "c"), ("b", "c")]
  , "Dupes"       ~:  unify (param ["a", "c"]) (param ["b", "b"])
                  ~?= pairs [("a", "b"), ("c", "b")]
  , "Mapped"      ~:  unify (param ["a", "c"]) (param ["b", "a"])
                  ~?= pairs [("a", "b"), ("c", "b")]
  ]
  where param = TParam "M" . map mk

tests = "Unification" ~:
  [ simpleUnifyTests
  , functionUnifyTests
  , paramUnifyTests
  ]
