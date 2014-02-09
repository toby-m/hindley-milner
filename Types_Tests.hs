module Types_Tests (tests) where
import Types
import Parse (readData)
import Control.Arrow (second)
import Data.Char  (isAsciiLower)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit

-- Empty environment
emptyEnv :: Environment
emptyEnv = Map.empty

-- Single bound type var
simpleEnv :: Id -> Type -> Environment
simpleEnv i t = Map.singleton i (Scheme [] t)

-- Type (concrete or var)
mk :: Id -> Type
mk v@(x:_) = if isAsciiLower x then TVar v else TConcrete v

-- Function type
ft :: Type -> Type -> Type
ft = TFunction

-- Function type
fn :: Id -> Id -> Type
fn a b = ft (mk a) (mk b)

-- Single substituion
sub :: Symbol -> Id -> Substitution
sub a b = Map.singleton a (mk b)

-- Multiple substitutions
subs :: [(Symbol, Id)] -> Substitution
subs = Map.fromList . map (second mk)

-- Empty Scheme
es :: Type -> Scheme
es = Scheme []

-- Scheme
forAll :: Id -> Type -> Scheme
forAll a = Scheme [a]

-- Constructor
cons = Constructor

ftvTests = "Free type variables" ~:
  [ "Environment" ~:
    [ "Empty"     ~: empty            ~=? ftv emptyEnv
    , "Free"      ~: single "f"       ~=? ftv (simpleEnv "v" (mk "f"))
    , "Concrete"  ~: empty            ~=? ftv (simpleEnv "v" (mk "C"))
    ]
  , "Type"        ~:
    [ "Var"       ~: single "a"       ~=? ftv (mk "a")
    , "Concrete"  ~: empty            ~=? ftv (mk "A")
    , "Func 1"    ~: single "a"       ~=? ftv (fn "B" "a")
    , "Func 2"    ~: single "a"       ~=? ftv (fn "a" "B")
    , "Func 3"    ~: items ["a", "b"] ~=? ftv (fn "a" "b")
    ]
  , "Scheme"      ~:
    [ "Empty"     ~: single "a"       ~=? ftv (es (mk "a"))
    , "Masked"    ~: empty            ~=? ftv (forAll "a" (mk "a"))
    , "Mixed"     ~: single "b"       ~=? ftv (forAll "a" (fn "a" "b"))
    ]
  ]
  where
  empty  = Set.empty
  single = Set.singleton
  items  = Set.fromList

applyTests = "Apply" ~:
  [ "Substitution"   ~:
    [ "Combine"      ~: subs [("a", "b"), ("c", "d")] ~=? apply (sub "a" "b") (sub "c" "d")
    , "Override"     ~: sub "a" "b"                   ~=? apply (sub "a" "b") (sub "a" "G")
    , "Inner"        ~: subs [("a","g"),("b","g")]    ~=? apply (sub "b" "g") (sub "a" "b")
    ]
  , "Type"           ~:
    [ "Var"          ~: mk "b"                        ~=? apply (sub "a" "b") (mk "a")
    , "Concrete"     ~: mk "A"                        ~=? apply (sub "A" "B") (mk "A")
    , "Function"     ~:
      [ "Return"     ~: fn "a" "c"                    ~=? apply (sub "b" "c") (fn "a" "b")
      , "Argument"   ~: fn "c" "b"                    ~=? apply (sub "a" "c") (fn "a" "b")
      , "Nested"     ~: ft (mk "a") (fn "d" "c")      ~=? apply (sub "b" "d") (ft (mk "a") (fn "b" "c"))
      ]
    ]
  , "Scheme"         ~:
    [ "Empty"        ~: es (mk "b")                   ~=? apply (sub "a" "b") (es (mk "a"))
    , "Masked"       ~: forAll "a" (mk "a")               ~=? apply (sub "a" "b") (forAll "a" (mk "a"))
    , "Mixed"        ~: forAll "a" (fn "a" "c")           ~=? apply (sub "b" "c") (forAll "a" (fn "a" "b"))
    ]
  ]

dataConsTests = "Data Constructors" ~:
  [ "Variable" ~: ("C", fn "b" "a") ~=? makeConstructor (mk "a") (cons "C" ["b"])
  , "Concrete" ~: ("C", fn "B" "A") ~=? makeConstructor (mk "A") (cons "C" ["B"])
  , "Mixed"    ~: ("C", ft (mk "a") $ ft (mk "b") $ fn "c" "D")
               ~=? makeConstructor (mk "D") (cons "C" ["a", "b", "c"])
  ]

getDataTypeTests = "Data Types" ~:
  [ "Maybe Nothing" ~: Map.singleton "N" schemeN                     ~=? getDataType maybeN
  , "Maybe Just"    ~: Map.singleton "J" schemeJ                     ~=? getDataType maybeJ
  , "Maybe Full"    ~: Map.fromList [("J", schemeJ), ("N", schemeN)] ~=? getDataType maybe
  ]
  where
    decl     = DataDeclaration . words
    param    = TParam . map mk
    maybeN   = decl "M a" [cons "N" []]
    maybeJ   = decl "M a" [cons "J" ["a"]]
    maybe    = decl "M a" [cons "N" [], cons "J" ["a"]]
    schemeJ  = forAll "a" $ ft (mk "a") (param ["M", "a"])
    schemeN  = forAll "a" $ param ["M", "a"]

tests = "Expression" ~:
        [ applyTests
        , ftvTests
        , dataConsTests
        , getDataTypeTests
        ]
