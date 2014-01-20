module Parse.Tests (tests) where
import Expression
import Parse (readExpr, readData)
import Test.HUnit

exprTests = "Expressions" ~: TestList
  [ "Variable"       ~: readExpr "x"                  ~?= var "x"
  , "Lambda"         ~: readExpr "(lambda (x) x)"     ~?= Abstraction "x" (var "x")
  , "If"             ~: readExpr "(if c t f)"         ~?= If (var "c") (var "t") (var "f")
  , "Block"          ~: readExpr "(block c t f)"      ~?= Block [var "c", var "t", var "f"]
  , "Let"            ~: readExpr "(let x y z)"        ~?= Let "x" (var "y") (var "z")
  , "Application"    ~: readExpr "(x y)"              ~?= Application (var "x") (var "y")
  , "Literal bool"   ~: readExpr "#t"                 ~?= Literal (LBool True)
  , "Literal char"   ~: readExpr "'m'"                ~?= Literal (LChar 'm')
  , "Literal int"    ~: readExpr "5"                  ~?= Literal (LInt 5)
  , "Literal string" ~: readExpr "\"Hello\""          ~?= Literal (LString "Hello")
  ]
  where var = Variable

dataTests = "Data declarations" ~: TestList
  [ "Data Single"    ~: readData "(data A B)"         ~?= dt "A" [enum "B"]
  , "Data Enum"      ~: readData "(data C R B)"       ~?= dt "C" [enum "R", enum "B"]
  , "Data IntList"   ~: readData "(data L E (C I L))" ~?= dt "L" [enum "E", con "C" ["I", "L"]]
  ]
  where
  con = Constructor
  enum a = Constructor a []
  dt = DataDeclaration

tests = "Parsing" ~: TestList [ exprTests, dataTests ]
