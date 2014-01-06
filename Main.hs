{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExtendedDefaultRules #-}
module Main where
import Expression
import Control.Monad.State
import qualified Data.Map as Map
default (Int, Integer, Double)

class    MakeLit a      where literal :: a -> Expression
instance MakeLit Int    where literal = Literal . LInt
instance MakeLit Char   where literal = Literal . LChar
instance MakeLit String where literal = Literal . LString
instance MakeLit Bool   where literal = Literal . LBool

exampleId = Abstraction "x" (Variable "x")
example1  = Abstraction "x" (If (Variable "x") (Variable "Red") (Variable "Green"))
example2  = Let "m" (Variable "Cons") (Abstraction "x" (Application (Variable "x") (Variable "m")))
example9  = Abstraction "m" (Let "y" (Variable "m") (Let "x" (Application (Variable "y") (literal 8)) (Variable "x")))
example10 = Let "id" (Abstraction "x" (Let "y" (Variable "x") (Variable "y"))) (Application (Application (Variable "id") (Variable "id")) (literal 2))
example11 = Abstraction "m" (Application (Abstraction "x" (literal 7)) (literal 'g'))
example13 = Abstraction "x" (If (Variable "x") (Variable "x") (Variable "z"))
example15 = Let "id" exampleId (If (Application (Variable "id") (literal False))
                                   (Application (Variable "id") (literal True))
                                   (Application (Variable "id") (literal True)))
example16 = Let "id" exampleId (Block [Application (Variable "id") (literal False), Application (Variable "id") (literal 9)])

colours = DataDeclaration "Colour" [Constructor "Red" [], Constructor "Green" [], Constructor "Blue" []]
intList = DataDeclaration "IntList" [Constructor "Empty" [], Constructor "Cons" ["Int", "IntList"]]

varNames  = [f a b | b <- [0..], a <- ['a'..'z']] where f c n = if n == 0 then [c] else c:show n
inferType e = snd . fst $ runState (w Map.empty e) varNames
showExample e = show e ++ " :: " ++ show (inferType e)

runProgram :: [DataDeclaration] -> Expression -> Type
runProgram ds e = snd $ evalState (w env e) varNames
  where
  cons = foldr1 apply (map getDataType ds)
  env  = Map.map (Scheme []) cons

showProgram :: [DataDeclaration] -> Expression -> String
showProgram ds e = show e ++ " :: " ++ show (runProgram ds e)

main = mapM (putStrLn . showProgram [colours, intList]) examples
examples = [
             example1
           , example2
           , example9
           , example10
           , example11
           , example13
           , example15
           , example16
           ]
