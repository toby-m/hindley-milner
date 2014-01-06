{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExtendedDefaultRules #-}
module Main where
import Expression
import Parse
import Control.Monad.State
import qualified Data.Map as Map
default (Int, Integer, Double)

class    MakeLit a      where literal :: a -> Expression
instance MakeLit Int    where literal = Literal . LInt
instance MakeLit Char   where literal = Literal . LChar
instance MakeLit String where literal = Literal . LString
instance MakeLit Bool   where literal = Literal . LBool

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

--main = mapM (putStrLn . showProgram [colours, intList] . readExpr) examples
main = mapM print $ map readData dataDecs ++ [colours, intList]
dataDecs = ["(data Colour Red Green Yellow)"
           ,"(data BoolList Empty (Cons Bool BoolList))"
           ]

examples = [
             "(lambda (x) (y x))"
           , "(lambda (i j) (i j))"  
           , "(lambda (x) (let g Green (if x Red g)))"
           , "((Cons 5) Empty)"
           , "(let x (lambda (x) Green) (if True x ((lambda (x y) Red) 0)))"
           , "Empty"
           , "Cons"
           , "(Cons 9)"
           , "((Cons 9) ((Cons 10) Empty))"
           ]
