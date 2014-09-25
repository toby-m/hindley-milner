module Main where
import Inference
import Types
import Parse
import Control.Monad.State
import qualified Data.Map as Map
import Data.List (intercalate)
default (Int, Integer, Double)

-- This file is mostly a test harness from before I got the unit tests up and running.

varNames :: [Symbol]
varNames = [f a b | b <- [0..], a <- ['a'..'z']] where f c n = if n == 0 then [c] else c:show n

runProgram :: [DataDeclaration] -> Expression -> (Substitution, Type)
runProgram ds e = evalState (w env e) varNames
  where env = Map.unions (map getDataType ds)

showProgram :: [DataDeclaration] -> Expression -> String
showProgram ds e = show e ++ " :: " ++ show typ ++ (if null subs then "" else "\n\twith " ++ subs)
  where (ss, typ) = runProgram ds e
        subs = intercalate "; " (map describe . Map.toList $ ss)
        describe (id, sub) = id ++ ": " ++ show sub
        

main = mapM (putStrLn . showProgram dataTypes . readExpr) expressions

dataTypes = map readData dt
    where
    dt = [ "(data (Colour) = Red Green Blue)"
         , "(data (Either a b) = (Left a) (Right b))"
         , "(data (IntList) = Empty (Cons Int IntList))"
         ]

expressions = [ "(lambda (x) (y x))"
              , "(lambda (i j) (i j))"
              , "(lambda (x) (let g Green (if x Red g)))"
              , "(let x (lambda (x) Green) (if True x ((lambda (x y) Red) 0)))"
              , "Red"
              , "Empty"
              , "(Cons 5)"
              , "((Cons 5) Empty)"
              , "(Left 9)"
              , "(Right \"hi\")"
              , "(if #t (Right 'a') (Left 4))"
              , "(if (f (Right #f)) (Right 'a') (Left 4))"
              ]
