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
        

main = mapM (putStrLn . showProgram [] . readExpr) expressions

dataTypes = [ "(data Colour = Red | Green | Blue)"
            , "(data List a = (Cons a (List a)) | Empty)"
            ]

expressions = [ "(lambda (x) (y x))"
              , "(lambda (i j) (i j))"
              , "(lambda (x) (let g Green (if x Red g)))"
              , "((Cons 5) Empty)"
              , "(let x (lambda (x) Green) (if True x ((lambda (x y) Red) 0)))"
              , "Empty"
              , "Cons"
              , "(Cons 9)"
              , "((Cons 9) ((Cons 10) Empty))"
              ]
