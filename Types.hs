{-# LANGUAGE FlexibleInstances #-}
module Types where
import Data.Char (isAsciiUpper)
import Data.Maybe (fromJust, isJust)
import Data.Tuple (swap)
import qualified Data.Set as Set
import qualified Data.Map as Map

data LiteralValue = LInt Int
                  | LChar Char
                  | LBool Bool
                  | LString String
                  deriving (Eq)

instance Show LiteralValue where
  show (LInt i)    = show i
  show (LChar i)   = show i
  show (LBool i)   = show i
  show (LString i) = show i

literalType :: LiteralValue -> Type
literalType (LInt _)    = TConcrete  "Int"
literalType (LChar _)   = TConcrete  "Char"
literalType (LBool _)   = TConcrete  "Bool"
literalType (LString _) = TConcrete  "String"

type Symbol = String
data Expression = Literal LiteralValue
                | Variable Symbol
                | Application Expression Expression
                | Abstraction Symbol Expression
                | Let Symbol Expression Expression
                | If Expression Expression Expression
                | Block [Expression]
                deriving (Eq)

data Constructor     = Constructor Id [Id] deriving (Show, Eq)
data DataDeclaration = DataDeclaration [Id] [Constructor] deriving (Show, Eq)

getType :: Id -> Type
getType id@(i:_) = if isAsciiUpper i then TConcrete id else TVar id

getDataType :: DataDeclaration -> Environment
getDataType (DataDeclaration (i:ids) cons) =
    let ret = if null ids then TConcrete i else TParam i . map getType $ ids
    in Map.fromList $ map (makeScheme . makeConstructor ret) cons
  where makeScheme (i, t) = (i, Scheme (Set.toList $ ftv t) t)

makeConstructor :: Type -> Constructor -> (Id, Type)
makeConstructor t (Constructor name args) = (name, foldr (TFunction . getType) t args)

instance Show Expression where
  show (Literal i)         = show i
  show (Variable s)        = s
  show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Abstraction s e)   = "λ" ++ s ++ "." ++ show e
  show (Let s e1 e2)       = "let " ++ s ++ " = " ++ show e1 ++ " in " ++ show e2
  show (If c t e)          = "(if " ++ show c ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Block xs)          = "(do \n" ++ unlines (map (("\t" ++ ).show) xs) ++ ")"

type Id = String
data Type = TConcrete Id
          | TVar Id
          | TFunction Type Type
          | TParam Id [Type]
          deriving (Eq)

instance Show Type where
  show (TConcrete i) = i
  show (TVar i)      = i
  show (TFunction t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TParam i ts) = unwords $ i : map show ts

data Scheme = Scheme [Id] Type deriving (Eq)
instance Show Scheme where
  show (Scheme vars t) = concatMap ("∀" ++) vars ++ " => " ++ show t

type Substitution = Map.Map Id Type
type Environment  = Map.Map Id Scheme

class Types a where
  ftv   :: a -> Set.Set Id
  apply :: Substitution -> a -> a

instance Types Environment where
  ftv = ftv . Map.elems
  apply new = Map.map (apply new)

instance Types Type where
  ftv (TConcrete _)     = Set.empty
  ftv (TVar i)          = Set.singleton i
  ftv (TFunction t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TParam _ ts)     = foldr (Set.union.ftv) Set.empty ts
  apply _   t@(TConcrete _)   = t
  apply new t@(TVar i)        = Map.findWithDefault t i new
  apply new (TFunction t1 t2) = TFunction (apply new t1) (apply new t2)
  apply new (TParam i ts)     = TParam i $ map (apply new) ts

instance Types Substitution where
  ftv = Set.fromList . Map.keys
  apply new set = let primary = new `Map.union` Map.map (apply new) set
                      updated = Map.map (apply new) $ invertVarSubs (set `Map.intersection` new)
                  in primary `Map.union` updated

invertVarSubs :: Substitution -> Substitution
invertVarSubs = makeSubs . Map.map fromJust . Map.filter isJust . Map.map extract
  where extract t = case t of (TVar i) -> Just i; otherwise -> Nothing
        makeSubs  = Map.map TVar . Map.fromList . map swap . Map.toList

instance Types Scheme where
  ftv (Scheme vars t)   = ftv t `Set.difference` Set.fromList vars
  apply new (Scheme vars t)  = Scheme vars $ apply (foldr Map.delete new vars) t

instance (Types a) => Types [a] where
  ftv [] = Set.empty
  ftv xs = foldr (Set.union . ftv) Set.empty xs
  apply new = map (apply new)
