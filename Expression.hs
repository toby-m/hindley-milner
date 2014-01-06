{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExtendedDefaultRules #-}
module Expression where
import Control.Monad.State
import Control.Applicative
import Data.Char (isAsciiUpper)
import Data.List (delete)
import qualified Data.Set as Set
import qualified Data.Map as Map

data LiteralValue = LInt Int
                  | LChar Char
                  | LBool Bool
                  | LString String

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

data Constructor     = Constructor Id [Id] deriving Show
data DataDeclaration = DataDeclaration  Id [Constructor] deriving Show

getDataType :: DataDeclaration  -> Substitution
getDataType (DataDeclaration name cons) = Map.fromList $ map (makeConstructor name) cons 

makeConstructor :: Id -> Constructor -> (Id, Type)
makeConstructor t (Constructor name args) = (name, foldr (TFunction . get) (get t) args)
  where get id@(i:is) | isAsciiUpper i = TConcrete id
                      | otherwise      = TVar id

instance Show Expression where
  show (Literal i)         = show i
  show (Variable s)        = s
  show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Abstraction s e)   = "λ" ++ s ++ "." ++ show e
  show (Let s e1 e2)       = "let " ++ s ++ " = " ++ show e1 ++ " in " ++ show e2
  show (If c t e)          = "(if " ++ show c ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Block xs)          = "(do \n" ++ unlines (map show xs) ++ ")"

type Id = String
data Type = TConcrete Id
          | TVar Id
          | TFunction Type Type
          deriving (Eq)

instance Show Type where
  show (TConcrete i) = i
  show (TVar i)      = i
  show (TFunction t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

data Scheme = Scheme [Id] Type
instance Show Scheme where
  show (Scheme vars t) = concatMap ("∀" ++) vars ++ " " ++ show t

type Substitution = Map.Map Id Type
type Environment  = Map.Map Id Scheme

class Types a where
  ftv   :: a -> Set.Set Id
  apply :: Substitution -> a -> a

instance Types Environment where
  ftv = ftv . Map.elems
  apply new = Map.map (apply new)

instance Types Type where
  ftv (TConcrete i)     = Set.empty
  ftv (TVar i)          = Set.singleton i
  ftv (TFunction t1 t2) = ftv t1 `Set.union` ftv t2
  apply new t@(TVar i)        = Map.findWithDefault t i new
  apply new (TFunction t1 t2) = TFunction (apply new t1) (apply new t2)
  apply new t                 = t

instance Types Substitution where
  ftv = Set.fromList . Map.keys
  apply new set = new `Map.union` Map.map (apply new) set

instance Types Scheme where
  ftv (Scheme vars t)   = ftv t `Set.difference` Set.fromList vars
  apply new (Scheme vars t)  = Scheme vars $ apply (foldr Map.delete new vars) t

instance (Types a) => Types [a] where
  ftv [] = Set.empty
  ftv xs = foldr (Set.union . ftv) Set.empty xs
  apply new = map (apply new)

findVar :: Environment -> Symbol -> State [Id] Scheme
findVar vars s = case Map.lookup s vars of
        Just t  -> return t
        Nothing -> getVar s
    where
    getVar :: Id -> State [Id] Scheme
    getVar id = state $ \(s:ss) -> let (i, is) = findName id (s:ss) in case i of
      Just newId -> (Scheme [] (TVar newId), is)
      Nothing    -> (Scheme [] (TVar s), ss)
    findName :: Id -> [Id] -> (Maybe Id, [Id])
    findName id ids = let range = take 100 ids
      in if id `elem` range
         then (Just id, delete id ids)
         else (Nothing, ids)

w :: Environment -> Expression -> State [Id] (Substitution, Type)
w _   (Literal i)  = return (Map.empty, literalType i)
w env (Variable s) = do i <- findVar env s >>= inst
                        return (Map.empty, i)

w env (Application e1 e2) = do (s1, t1) <- w env e1
                               (s2, t2) <- w (apply s1 env) e2
                               var <- newVar
                               let v = unify (apply s2 t1) (TFunction t2 var)
                                 in return (v `apply` s2 `apply` s1, apply v var)

w env (Abstraction sym e1) = do b <- newVar
                                (s1, t1) <- w (simple sym (Scheme [] b) env) e1
                                let t2 = TFunction b t1
                                  in return (s1, apply s1 t2)

w env (Let sym e1 e2) = do (s1, t1) <- w env e1
                           (s2, t2) <- let env1 = apply s1 env
                                           gen  = generalise env1 t1
                                       in w (simple sym gen env1) e2
                           return (apply s2 s1, t2)

w env (If c t e)   = do (s1, c') <- w env c
                        (s2, t') <- w (apply s1 env) t
                        (s3, e') <- w (s2 `apply` s1 `apply` env) e
                        it       <- newVar
                        let subs = s3 `apply` s2 `apply` s1
                            v1 = unify (apply subs c') (literalType $ LBool True)
                            v2 = unify (v1 `apply` subs `apply` t') (v1 `apply` subs `apply` e')
                            v3 = unify (v1 `apply` v2 `apply` subs `apply` t') it
                            final = v3 `apply` v2 `apply` v1 `apply` subs
                            in return (final, apply final it)

w env (Block [])     = error "Empty block!"
w env (Block [x])    = w env x
w env (Block (x:xs)) = do (s1, e1) <- w env x
                          w (apply s1 env) (Block xs)

simple :: Id -> Scheme -> Environment -> Environment
simple = Map.insert

unify :: Type -> Type -> Substitution
unify (TConcrete i1) (TConcrete i2) | i1 == i2  = Map.empty
                                    | otherwise = error $ "Mismatched types : " ++ i1 ++ " " ++ i2
unify (TVar i1)  t2                 = unifySimple i1 t2
unify t1         (TVar i2)          = unifySimple i2 t1
unify (TFunction t11 t12) (TFunction t21 t22) =
    let s1 = unify t11 t21
        s2 = unify (apply s1 t12) (apply s1 t22)
    in apply s2 s1
unify t1 t2 = error $ "unify: " ++ show t1 ++ " " ++ show t2

unifySimple :: Id -> Type -> Substitution
unifySimple i t = if i `Set.member` ftv t
                  then error $ "Failed to make infinite type " ++ i ++ " => " ++ show t
                  else Map.singleton i t

newVar :: State [Id] Type
newVar = state $ \(s:ss) -> (TVar s, ss)

inst :: Scheme -> State [Id] Type
inst (Scheme vars t) = do newVars <- mapM (\_ -> newVar) vars
                          let s = Map.fromList (zip vars newVars) in return (apply s t)

generalise :: Environment -> Type -> Scheme
generalise env t = let diff = Set.difference (ftv t) (ftv env)
                   in Scheme (Set.toList diff) t
