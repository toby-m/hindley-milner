{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExtendedDefaultRules #-}
module Expression where
import Control.Monad.State
import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map as Map

data LiteralValue = LInt Int
                  | LChar Char
                  | LBool Bool
                  | LString String

literalType (LInt _)    = "Int"
literalType (LChar _)   = "Char"
literalType (LBool _)   = "Bool"
literalType (LString _) = "String"

instance Show LiteralValue where
  show (LInt i)    = show i
  show (LChar i)   = show i
  show (LBool i)   = show i
  show (LString i) = show i

type Symbol = String
data Expression = Literal LiteralValue
                | Variable Symbol
                | Application Expression Expression
                | Abstraction Symbol Expression
                | Let Symbol Expression Expression
                | If Expression Expression Expression
                | Block [Expression]

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
          | TQuantified Id Type
          deriving (Eq)

instance Show Type where
  show (TConcrete i) = i
  show (TVar i)      = i
  show (TFunction t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TQuantified i t) = "∀" ++ i ++ " " ++ show t

type Substitution = Map.Map Id Type
type Environment = (Map.Map Id Type, [Symbol])

class Types a where
  ftv :: a -> Set.Set Id

instance Types Environment where
  ftv (subs, _) = ftv $ Map.elems subs

instance Types Type where
  ftv (TConcrete i)     = Set.empty
  ftv (TVar i)          = Set.singleton i
  ftv (TFunction t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TQuantified i t) = Set.delete i (ftv t)

instance (Types a) => Types [a] where
  ftv [] = Set.empty
  ftv xs = foldr (Set.union . ftv) Set.empty xs

class Subs s where
  apply :: Substitution -> s -> s

instance Subs Substitution where
  apply = Map.union

instance Subs Environment where
  apply new (s, syms) = (apply new s, syms)

instance Subs Type where
  apply new t@(TVar i)        = Map.findWithDefault t i new
  apply new (TFunction t1 t2) = TFunction (apply new t1) (apply new t2)
  apply new (TQuantified i t) = let subs = Map.delete i new
                                in TQuantified i (apply subs t)
  apply new t                 = t

findVar :: Environment -> Symbol -> State [Id] Type
findVar (vars, _) s = case Map.lookup s vars of
                        Just t  -> return t
                        Nothing -> liftM TVar newVar

w :: Environment -> Expression -> State [Id] (Substitution, Type)
w _   (Literal i)  = return (Map.empty, TConcrete $ literalType i)
w env (Variable s) = do i <- findVar env s >>= inst
                        return (Map.empty, i)

w env (Application e1 e2) = do (s1, t1) <- w env e1
                               (s2, t2) <- w (apply s1 env) e2
                               var <- liftM TVar newVar
                               let v = unify (apply s2 t1) (TFunction t2 var)
                                 in return (v `apply` s2 `apply` s1, apply v var)

w env (Abstraction sym e1) = do b <- newVar
                                (s1, t1) <- w (simple sym (TVar b) env) e1
                                let t2 = TFunction (TVar b) t1
                                  in return (s1, apply s1 t2)

w env (Let sym e1 e2) = do (s1, t1) <- w env e1
                           (s2, t2) <- let env1 = apply s1 env
                                           gen  = generalise env1 t1
                                       in w (simple sym gen env1) e2
                           return (apply s2 s1, t2)

w env (If c t e)   = do (s1, c') <- w env c
                        (s2, t') <- w (apply s1 env) t
                        (s3, e') <- w (s2 `apply` s1 `apply` env) e
                        let subs = s3 `apply` s2 `apply` s1
                            v1  = unify (apply subs c') (TConcrete . literalType $ LBool True)
                            v2  = unify (v1 `apply` subs `apply` t') (v1 `apply` subs `apply` e')
                            final = v2 `apply` v1 `apply` subs
                            in return (final, apply final t')

w env (Block [])     = error "Empty block!"
w env (Block [x])    = w env x
w env (Block (x:xs)) = do (s1, e1) <- w env x
                          w (apply s1 env) (Block xs)

simple :: Id -> Type -> Environment -> Environment
simple s t = apply (Map.singleton s t)

generalise :: Environment -> Type -> Type
generalise env t = let diff = Set.difference (ftv t) (ftv env)
                   in wrapQuals t (Set.toList diff)
                   where
                   wrapQuals :: Type -> [Id] -> Type
                   wrapQuals = foldl (flip TQuantified)

unify :: Type -> Type -> Substitution
unify (TConcrete i1) (TConcrete i2) | i1 == i2  = Map.empty
                                    | otherwise = error $ "Mismatched types : " ++ i1 ++ " " ++ i2
unify (TVar i1)  t2                 = Map.singleton i1 t2
unify t1         (TVar i2)          = Map.singleton i2 t1

unify (TFunction t11 t12) (TFunction t21 t22) =
    let s1 = unify t11 t21
        s2 = unify (apply s1 t12) (apply s1 t22)
    in apply s2 s1

unify (TQuantified _ t1) t2 = unify t1 t2
unify t1 (TQuantified _ t2) = unify t1 t2
unify t1 t2 = error $ "unify: " ++ show t1 ++ " " ++ show t2

newVar :: State [Id] Id
newVar = state $ \(s:ss) -> (s, ss)

replace :: Id -> Id -> Type -> Type
replace old new t@(TConcrete i)      = if i == old then TConcrete new else t
replace old new t@(TVar i)           = if i == old then TVar new else t
replace old new (TFunction t1 t2)    = TFunction (replace old new t1) (replace old new t2)
replace old new t@(TQuantified i ti) | i == new = t
                                     | i == old = TQuantified new (replace old new ti)
                                     | otherwise = TQuantified i (replace old new ti)

inst :: Type -> State [Id] Type
inst (TQuantified i t) = do newId <- newVar
                            let inner = replace i newId t in
                                return (TQuantified newId inner)
inst t                 = return t
