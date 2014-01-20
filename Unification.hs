module Unification (unify) where
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set

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

