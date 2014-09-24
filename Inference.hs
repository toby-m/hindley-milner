module Inference where
import Types
import Unification (unify)
import Control.Monad.State
import Data.List (delete)
import qualified Data.Set as Set
import qualified Data.Map as Map

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

newVar :: State [Id] Type
newVar = state $ \(s:ss) -> (TVar s, ss)

inst :: Scheme -> State [Id] Type
inst (Scheme vars t) = do newVars <- mapM (\_ -> newVar) vars
                          let s = Map.fromList (zip vars newVars) in return (apply s t)

generalise :: Environment -> Type -> Scheme
generalise env t = let diff = Set.difference (ftv t) (ftv env)
                   in Scheme (Set.toList diff) t
