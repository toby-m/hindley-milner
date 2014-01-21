module TestHelpers where
import Types
import Control.Arrow (second)
import Data.Char  (isAsciiLower)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Empty environment
emptyEnv :: Environment
emptyEnv = Map.empty

-- Single bound type var
simpleEnv :: Id -> Type -> Environment
simpleEnv i t = Map.singleton i (Scheme [] t)

-- Type (concrete or var)
mk :: Id -> Type
mk v@(x:_) = if isAsciiLower x then TVar v else TConcrete v

-- Function type
ft :: Type -> Type -> Type
ft = TFunction

-- Function type
fn :: Id -> Id -> Type
fn a b = ft (mk a) (mk b)

-- Single substituion
sub :: Symbol -> Id -> Substitution
sub a b = Map.singleton a (mk b)

-- Multiple substitutions
subs :: [(Symbol, Id)] -> Substitution
subs = Map.fromList . map (second mk)

-- Empty Scheme
es :: Type -> Scheme
es = Scheme []

-- Scheme
sc :: Id -> Type -> Scheme
sc a = Scheme [a]
