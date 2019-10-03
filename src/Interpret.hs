module Interpret (
    typecheck,
    reduce,
    evalDefinition,
) where

import Data

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Debug.Trace

instance Eq Value where
    (==) (VVariable l) (VVariable l')  = l == l'
    (==) (VPi l a r) (VPi l' a' r')    = a == a' && r == substitute l' (VVariable l) r'
    (==) (VLambda l r) (VLambda l' r') = r == substitute l' (VVariable l) r'
    (==) (VApp f a) (VApp f' a')       = f == f' && a == a'
    (==) (VBlank t i) (VBlank t' i')   = t == t' && i == i'
    (==) (VInt i) (VInt i')            = i == i'
    (==) TInt TInt                     = True
    (==) TType TType                   = True
    (==) (VPrimitive l _ _ as) (VPrimitive l' _ _ as') = l == l' && as == as'
    (==) _ _                           = False

-- Something that does not occur free in any of the values.
newVariable :: [Value] -> Label
newVariable vs = head $ filter (\l -> not $ any (isFree l) vs) $ map (('v':).show) [0..]

data Constraint = BlankVal Bool Int Value

data UnificationContext = UContext{nextLabel::Int, constraints::[Constraint]}

blankValue :: Bool -> Int -> UnificationContext -> Maybe Value
blankValue t i uc = listToMaybe $ catMaybes (constraintMatch t i) (constraints u)
    where constraintMatch t i (BlankVal t' i' v) = if t == t' && i == i' then Just v else nothing

type TypecheckingContext = StateT UnificationContext (Either InterpreterError)

-- Try to unify types.
-- I'll try later to not always immediately reduce it, in case the existing version is simpler.
-- TODO: avoid variable capture by applying α-conversion where the variable exists in the unification context in the VPi and VLambda cases. Also perform the substitution in typecheck. (There's probably a neater way to do this.)
unify :: Value -> Value -> TypecheckingContext ()
unify x y = unify' (headReduce x) (headReduce y)
    where unify' (VPi l a r) (VPi l' a' r') = unify a a' >> unify r (substitute l' (VVariable l) r')
          unify' (VLambda l r) (VLambda l' r') = unify r (substitute l' (VVariable l) r')
          unify' (VApp f a) (VApp f' a') = unify f f' >> unify a a'
          unify' (VPrimitive l _ _ as) (VPrimitive l' _ _ as') | l == l' = sequence $ zipWith unify as as'
          unify' t t' | t == t' = return ()
          unify' x (VBlank t i) = unify' (VBlank t i) x
          unify' (VBlank t i) x = do
              v <- gets (blankValue t i)
              case v of -- TODO: catch circularity.
                  Nothing -> modify (BlankVal t i x :)
                  Just x' -> unify' x x'
          unify' t t' = Left (TypeError t t')

newBlank :: TypecheckingContext (Value, Value)
newBlank = do
    i <- gets nextLabel
    modify (\u -> u{nextLabel = i+1})
    return (VBlank True i, VBlank False i)

-- typechecks then convert to Value, without performing beta-reduction.
typecheck :: Context -> Expr -> TypecheckingContext (Value, Value)
typecheck ctx (EVariable l) = case (M.lookup l ctx) of
    Nothing -> lift $ Left (UnknownNameError l) -- This would be an interpreter bug in a module, but could occur in the REPL.
    Just (t, mv) -> return (t, fromMaybe (VVariable l) mv)
typecheck ctx (EPi l at rt) = do
    (att, at') <- typecheck ctx at
    unify att TType
    (rtt, rt') <- typecheck (M.insert l (at',Nothing) ctx) rt
    unify rtt TType
    return (TType, VPi l at' rt')
typecheck ctx (ELambda l ate re) = do
    (att, atv) <- typecheck ctx ate
    unify att TType
    (rt, rv) <- typecheck (M.insert l (atv,Nothing) ctx) re
    return (VPi l atv rt, VLambda l rv)
typecheck ctx (EApp f a) = do
    (ft, fv) <- typecheck ctx f
    (l, fat, frt) <- case ft of
        (VPi l' fat' frt') -> return (l', fat', frt')
        _ -> lift $ Left $ TypeError ft (VPi "_" VBlank VBlank) -- TODO: use unify instead.
    (at, av) <- typecheck ctx a
    unify fat at
    return (substitute l av frt, VApp fv av)
typecheck ctx EBlank = newBlank
typecheck ctx (EInt i) = return (TInt, VInt i)
typecheck ctx EType = return (TType, TType)

-- substitute but don't perform further reductions.
substitute :: Label -> Value -> Value -> Value
substitute l v = if (v == VVariable l) then id else s
    where s (VVariable l') = if l == l' then v else (VVariable l')
          s (VPi l' a r) = if l == l' then VPi l' (s a) r else VPi l' (s a) (s r)
          s (VLambda l' r) = if l == l' then VLambda l' r
              else if (isFree l' v) then let l'' = newVariable [v,r] in VLambda l'' (s (substitute l' (VVariable l'') r))
              else VLambda l' (s r)
          s (VApp f a) = VApp (s f) (s a)
          -- I could just use "s x = x" here, but that would be more risky if I later add more cases to Value.
          s (VBlank t i) = VBlank t i
          s (VInt i) = VInt i
          s TInt = TInt
          s TType = TType
          s (VPrimitive l f n as) = VPrimitive l f n (map s as)

-- Apply beta-reduction in strict order.
reduce :: Value -> Value
reduce (VVariable l) = (VVariable l)
reduce (VPi l a r) = VPi l (reduce a) (reduce r)
reduce (VLambda l r) = VLambda l (reduce r)
reduce (VApp f a) = headReduce (VApp (reduce f) (reduce a))
reduce (VBlank t i) = VBlank t i
reduce (VInt i) = VInt i
reduce TInt = TInt
reduce TType = TType
reduce (VPrimitive l f n as) = headReduce $ VPrimitive l f n (map reduce as)

-- Apply the given function to the given argument.
headReduce :: Value -> Value
headReduce (VApp (VLambda l r) a) = reduce $ substitute l a r
headReduce (VApp (VPrimitive l f n as) a) | length as < n = headReduce $ VPrimitive l f n (as++[a])
headReduce p@(VPrimitive _ f _ as) = fromMaybe p (f as)
headReduce x = x -- Can't apply (e.g. (VApp f a) where f is App or Variable)

evalDefinition :: Context -> (Expr, Expr) -> Either InterpreterError (Value, Value)
evalDefinition ctx (te, ve) = do
    (tt, tv) <- typecheck ctx te
    unify tt TType
    (vt, vv) <- typecheck ctx ve
    unify vt tv
    return (tv, vv)
