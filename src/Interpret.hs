module Interpret (
    Value(..),
    Context(..),
    typecheck,
    reduce,
    evalDefinition,
) where

import Data

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe

-- TODO: Include α-conversion, and possibly other things, in this equality.
instance Eq Value where
    (==) (VVariable l) (VVariable l')  = l == l'
    (==) (VPi l a r) (VPi l' a' r')    = l == l' && a == a' && r == r'
    (==) (VLambda l r) (VLambda l' r') = l == l' && r == r'
    (==) (VApp f a) (VApp f' a')       = f == f' && a == a'
    (==) VBlank VBlank                 = True
    (==) (VInt i) (VInt i')            = i == i'
    (==) TInt TInt                     = True
    (==) TType TType                   = True
    (==) (VPrimitive l _ _ as) (VPrimitive l' _ _ as') = l == l' && as == as'
    (==) _ _                           = False

type Context = Map Label (Value, Maybe Value)

-- Try to unify types. This will need some sort of context later so it can unify blanks with each other.
unify :: Value -> Value -> Either InterpreterError ()
unify t t' = if t == t' then Right () else Left (TypeError t t')

-- typechecks then convert to Value, without performing beta-reduction.
typecheck :: Context -> Expr -> Either InterpreterError (Value, Value)
typecheck ctx (EVariable l) = case (M.lookup l ctx) of
    Nothing -> Left (UnknownNameError l)
    Just (t, mv) -> Right (t, fromMaybe (VVariable l) mv)
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
        _ -> Left $ TypeError ft (VPi "_" VBlank VBlank)
    (at, av) <- typecheck ctx a
    unify fat at
    return (substitute l av frt, VApp fv av)
typecheck ctx (EInt i) = return (TInt, VInt i)
typecheck ctx EType = return (TType, TType)

-- substitute but don't perform further reductions.
substitute :: Label -> Value -> Value -> Value
substitute l v = s
    where s (VVariable l') = if l == l' then v else (VVariable l')
          s (VPi l' a r) = if l == l' then VPi l' (s a) r else VPi l' (s a) (s r)
          s (VLambda l' r) = if l == l' then VLambda l' r else VLambda l' (s r)
          s (VApp f a) = VApp (s f) (s a)
          -- I could just use "s x = x" here, but that would be more risky if I later add more cases to Value.
          s VBlank = VBlank
          s (VInt i) = VInt i
          s TInt = TInt
          s TType = TType
          s (VPrimitive l f n as) = VPrimitive l f n (map s as)

-- substitute and perform any beta-reductions that result.
substituteReduce :: Label -> Value -> Value -> Value
substituteReduce l v = s
    where s (VVariable l') = if l == l' then v else (VVariable l')
          s (VPi l' a r) = if l == l' then VPi l' (s a) r else VPi l' (s a) (s r)
          s (VLambda l' r) = if l == l' then VLambda l' r else VLambda l' (s r)
          s (VApp f a) = headReduce (s f) (s a)
          -- I could just use "s x = x" here, but that would be more risky if I later add more cases to Value.
          s VBlank = VBlank
          s (VInt i) = VInt i
          s TInt = TInt
          s TType = TType
          s (VPrimitive l f n as) = applyPrimitive $ VPrimitive l f n (map s as)

-- Apply beta-reduction in strict order.
reduce :: Value -> Value
reduce (VVariable l) = (VVariable l)
reduce (VPi l a r) = VPi l (reduce a) (reduce r)
reduce (VLambda l r) = VLambda l (reduce r)
reduce (VApp f a) = headReduce (reduce f) (reduce a)
reduce VBlank = VBlank
reduce (VInt i) = VInt i
reduce TInt = TInt
reduce TType = TType
reduce (VPrimitive l f n as) = applyPrimitive $ VPrimitive l f n (map reduce as)

-- Apply the given function to the given argument.
headReduce :: Value -> Value -> Value
headReduce (VLambda l r) a = substituteReduce l a r
headReduce (VPrimitive l f n as) a | length as < n = applyPrimitive $ VPrimitive l f n (as++[a])
headReduce f a = VApp f a -- Can't apply (e.g. f is App or Variable)

-- If possible, turn a VPrimitive and its collected arguments into the result value.
applyPrimitive :: Value -> Value
applyPrimitive p@(VPrimitive _ f _ as) = fromMaybe p (f as)
applyPrimitive v = error "applyPrimitive is only meant to be applied to VPrimitives."

evalDefinition :: Context -> (Expr, Expr) -> Either InterpreterError (Value, Value)
evalDefinition ctx (te, ve) = do
    (tt, tv) <- typecheck ctx te
    unify tt TType
    (vt, vv) <- typecheck ctx ve
    unify vt tv
    return (tv, vv)
