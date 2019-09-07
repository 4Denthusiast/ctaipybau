module Interpret (
    Value(..),
    Context(..),
    typecheck,
    reduce,
    evalDefinition,
) where

import Parser
import Graph (InterpreterError(..))

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe

data Value = VVariable Label | VPi Label Value Value | VLambda Label Value | VApp Value Value | VBlank | VInt Int | TInt | TType | VPrimitive String (Value -> Maybe Value) -- TODO: explicitly include arity in VPrimitive rather than leaving it curried, so partially applied versions can be treated properly.

-- TODO: Include α-conversion, and possibly other things, in this equality.
instance Eq Value where
    (==) (VVariable l) (VVariable l')       = l == l'
    (==) (VPi l a r) (VPi l' a' r')         = l == l' && a == a' && r == r'
    (==) (VLambda l r) (VLambda l' r')      = l == l' && r == r'
    (==) (VApp f a) (VApp f' a')            = f == f' && a == a'
    (==) VBlank VBlank                      = True
    (==) (VInt i) (VInt i')                 = i == i'
    (==) TInt TInt                          = True
    (==) TType TType                        = True
    (==) (VPrimitive n _) (VPrimitive n' _) = n == n'
    (==) _ _                                = False

instance Show Value where
    show (VVariable l) = l
    show (VPi l a r) = "Π"++l++" : "++show a++" -> "++show r
    show (VLambda l r) = "λ"++l++" -> "++show r
    show (VApp f a) = "("++show f++") ("++show a++")"
    show VBlank = "_"
    show (VInt i) = show i
    show TInt = "Int"
    show TType = "Type"
    show (VPrimitive n f) = n

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
    (at, av) <- typecheck ctx f
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
          s (VPrimitive n f) = VPrimitive n f

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

-- Apply the given function to the given argument.
headReduce :: Value -> Value -> Value
headReduce (VLambda l r) a = substituteReduce l a r
headReduce (VPrimitive n f) a = fromMaybe (VApp (VPrimitive n f) a) (f a)
headReduce f a = VApp f a -- Can't apply (e.g. f is App or Variable)

evalDefinition :: Context -> (Expr, Expr) -> Either InterpreterError (Value, Value)
evalDefinition ctx (te, ve) = do
    (tt, tv) <- typecheck ctx te
    unify tt TType
    (vt, vv) <- typecheck ctx ve
    unify vt tv
    return (tv, vv)
