module Data (
    Label,
    Expr(..),
    Value(..),
    isFree,
    Context(..),
    InterpreterError(..)
) where
{- Split out into its own module so that other modules don't need cyclic dependencies. -}

import Data.Map.Lazy (Map)
import Text.Parsec (ParseError)

type Label = String

data Expr = EVariable Label | EPi Label Expr Expr | ELambda Label Expr Expr | EApp Expr Expr | EInt Int | EType

data Value = VVariable Label | VPi Label Value Value | VLambda Label Value | VApp Value Value | VBlank | VInt Int | TInt | TType | VPrimitive String ([Value] -> Maybe Value) Int [Value] -- TODO: explicitly include arity in VPrimitive rather than leaving it curried, so partially applied versions can be treated properly.

isFree :: Label -> Value -> Bool
isFree l (VVariable l') = l == l'
isFree l (VPi l' a r) = isFree l a || (l /= l' && isFree l r)
isFree l (VLambda l' r) = l /= l' && isFree l r
isFree l (VApp f a) = isFree l f || isFree l a
isFree l VBlank = False
isFree l (VInt _) = False
isFree l TInt = False
isFree l TType = False
isFree l (VPrimitive _ _ _ as) = any (isFree l) as

type Context = Map Label (Value, Maybe Value)

--These are used so much, appreviations help.
ss = showString
sp n = showParen . (n >)

instance Show Expr where
    showsPrec _ (EVariable l) = ss l
    showsPrec n (EPi l a r) = sp n 8 $ ss ("∏"++l++":") . showsPrec 11 a . ss " -> " . showsPrec 8 r
    showsPrec n (ELambda l a r) = sp n 8 $ ss ("λ"++l++":") . showsPrec 11 a . ss " -> " . showsPrec 8 r
    showsPrec n (EApp f a) = sp n 10 $ showsPrec 10 f . ss " " . showsPrec 11 a
    showsPrec _ (EInt i) = shows i
    showsPrec _ EType = ss "Type"

instance Show Value where
    showsPrec _ (VVariable l) = ss l
    showsPrec n (VPi l a r) = sp n 8 $ (if isFree l r then ss ("∏"++l++":") . showsPrec 11 a else showsPrec 9 a) . ss " -> " . showsPrec 8 r
    showsPrec n (VLambda l r) = sp n 8 $ ss ("λ"++l) . showLambda r
        where showLambda (VLambda l' r') = ss (", "++l') . showLambda r'
              showLambda r' = ss " -> " . showsPrec 8 r'
    showsPrec n (VApp f a) = sp n 10 $ showsPrec 10 f . ss " " . showsPrec 11 a
    showsPrec _ VBlank = ss "_"
    showsPrec _ (VInt i) = shows i
    showsPrec _ TInt = ss "Int"
    showsPrec _ TType = ss "Type"
    showsPrec n (VPrimitive l _ _ as) = sp n 10 $ foldl (\s a -> s . ss " " . showsPrec 11 a) (ss l) as

data InterpreterError =
      ParserError ParseError
    | MissingTypeDecError Label
    | MissingDecError Label
    | CyclicDependenciesError [Label]
    | UnknownNameError Label
    | TypeError Value Value -- expected, actual
    deriving Show
