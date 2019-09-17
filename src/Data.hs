module Data (
    Label,
    Expr(..),
    Value(..),
    Context(..),
    InterpreterError(..)
) where
{- Split out into its own module so that other modules don't need cyclic dependencies. -}

import Data.Map.Lazy (Map)
import Text.Parsec (ParseError)

type Label = String

data Expr = EVariable Label | EPi Label Expr Expr | ELambda Label Expr Expr | EApp Expr Expr | EInt Int | EType

data Value = VVariable Label | VPi Label Value Value | VLambda Label Value | VApp Value Value | VBlank | VInt Int | TInt | TType | VPrimitive String ([Value] -> Maybe Value) Int [Value] -- TODO: explicitly include arity in VPrimitive rather than leaving it curried, so partially applied versions can be treated properly.

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
    showsPrec n (VPi l a r) = sp n 8 $ ss ("∏"++l++":") . showsPrec 11 a . ss " -> " . showsPrec 8 r
    showsPrec n (VLambda l r) = sp n 8 $ ss ("λ"++l++" -> ") . showsPrec 8 r
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
