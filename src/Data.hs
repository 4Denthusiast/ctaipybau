module Data (
    Label,
    Expr(..),
    Value(..),
    InterpreterError(..)
) where
{- Split out into its own module so that other modules don't need cyclic dependencies. -}

import Text.Parsec (ParseError)

type Label = String

data Expr = EVariable Label | EPi Label Expr Expr | ELambda Label Expr Expr | EApp Expr Expr | EInt Int | EType

data Value = VVariable Label | VPi Label Value Value | VLambda Label Value | VApp Value Value | VBlank | VInt Int | TInt | TType | VPrimitive String (Value -> Maybe Value) -- TODO: explicitly include arity in VPrimitive rather than leaving it curried, so partially applied versions can be treated properly.

instance Show Expr where
    show (EVariable l) = l
    show (EPi l a r) = "∏"++l++" : "++show a++" -> "++show r
    show (ELambda l a r) = "λ"++l++" : "++show a++" -> "++show r
    show (EApp f a) = "("++show f++") ("++show a++")"
    show (EInt i) = show i
    show EType = "Type"

instance Show Value where
    show (VVariable l) = l
    show (VPi l a r) = "∏"++l++" : "++show a++" -> "++show r
    show (VLambda l r) = "λ"++l++" -> "++show r
    show (VApp f a) = "("++show f++") ("++show a++")"
    show VBlank = "_"
    show (VInt i) = show i
    show TInt = "Int"
    show TType = "Type"
    show (VPrimitive n f) = n

data InterpreterError =
      ParserError ParseError
    | MissingTypeDecError Label
    | MissingDecError Label
    | CyclicDependenciesError [Label]
    | UnknownNameError Label
    | TypeError Value Value -- expected, actual
    deriving Show
