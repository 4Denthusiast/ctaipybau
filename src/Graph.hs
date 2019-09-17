module Graph (
    Module2(..),
    checkReferences
) where

import Data
import Parser

import Control.Monad
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Text.Parsec (ParseError)

-- There's a distinction between which variables are necessary to determine typing, and which are only necessary for evaluation, which may be useful later, but I'm not using it yet.
allFreeVariables :: Expr -> [Label]
allFreeVariables (EVariable l) = [l]
allFreeVariables (EPi l a b) = union (allFreeVariables a) (delete l $ allFreeVariables b)
allFreeVariables (ELambda l a b) = union (allFreeVariables a) (delete l $ allFreeVariables b)
allFreeVariables (EApp a b) = union (allFreeVariables a) (allFreeVariables b)
allFreeVariables (EInt _) = []
allFreeVariables EType = []

referenceGraph :: Module1 -> Either InterpreterError (Map Label ([Label], Expr, Expr))
referenceGraph (Module1 tdecs decs) = M.fromList <$> mapM record names
    where names = union (M.keys tdecs) (M.keys decs)
          record l = (\tdec dec -> (l, (union (allFreeVariables tdec) (allFreeVariables dec), tdec, dec))) <$> maybe (Left (MissingTypeDecError l)) Right (M.lookup l tdecs) <*> maybe (Left (MissingDecError l)) Right (M.lookup l decs)

topologicalSort :: [Label] -> Map Label ([Label], Expr, Expr) -> Either InterpreterError [Label]
topologicalSort predefs g = drop (length predefs) <$> reverse <$> foldM (searchFrom []) predefs (M.keys g)
    where searchFrom :: [Label] -> [Label] -> Label -> Either InterpreterError [Label]
          searchFrom stack done l =
              if elem l done then Right done else
              if elem l stack then Left (CyclicDependenciesError (l:takeWhile (/= l) stack)) else
              case M.lookup l g of
                  Nothing -> Left (UnknownNameError l)
                  Just (ls, _, _) -> (l:) <$> foldM (searchFrom (l:stack)) done ls

data Module2 = Module2 [Label] (Map Label (Expr, Expr))

checkReferences :: Context -> Module1 -> Either InterpreterError Module2
checkReferences ctx m = do
    m' <- referenceGraph m
    o <- topologicalSort (M.keys ctx) m'
    return (Module2 o ((\(_,t,v) -> (t,v)) <$> m'))
