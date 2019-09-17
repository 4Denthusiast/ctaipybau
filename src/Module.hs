module Module (
    initialContext,
    interpretModule,
    processModule,
) where

import Data
import Parser
import Graph
import Interpret

import Control.Monad
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

initialContext :: Context
initialContext = M.fromList [
        ("Int", (TType, Just TInt)),
        ("+", (
            VPi "x" TInt (VPi "y" TInt TInt),
            Just $ VPrimitive "+" (\v -> case v of
                [VInt x, VInt y] -> Just $ VInt (x+y)
                _ -> Nothing) 2 []
        ))
    ]

-- TODO: Keep defined functions separate from pre-existing ones.
interpretModule :: Module2 -> Either InterpreterError Context
interpretModule (Module2 ls defs) = foldM interpretDef initialContext ls
    where interpretDef :: Context -> Label -> Either InterpreterError Context
          interpretDef ctx l = let (Just def) = M.lookup l defs in (\(t,v) -> M.insert l (t, Just v) ctx) <$> evalDefinition ctx def

processModule :: String -> String -> Either InterpreterError Context
processModule n mod = parse n parseModule mod >>= checkReferences initialContext >>= interpretModule
