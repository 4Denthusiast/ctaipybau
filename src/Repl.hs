module Repl (
    repl
) where

import Parser
import Graph (InterpreterError(..))
import Interpret
import Module (initialContext)

import Data.Bifunctor (first)
import System.IO
import Text.Parsec (parse)

data ReplState = ReplState Context

processLine :: ReplState -> IO ReplState
processLine (ReplState ctx) = do -- IO
    line <- getLine
    r <- return (do -- Either InterpreterError
        exp <- first ParserError $ parse parseExpr "repl input" line
        (t, v) <- typecheck ctx exp
        return (reduce t, reduce v))
    case r of
        Left err -> putStrLn (show err)
        Right (t, v) -> putStrLn ("_ : "++show t) >> putStrLn ("_ = "++show v)
    return (ReplState ctx)

repl :: IO ()
repl = repl' initialState
    where initialState = ReplState initialContext
          repl' s = do
              putStr "> "
              hFlush stdout
              s' <- processLine s
              repl' s'
