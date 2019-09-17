module Repl (
    repl
) where

import Data
import Parser
import Interpret
import Module

import Data.Bifunctor (first)
import System.Exit
import System.IO

data ReplState = ReplState Context

initialState :: ReplState
initialState = ReplState initialContext

processLine :: ReplState -> IO ReplState
processLine (ReplState ctx) = do -- IO
    line <- getLine
    case line of
        ":q" -> exitSuccess
        ":r" -> return initialState
        (':':'l':' ':mn) -> do
            mod <- readFile mn
            case processModule mn mod of
                Left err -> putStrLn (show err) >> return (ReplState ctx)
                Right ctx' -> return (ReplState ctx')
        _ -> do
            r <- return (do -- Either InterpreterError
                exp <- parse "repl input" parseExpr line
                (t, v) <- typecheck ctx exp
                return (reduce t, reduce v))
            case r of
                Left err -> putStrLn (show err)
                Right (t, v) -> putStrLn ("_ : "++show t) >> putStrLn ("_ = "++show v)
            return (ReplState ctx)

repl :: IO ()
repl = repl' initialState
    where repl' s = do
              putStr "> "
              hFlush stdout
              s' <- processLine s
              repl' s'
