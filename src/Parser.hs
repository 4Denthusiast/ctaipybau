module Parser (
    Label,
    Expr(..),
    Module1(..),
    parseModule,
    parseExpr
) where

import Data.Bifunctor
import Data.Char
import Data.Either (partitionEithers)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Token

type Label = String

data Expr = EVariable Label | EPi Label Expr Expr | ELambda Label Expr Expr | EApp Expr Expr | EInt Int | EType

data Module1 = Module1 {typeDecs :: Map Label Expr, declarations :: Map Label Expr}

tp :: TokenParser ()
tp = makeTokenParser $ LanguageDef{
        commentStart = "{-",
        commentEnd = "-}",
        commentLine = "--",
        nestedComments = True,
        identStart = satisfy (not . isSpace),
        identLetter = satisfy (not . isSpace),
        opStart = fail "no operators in language def",
        opLetter = fail "no operators in language def",
        reservedNames = [":","Π","Σ","=","->","\\","Type"],
        reservedOpNames = ["->"],
        caseSensitive = True
    }

parseLabel :: Parsec String () Label
parseLabel = identifier tp

parseExpr :: Parsec String () Expr
parseExpr = chainl1 parseTerm (return EApp)

parseTerm :: Parsec String () Expr
parseTerm =
        parens tp parseExpr
    <|> (EInt . fromIntegral) <$> integer tp
    <|> EVariable <$> parseLabel
    <|> EPi <$> (reserved tp "Π" >> parseLabel) <*> (reserved tp ":" >> parseExpr) <*> (reserved tp "->" >> parseExpr)
    <|> ELambda <$> (reserved tp "\\" >> parseLabel) <*> (reserved tp ":" >> parseExpr) <*> (reserved tp "->" >> parseExpr)
    <|> (reserved tp "Type" >> return EType)

parseTypeDec :: Parsec String () (Label, Expr)
parseTypeDec = (,) <$> parseLabel <*> (reserved tp ":" >> parseExpr)

parseDeclaration :: Parsec String () (Label, Expr)
parseDeclaration = (,) <$> parseLabel <*> (reserved tp "=" >> parseExpr)

parseModule :: Parsec String () Module1
parseModule = (uncurry Module1 . bimap M.fromList M.fromList . partitionEithers . catMaybes) <$> sepBy (return Nothing <|> Just <$> (Left <$> parseTypeDec <|> Right <$> parseDeclaration)) (string "\n")
