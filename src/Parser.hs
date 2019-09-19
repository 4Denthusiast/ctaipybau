module Parser (
    Module1(..),
    parseModule,
    parseExpr,
    parse
) where

import Data

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Either (partitionEithers)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Text.Parsec hiding (parse, token)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as PC
import Text.Parsec.Combinator (between)

data Module1 = Module1 {typeDecs :: Map Label Expr, declarations :: Map Label Expr}

whitespace :: Parsec String () ()
whitespace = flip label "whitespace" $ void $ many (try space <|> comment)
    where comment = void $ (try (PC.string "--") >> manyTill commentContents (void (lookAhead PC.endOfLine) <|> eof))
                       <|> (try (PC.string "{-") >> manyTill commentContents (try $ PC.string "-}"))
          commentContents = comment <|> void anyToken
          space = many PC.endOfLine >> void (oneOf " \t") -- Starting on new lines is allowed, provided the new line starts with whitespace.

token :: Parsec String () a -> Parsec String () a
token p = try $ whitespace >> p

keyword :: String -> Parsec String () ()
keyword = token . void . PC.string

parseLabel :: Parsec String () Label
parseLabel = token $ flip label "label" $ unreserved $ ((:) <$> PC.letter <*> many (PC.alphaNum <|> PC.oneOf "'_")) <|> many1 (satisfy $ \c -> (isPunctuation c || isSymbol c) && not (elem c ['(',')',',','[',']']))
    where unreserved p = do
              l <- p
              if elem l [":", "->", "λ", "\\", "∏", "'\\", "Type", "="]
                  then parserZero
                  else return l

parseInteger :: Parsec String () Int
parseInteger = token $ do
    minus <- isJust <$> optionMaybe (PC.string "-")
    digits <- many1 PC.digit
    return $ (if minus then (-1) else 1) * foldl (\n d -> n*10 + fromEnum d - fromEnum '0') 0 digits

parseExpr :: Parsec String () Expr
parseExpr = chainr1 parseExpr1 (keyword "->" >> return (EPi "_"))

parseExpr1 :: Parsec String () Expr
parseExpr1 = chainl1 parseTerm (return EApp)

parseTerm :: Parsec String () Expr
parseTerm =
        between (keyword "(") (keyword ")") parseExpr
    <|> EInt <$> parseInteger
    <|> EPi <$> ((keyword "∏" <|> keyword "'\\") >> parseLabel) <*> (keyword ":" >> parseExpr1) <*> (keyword "->" >> parseExpr)
    <|> (flip $ foldr $ uncurry ELambda) <$> ((keyword "λ" <|> keyword "\\") >> sepBy1 ((,) <$> parseLabel <*> (keyword ":" >> parseExpr1)) (keyword ",")) <*> (keyword "->" >> parseExpr)
    <|> (keyword "Type" >> return EType)
    <|> EVariable <$> parseLabel

parseTypeDec :: Parsec String () (Label, Expr)
parseTypeDec = (,) <$> parseLabel <*> (keyword ":" >> parseExpr)

parseDeclaration :: Parsec String () (Label, Expr)
parseDeclaration = (,) <$> parseLabel <*> (keyword "=" >> parseExpr)

parseModule :: Parsec String () Module1
parseModule = (uncurry Module1 . bimap M.fromList M.fromList . partitionEithers . catMaybes) <$> sepBy (Just <$> (Left <$> try parseTypeDec <|> Right <$> try parseDeclaration) <|> (try whitespace >> return Nothing)) PC.endOfLine

parse :: String -> Parsec String () a -> String -> Either InterpreterError a
parse name p s = first ParserError $ Parsec.parse (do x <- p; whitespace; eof; return x) name s
