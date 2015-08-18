module Text.Yate.Parser.Internal
  ( templateParser
  , leftDelimiterParser
  , pathParser
  , variableParser
  , ifParser
  , forParser
  , forallParser
  , inParser
  , contentParser
  ) where

import           Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Monad

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Monoid
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL

import           Text.Yate.Types

templateParser :: T.Text -> T.Text -> Parser (Template a)
templateParser l r = do
  parts <- many' $ choice $ map (\f -> f l r)
    [ leftDelimiterParser
    , variableParser
    , ifParser
    , forParser
    , forallParser
    , inParser
    , contentParser
    ]

  return $ case parts of
    [x] -> x
    _   -> Parts parts

whitespaces :: Parser ()
whitespaces = skipMany space

betweenDelimiters :: T.Text -> T.Text -> Parser a -> Parser a
betweenDelimiters l r f =
  (string l *> whitespaces) *> f <* (whitespaces <* string r)

leftDelimiterParser :: T.Text -> T.Text -> Parser (Template a)
leftDelimiterParser l r = do
  _ <- betweenDelimiters l r $ string "left_delimiter"
  return $ Content $ TL.fromChunks [l]

nameParser :: Parser T.Text
nameParser = takeWhile $ \c -> isAlphaNum c || inClass "-~!@#$%^&*_+=;:'?" c

pathParser :: Parser Path
pathParser = do
  names <- nameParser `sepBy1'` char '.'
  return $ case names of
    "" : names' -> RelativePath names'
    _           -> AbsolutePath names

variableParser :: T.Text -> T.Text -> Parser (Template a)
variableParser l r = betweenDelimiters l r $ do
  _ <- string "="
  whitespaces
  Variable <$> pathParser

ifParser :: T.Text -> T.Text -> Parser (Template a)
ifParser l r = do
  path <- betweenDelimiters l r $ string "if" >> whitespaces >> pathParser
  trueBlock  <- templateParser l r
  falseBlock <- option (Content "") $ do
    _ <- betweenDelimiters l r $ string "else"
    templateParser l r
  _ <- betweenDelimiters l r $ string "end"
  return $ If path trueBlock falseBlock

forParser :: T.Text -> T.Text -> Parser (Template a)
forParser l r = do
  (name, path) <- betweenDelimiters l r $ do
    _<- string "for"
    whitespaces
    n <- nameParser
    whitespaces
    _ <- string "in"
    whitespaces
    p <- pathParser
    return (n, p)

  block <- templateParser l r
  _ <- betweenDelimiters l r $ string "end"
  return $ For name path block

forallParser :: T.Text -> T.Text -> Parser (Template a)
forallParser l r = do
  path <- betweenDelimiters l r $ do
    _ <- string "forall"
    whitespaces
    pathParser
  block <- templateParser l r
  _ <- betweenDelimiters l r $ string "end"
  return $ For "_element" path $ In (AbsolutePath ["_element"]) block

inParser :: T.Text -> T.Text -> Parser (Template a)
inParser l r = do
  path <- betweenDelimiters l r $ do
    _ <- string "in"
    whitespaces
    pathParser
  block <- templateParser l r
  _ <- betweenDelimiters l r $ string "end"
  return $ In path block

contentParser :: T.Text -> T.Text -> Parser (Template a)
contentParser l r = do
  when (T.length l == 0) $ fail "invalid zero-length left delimiter"
  txt <- (TL.fromChunks . pure) <$> takeTill (== T.head l)
  when (TL.length txt == 0) empty
  let done = do
        lookAhead (void $ string l) <|> endOfInput
        return $ Content txt
      more = do
        Content txt' <- contentParser l r
        return $ Content $ txt <> txt'
  done <|> more
