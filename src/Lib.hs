{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Monad              (mzero, void)
import           Data.Functor
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           Debug.Trace
import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as Lex

data EdnElement = EdnNil
                | EdnBool Bool
                | EdnString String
                | EdnChar Char
                | EdnInt Int
                | EdnFloat Float
                | EdnSymbol String
                | EdnPrefixedSymbol
                  { prefix :: String
                  , name   :: String
                  }
                | EdnKeyword String
                | EdnPrefixedKeyword
                  { prefix :: String
                  , name   :: String
                  }
                | EdnTaggedElement
                  { tag     :: String
                  , element :: EdnElement
                  }
                | EdnList [EdnElement]
                | EdnVector (V.Vector EdnElement)
                | EdnSet (S.Set EdnElement)
                | EdnMap (M.Map EdnElement EdnElement)
  deriving (Show, Eq, Ord)

type EdnParser = Parsec Void T.Text

ednWhitespace :: EdnParser ()
ednWhitespace = Lex.space whitespaceConsumer semicolonComment discardPattern
  where semicolonComment   = Lex.skipLineComment ";"
        discardPattern     = P.string "#_" >> void ednParser
        whitespaceConsumer = void $ takeWhile1P (Just "white space") isWhitespace
        isWhitespace c     = (c == ',') || (c == ' ')

ednParser :: EdnParser EdnElement
ednParser = ednWhitespace >>
  -- collections
  listParser <|> setParser <|> vectorParser <|>
  -- numbers
  try floatParser <|> numberParser <|>
  -- bools
  boolParser <|>
  -- nil
  nilParser <|>
  -- chars
  try charSymsParser <|> try unicodeCharParser <|> charParser <|>
  -- strings
  stringParser <|>
  -- identifiers (keywords, symbols)
  divideSymParser <|> keywordParser <|> symbolParser

nilParser :: EdnParser EdnElement
nilParser =
  symbol "nil" $> EdnNil

boolParser :: EdnParser EdnElement
boolParser =
  try (symbol "true"  $> EdnBool True) <|>
       symbol "false" $> EdnBool False

charSymsParser :: EdnParser EdnElement
charSymsParser =
  try (symbol "\\newline" $> EdnChar '\n') <|>
  try (symbol "\\return"  $> EdnChar '\r') <|>
  try (symbol "\\space"   $> EdnChar ' ')  <|>
       symbol "\\tab"     $> EdnChar '\t'

charParser :: EdnParser EdnElement
charParser =
  EdnChar <$> (P.char '\\' >> lexeme P.latin1Char)

unicodeCharParser :: EdnParser EdnElement
unicodeCharParser =
  EdnChar . toEnum <$> (P.char '\\' >> P.char 'u' >> lexeme Lex.decimal)

numberParser :: EdnParser EdnElement
numberParser =
  EdnInt <$> Lex.signed ednWhitespace (lexeme Lex.decimal)

floatParser :: EdnParser EdnElement
floatParser =
  EdnFloat <$> lexeme Lex.float

listParser :: EdnParser EdnElement
listParser =
  EdnList <$> parens (many ednParser)

vectorParser :: EdnParser EdnElement
vectorParser =
  EdnVector . V.fromList <$> brackets (many ednParser)

setParser :: EdnParser EdnElement
setParser =
  EdnSet . S.fromList <$> (P.char '#' >> braces (many ednParser))

divideSymParser :: EdnParser EdnElement
divideSymParser =
  try (P.char '/' $> EdnSymbol "/")
  <|> (P.string "clojure.core//" $> EdnPrefixedSymbol "clojure.core" "/")

identifierParser :: EdnParser String
identifierParser =
  let beginningCharacters   = ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '*', '+', '!', '-', '_', '?', '$', '%', '&', '=', '<', '>']
      constituentCharacters = ':' : '#' : ['0'..'9'] ++ beginningCharacters in do
  startingChar <- oneOf beginningCharacters
  secondChar <- optional $ oneOf (if startingChar `elem` ['-', '+', '.']
    then ':' : '#' : beginningCharacters
    else constituentCharacters)
  rest <- many (oneOf constituentCharacters)
  return $ maybe (startingChar : rest) (\c -> startingChar : c : rest) secondChar

keywordParser :: EdnParser EdnElement
keywordParser = do
  P.char ':'
  name      <- identifierParser
  maybeName <- optional $ P.char '/' >> identifierParser
  return $ maybe (EdnKeyword name) (EdnPrefixedKeyword name) maybeName

symbolParser :: EdnParser EdnElement
symbolParser = do
  name      <- identifierParser
  maybeName <- optional $ P.char '/' >> identifierParser
  return $ maybe (EdnSymbol name) (EdnPrefixedSymbol name) maybeName

stringParser :: EdnParser EdnElement
stringParser = EdnString <$> (P.char '"' >> manyTill Lex.charLiteral (P.char '"'))

lexeme   = Lex.lexeme ednWhitespace
symbol   = Lex.symbol ednWhitespace
brackets = between (symbol "[") (symbol "]")
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")

runParse =
  parseTest ednParser "#{[(:test test :test/test :a.b.c.d/q clojure.core/= \"teststring\" \" \\t \" clojure.core// 1 2 [3 #_(1 2 3) #_[\\q \\q] #_10 4 5.0 3.143221 #{\\a, \\e, \\i, \\o, \\u,}] 10e2 10.32222e-3 ([[#{}]]) ((\\newline \\return \\space \\tab \\u64 \\u126 \\c \\e nil nil nil)) ((1 2) (3 4)))]}"
