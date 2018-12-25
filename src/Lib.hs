{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Functor
import Text.Megaparsec
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad (void, mzero)
import Data.Void (Void)

data EdnElement = EdnNil
                | EdnBool Bool
                | EdnString String
                | EdnChar Char
                | EdnInt Int
                | EdnFloat Float
                | EdnKeyword String
                | EdnNamespacedKeyword
                  { namespace :: String
                  , keyword   :: String
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
        discardPattern     = P.string "#_" >> (void ednParser)
        whitespaceConsumer = void $ takeWhile1P (Just "white space") isWhitespace
        isWhitespace c     = (c == ',') || (c == ' ')

lexeme   = Lex.lexeme ednWhitespace
symbol   = Lex.symbol ednWhitespace
brackets = between (symbol "[") (symbol "]")
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")

-- TODO: read #_ ; parse next edn element ; discard it (fail if edn read fail)
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
  EdnInt <$> (Lex.signed ednWhitespace $ lexeme Lex.decimal)

floatParser :: EdnParser EdnElement
floatParser =
  EdnFloat <$> (lexeme Lex.float)

listParser :: EdnParser EdnElement
listParser =
  EdnList <$> (parens $ many ednParser)

vectorParser :: EdnParser EdnElement
vectorParser =
  EdnVector . V.fromList <$> (brackets $ many ednParser)

setParser :: EdnParser EdnElement
setParser =
  EdnSet . S.fromList <$> (P.char '#' >> (braces $ many ednParser))

ednParser :: EdnParser EdnElement
ednParser = listParser
  <|> setParser
  <|> vectorParser
  <|> try floatParser
  <|> numberParser
  <|> boolParser
  <|> nilParser
  <|> try charSymsParser
  <|> try unicodeCharParser
  <|> charParser

runParse =
  parseTest ednParser "#{[(1 2 [3 #_(1 2 3) #_[\\q \\q] #_10 4 5.0 3.143221 #{\\a, \\e, \\i, \\o, \\u,}] 10e2 10.32222e-3 () ((\\newline \\return \\space \\tab \\u64 \\u126 \\c \\e nil nil nil)) ((1 2) (3 4)))]}"
