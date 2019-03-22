{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}
module EdnParser where

import           Control.Monad              (void)
import           Data.Functor               (($>))
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Data.Void                  (Void)
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
                | EdnNamespacedMap String (M.Map EdnElement EdnElement)
  deriving (Eq, Ord)

instance Show EdnElement where
  show = \case
    EdnNil        -> "nil"
    EdnBool True  -> "true"
    EdnBool False -> "false"
    EdnString s   -> show s
    EdnChar '\n'  -> "\\newline"
    EdnChar '\r'  -> "\\return"
    EdnChar ' '   -> "\\space"
    EdnChar '\t'  -> "\\tab"
    EdnChar c     -> '\\' : c : ""
    EdnInt i      -> show i
    EdnFloat f    -> show f
    EdnSymbol s   -> s
    EdnKeyword s  -> ":" ++ s

    EdnPrefixedSymbol{..} ->
      prefix ++ "/" ++ name

    EdnPrefixedKeyword{..} ->
      ":" ++ prefix ++ "/" ++ name

    EdnTaggedElement{..} ->
      "#" ++ tag ++ " " ++ (show element)

    EdnList elems ->
      "("  ++ (L.intercalate ", " . fmap show $ elems) ++ ")"

    EdnVector elemsVec ->
      "[" ++ (L.intercalate ", " . fmap show . V.toList $ elemsVec) ++ "]"

    EdnSet elemsSet ->
      "#{" ++ (L.intercalate ", " . fmap show . S.toList $ elemsSet) ++ "}"

    EdnMap elemsMap ->
      "{" ++ (L.intercalate ", " . fmap (\(k, v) -> show k ++ " " ++ show v) . M.toList $ elemsMap) ++ "}"

    EdnNamespacedMap ns m ->
      "#:" ++ ns ++ show (EdnMap m)

type EdnParser = Parsec Void T.Text

ednWhitespace :: EdnParser ()
ednWhitespace = Lex.space whitespaceConsumer semicolonComment discardPattern
 where
  semicolonComment   = Lex.skipLineComment ";"
  discardPattern     = P.string "#_" >> optional (P.char ' ') >> void ednParser
  whitespaceConsumer = void $ takeWhile1P (Just "white space") isWhitespace
  isWhitespace c = c `elem` [' ', ',', '\t', '\n', '\r']

ednParser :: EdnParser EdnElement
ednParser =
  ednWhitespace >>
  -- collections
        listParser
    <|> vectorParser
    <|> mapParser
    <|> try setParser
    <|> namespacedMapParser
  -- Tagged elements
    <|> taggedElementParser
  -- numbers
    <|> try floatParser
    <|> numberParser
  -- bools
    <|> boolParser
  -- nil
    <|> nilParser
  -- chars
    <|> try charSymsParser
    <|> try unicodeCharParser
    <|> charParser
  -- strings
    <|> stringParser
  -- identifiers (keywords, symbols)
    <|> divideSymParser
    <|> keywordParser
    <|> symbolParser

nilParser :: EdnParser EdnElement
nilParser = symbol "nil" $> EdnNil

boolParser :: EdnParser EdnElement
boolParser = try (symbol "true" $> EdnBool True) <|> symbol "false" $> EdnBool False

charSymsParser :: EdnParser EdnElement
charSymsParser =
  try (symbol "\\newline" $> EdnChar '\n')
    <|> try (symbol "\\return" $> EdnChar '\r')
    <|> try (symbol "\\space" $> EdnChar ' ')
    <|> (symbol "\\tab" $> EdnChar '\t')

charParser :: EdnParser EdnElement
charParser = EdnChar <$> (P.char '\\' >> lexeme P.latin1Char)

unicodeCharParser :: EdnParser EdnElement
unicodeCharParser = EdnChar . toEnum <$> (P.char '\\' >> P.char 'u' >> lexeme Lex.decimal)

numberParser :: EdnParser EdnElement
numberParser = EdnInt <$> Lex.signed ednWhitespace (lexeme Lex.decimal)

floatParser :: EdnParser EdnElement
floatParser = EdnFloat <$> lexeme Lex.float

listParser :: EdnParser EdnElement
listParser = EdnList <$> parens (many ednParser)

vectorParser :: EdnParser EdnElement
vectorParser = EdnVector . V.fromList <$> brackets (many ednParser)

setParser :: EdnParser EdnElement
setParser = EdnSet . S.fromList <$> (P.char '#' >> braces (many ednParser))

mapParser :: EdnParser EdnElement
mapParser = EdnMap . M.fromList . pairs <$> braces (many ednParser)
-- TODO: Fail mapParser if not even number of elements

namespacedMapParser :: EdnParser EdnElement
namespacedMapParser =
-- TODO: Fail namespacedMapParser if not even number of elements
  EdnNamespacedMap
    <$> ((P.string "#:") >> identifierParser)
    <*> ((optional $ P.char ' ') >> (M.fromList . pairs <$> braces (many ednParser)))

divideSymParser :: EdnParser EdnElement
divideSymParser =
  try (P.char '/' $> EdnSymbol "/")
    <|> (P.string "clojure.core//" $> EdnPrefixedSymbol "clojure.core" "/")

identifierParser :: EdnParser String
identifierParser = do
  startingChar <- oneOf beginningCharacters
  let allowedSecondChars = if startingChar `elem` ['-', '+', '.']
        then ':' : '#' : beginningCharacters
        else constituentCharacters
  secondChar <- optional $ oneOf allowedSecondChars
  -- TODO: If the second char exists but isn't valid, fail
  -- right now, if the *parser* fails but a char exists, this returns None
  -- and will result in an invalid identifier rather than a parsing error
  rest       <- many (oneOf constituentCharacters)
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
-- TODO: More specific escape sequences to edn

taggedElementParser :: EdnParser EdnElement
taggedElementParser = do
  P.char '#'
  ident     <- identifierParser
  maybeRest <- optional (P.char '/' >> identifierParser)
  optional (P.char ' ')
  EdnTaggedElement (maybe ident ((ident ++ "/") ++) maybeRest) <$> ednParser

lexeme = Lex.lexeme ednWhitespace
symbol = Lex.symbol ednWhitespace
brackets = between (symbol "[") (symbol "]")
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

letters = ['a' .. 'z'] ++ ['A' .. 'Z']
nums = ['0' .. '9']
allowedChars = ['.', '*', '+', '!', '-', '_', '?', '$', '%', '&', '=', '<', '>']
beginningCharacters = letters ++ allowedChars
constituentCharacters = ':' : '#' : nums ++ beginningCharacters

-- TODO: How can I capture this works on only even-length lists *at compile time*?
pairs :: [a] -> [(a, a)]
pairs []         = []
pairs (x:y:rest) = (x, y) : pairs rest

-- TODO: Actual tests
runParse = parseTest
  ednParser
  "#{[(1 2 #_ 3 :test #inst\"date-time\" #:test{test \"name\"} #myapp/Person {:first \"Fred\" :last \"Mertz\"} #inst \"1985-04-12T23:20:50.52Z\" #uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\" test :test/test :a.b.c.d/q clojure.core/= \"teststring\" \" \\t \" clojure.core// 1 2 [3 #_(1 2 3) #_[\\q \\q] #_10 4 5.0 3.143221 #{\\a, \\e, \\i, \\o, \\u,}] 10e2 10.32222e-3 ([[#{}]]) ((\\newline \\return \\space \\tab \\u64 \\u126 \\c \\e nil nil nil)) ((1 2) (3 4)))]}"
