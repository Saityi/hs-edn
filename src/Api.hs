{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api where

import           Data.Bifunctor  (first)
import           Data.Text
import           Data.Time.Clock
import qualified Data.UUID       as UUID
import           EdnParser
import           Text.Megaparsec (errorBundlePretty, parse, parseMaybe)

newtype ParseError = ParseError String deriving (Show)

parseEdnEither :: Text -> Either ParseError EdnElement
parseEdnEither edn = first (ParseError . errorBundlePretty) (parse ednParser "" edn)

parseEdnMaybe :: Text -> Maybe EdnElement
parseEdnMaybe = parseMaybe ednParser

class Untag t where
  untag :: String -> EdnElement -> Either ParseError t

instance Untag UUID.UUID where
  untag "uuid" (EdnString s) = case UUID.fromString s of
    Just uuid -> Right uuid
    Nothing   -> Left $ ParseError "invalid uuid"
  untag _ (EdnString _) = Left $ ParseError "UUID parser called, but tag was not uuid"
  untag "uuid" elem = Left $ ParseError $ "Invalid element type. Expected EdnString, got" ++ (show elem)
  untag tag elem = Left $ ParseError $ "All input to UUID tag parse was invalid: " ++ (show tag) ++ ", " ++ (show elem)

untagTaggedElem :: Untag t => EdnElement -> Either ParseError t
untagTaggedElem EdnTaggedElement{..} = untag tag element
