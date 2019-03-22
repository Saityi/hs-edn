{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api (Untag(..), parseEdnMaybe, parseEdnEither, parseEdnElements, untagUUID, untagInst, ParseError, EdnElement(..)) where

import           Data.Bifunctor      (first)
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import qualified Data.UUID           as UUID
import           EdnParser
import           Text.Megaparsec     (many, errorBundlePretty, parse, parseMaybe)
import           Text.Read           (readMaybe)

newtype ParseError = ParseError String deriving (Show)

parseEdnEither :: Text -> Either ParseError EdnElement
parseEdnEither edn = first (ParseError . errorBundlePretty) (parse ednParser "" edn)

parseEdnMaybe :: Text -> Maybe EdnElement
parseEdnMaybe = parseMaybe ednParser

parseEdnElements :: Text -> Maybe [EdnElement]
parseEdnElements = parseMaybe (many ednParser)

class Untag t where
  untag :: String -> EdnElement -> Either ParseError t

instance Untag UUID.UUID where
  untag "uuid" (EdnString s) = case UUID.fromString s of
    Just uuid -> Right uuid
    Nothing   -> Left $ ParseError "invalid uuid"
  untag _ (EdnString _) = Left $ ParseError "UUID parser called, but tag was not uuid"
  untag "uuid" elem = Left $ ParseError $ "Invalid element type. Expected EdnString, got" ++ (show elem)
  untag tag elem = Left $ ParseError $ "All input to UUID tag parse was invalid: " ++ (show tag) ++ ", " ++ (show elem)

instance Untag ZonedTime where
  untag "inst" (EdnString s) = case (parseTimeRFC3339 s) of
    Just time -> Right time
    Nothing   -> Left . ParseError $ "inst parse error"
  untag tag elem = Left . ParseError $ "invalid args for inst parsing: " ++ show tag ++ show elem

untagTaggedElem :: Untag t => EdnElement -> Either ParseError t
untagTaggedElem EdnTaggedElement{..} = untag tag element

untagUUID :: EdnElement -> Either ParseError UUID.UUID
untagUUID = untagTaggedElem

untagInst :: EdnElement -> Either ParseError ZonedTime
untagInst = untagTaggedElem
