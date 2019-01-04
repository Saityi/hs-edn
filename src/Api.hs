{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Api where

import Data.Text
import qualified Data.UUID as UUID
import Data.Time.Clock
import EdnParser

newtype ParseError = ParseError String

parseEdnEither :: Text -> Either ParseError EdnElement
parseEdnEither = undefined

parseEdnMaybe :: Text -> Maybe EdnElement
parseEdnMaybe = undefined

class Untag t where
  untag :: String -> EdnElement -> Either ParseError t

instance Untag UUID.UUID where
  untag "uuid" (EdnString s) = case UUID.fromString s of
    Just uuid -> Right uuid
    Nothing   -> Left $ ParseError "invalid uuid"
  untag _ (EdnString _) = Left $ ParseError "UUID parser called, but tag was not uuid"
  untag "uuid" elem = Left $ ParseError $ "Invalid element type. Expected EdnString, got" ++ (show elem)
  untag tag elem = Left $ ParseError $ "All input to UUID tag parse was invalid: " ++ (show tag) ++ ", " ++ (show elem)

untagTaggedElem EdnTaggedElement{..} = untag tag element
