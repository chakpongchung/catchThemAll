{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Main where

import System.Directory
import Control.Applicative
import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  result <- eitherDecodeFileStrict (cwd ++ "/movers.json")
  case ( result :: Either String [Move]) of
    Left err -> print err
    Right ms -> print (length ms)

data Move = Move
  { num :: Int
  , accuracy :: Either Int Bool
  , basePower :: Int
  , category :: Text
  , desc :: Maybe Text
  , shortDesc :: Text
  , id :: Text
  , isNonstandard :: Maybe Text
  , isViable :: Maybe Bool
  , name :: Text
  , pp :: Int
  , priority :: Int
   ,flags :: Flags
  , secondary :: Maybe (Either Bool Secondary)
  } deriving (Generic, Show)

data Secondary = Secondary
  { chance :: Maybe Int
  , volatileStatus :: Maybe Text
  , boosts :: Maybe Boosts
  , self :: Maybe Self
  , status :: Maybe Text
  } deriving (Generic, Show)

data Boosts = Boosts
  { atk :: Maybe Int
  , def :: Maybe Int
  , spa :: Maybe Int
  , spd :: Maybe Int
  , spe :: Maybe Int
  } deriving (Generic, Show)

data Self = Self
  { boosts :: Maybe Boosts
  } deriving (Generic, Show)

data Flags = Flags
  { authentic :: Maybe Int
  , bite :: Maybe Int
  , bullet :: Maybe Int
  , charge :: Maybe Int
  , contact :: Maybe Int
  , dance :: Maybe Int
  , defrost :: Maybe Int
  , distance :: Maybe Int
  , gravity :: Maybe Int
  , heal :: Maybe Int
  , mirror :: Maybe Int
  , mystery :: Maybe Int
  , nonsky :: Maybe Int
  , powder :: Maybe Int
  , protect :: Maybe Int
  , pulse :: Maybe Int
  , punch :: Maybe Int
  , recharge :: Maybe Int
  , reflectable :: Maybe Int
  , snatch :: Maybe Int
  , sound :: Maybe Int
  } deriving (Generic, Show)

instance FromJSON Move where
  parseJSON (Object v) = Move
    <$> v .: "num"
    <*> (   (Left  <$> v .: "accuracy")
        <|> (Right <$> v .: "accuracy")
        )
    <*> v .: "basePower"
    <*> v .: "category"
    <*> (v .:? "desc")
    <*> v .: "shortDesc"
    <*> v .: "id"
    <*> v .:? "isNonstandard"
    <*> v .:? "isViable"
    <*> v .: "name"
    <*> v .: "pp"
    <*> v .: "priority"
    <*> v .: "flags"
    <*> (   fmap (fmap Left)  (v .:? "secondary")
        <|> fmap (fmap Right) (v .:? "secondary")
        )
  parseJSON invalid = prependFailure
                      "parsing Coord failed, "
                      (typeMismatch "Object" invalid)

instance FromJSON Flags
instance FromJSON Secondary
instance FromJSON Boosts
instance FromJSON Self
