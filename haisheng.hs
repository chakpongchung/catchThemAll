{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Applicative
import           Data.Aeson
import           Data.Maybe
import           Data.Text                      ( Text )
import           GHC.Generics
import qualified Data.HashMap.Strict           as HashMap


main :: IO ()
main = do
  result <- eitherDecodeFileStrict "./moves.json"
  case (result :: Either String [Move]) of
    Left  error -> print error
    -- Right ms    -> print (length ms)
    Right ms    -> do
      print ms
      print (length ms)

data Move =
  Move
    { num           :: Int
    , accuracy      :: Either Int Bool
    , basePower     :: Int
    , category      :: Text
    , desc          :: Maybe Text
    , shortDesc     :: Text
    , id            :: Text
    , isNonstandard :: Maybe Text
    , isViable      :: Maybe Bool
    , name          :: Text
    , pp            :: Int
    , priority      :: Int
--   ,flags ::   Flags

    , secondary     :: Maybe (Either Bool Secondary)
    }
  deriving (Generic, Show)
--   ['authentic', 'bite', 'bullet', 'charge', 'contact', 
--   'dance', 'defrost', 'distance', 'gravity', 'heal',
--    'mirror', 'mystery', 'nonsky', 'powder', 'protect', 
--    'pulse', 'punch', 'recharge', 'reflectable', 'snatch', 'sound']
-- data Flags =
--   Flags
--     { authentic :: Maybe Int
--     }
--   deriving (Generic, Show)

-- newtype Flags = Flags (HashMap.HashMap Text.Text Int)
--   deriving (Show, Generic)

data Secondary =
  Secondary
    { chance         :: Maybe Int
    , volatileStatus :: Maybe Text
    , boosts         :: Maybe Boosts
    , self           :: Maybe Self
    , status         :: Maybe Text
    }
  deriving (Generic, Show)

data Boosts =
  Boosts
    { atk :: Maybe Int
    , def :: Maybe Int
    , spa :: Maybe Int
    , spd :: Maybe Int
    , spe :: Maybe Int
    }
  deriving (Generic, Show)

data Self =
  Self
    { boosts :: Maybe Boosts
    }
  deriving (Generic, Show)

instance FromJSON Move where
  parseJSON (Object v) =
    Move
      <$> v
      .:  "num"
      <*> ((Left <$> v .: "accuracy") <|> (Right <$> v .: "accuracy"))
      <*> v
      .:  "basePower"
      <*> v
      .:  "category"
      <*> (v .:? "desc")
      <*> v
      .:  "shortDesc"
      <*> (v .: "id")
      <*> (v .:? "isNonstandard")
      <*> (v .:? "isViable")
      <*> (v .: "name")
      <*> (v .: "pp")
      <*> (v .: "priority")

    --   <*> (   fmap (fmap Left)  (v .:? "flags")
    --   <|> fmap (fmap Right) (v .:? "flags")

      <*> (   fmap (fmap Left)  (v .:? "secondary")
          <|> fmap (fmap Right) (v .:? "secondary")
          )

-- instance FromJSON Flags

-- instance FromJSON Flags where
--   parseJSON = withObject "flags" $ \o -> Flags <$> mapM parseJSON o
instance FromJSON Secondary

instance FromJSON Boosts

instance FromJSON Self
