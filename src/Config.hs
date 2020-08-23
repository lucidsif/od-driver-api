{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Config where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Dhall
import           GHC.Generics
import qualified GHC.IO.Encoding
import qualified System.IO

data PsqlConfig = psqlConfig
  { psqlHost :: Text
  , psqlPort :: Natural
  , psqlUser :: Text
  , psqlPassword :: Text
  } deriving (Generic, Show)

newtype HttpServerConfig = HttpServerConfig
  { serverPort :: Natural
  } deriving (Generic, Show)

data AppConfig = AppConfig
  { psql :: PsqlConfig
  , httpServer :: HttpServerConfig
  } deriving (Generic, Show)


instance Interpret PsqlConfig
instance Interpret HttpServerConfig
instance Interpret AppConfig

loadConfig :: IO AppConfig
loadConfig = do
  -- Needed to run dhall under nix
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  input auto "./config/app.dhall"
