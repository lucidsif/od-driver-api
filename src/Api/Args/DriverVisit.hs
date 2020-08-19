{-# LANGUAGE DeriveGeneric #-}

module Api.Args.DeliveryRoute where

import           Data.Text                      ( Text )
import           Data.ISO8601Date               ( ISO8601Date)
import           GHC.Generics                   ( Generic )

newtype DriverVisitArgs = DriverVisitArgs
  { location :: Text
  , night_start :: ISO8601Date
  , night_end :: ISO8601Date
  } deriving (Generic, Show)
