{-# LANGUAGE DeriveGeneric #-}

module Api.Args.DeliveryRoute where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype DeliveryRouteArgs = DeliveryRouteArgs
  { name :: Text
  , created_at :: Text
  , updated_at :: Text
  } deriving (Generic, Show)
