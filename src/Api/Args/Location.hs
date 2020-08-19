{-# LANGUAGE DeriveGeneric #-}

module Api.Args.Location where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype LocationArgs = LocationArgs
  { business_name   :: Text
  , address :: Text
  } deriving Generic
