{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Http.Client.Params where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype AccessToken = AccessToken { unAccessToken :: Text } deriving (Generic, Show)
newtype DriverVisitId = DriverVisitId { unDriverVisitId :: Text } deriving Show
newtype DriverVisitName = DriverVisitName { unDriverVisitName :: Text } deriving Show

newtype DeliveryRouteId = DeliveryRouteId { unDeliveryRouteId :: Text } deriving (Eq, Ord, Show)

instance FromJSON AccessToken where
  parseJSON = withObject "f" $ \v -> AccessToken
    <$> v .: "access_token"
