{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Http.Client.Response where

import           Data.Aeson
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )

newtype TrackItem = TrackItem
  { trackDurationMs :: Int
  } deriving (Generic, Show)

newtype TrackResponse = TrackResponse
  { trackItems :: [TrackItem]
  } deriving (Generic, Show)

data DeliveryRouteItem = DeliveryRouteItem
  { deliveryRouteId :: Text
  , deliveryRouteName :: Text
  } deriving (Eq, Generic, Show)

instance Ord DeliveryRouteItem where
  (DeliveryRouteItem id1 _ _) `compare` (DeliveryRouteItem id2 _ _) = id1 `compare` id2

newtype DeliveryRouteResponse = DeliveryRouteResponse
  { DeliveryRouteItems :: [DeliveryRouteItem]
  } deriving (Generic, Show)

data DriverVisitItem = DriverVisitItem
  { driverVisitId :: Text
  , driverVisitName :: Text
  } deriving (Generic, Show)

newtype DriverVisitObject = DriverVisitObject
  { driverVisitItems :: [DriverVisitItem]
  } deriving (Generic, Show)

-- TODO: See if this can be simplified
newtype DriverVisitResponse = DriverVisitResponse
  { driverVisitObject :: DriverVisitObject
  } deriving (Generic, Show)

instance FromJSON TrackItem where
  parseJSON = withObject "item" $ \v -> TrackItem
    <$> v .: "duration_ms"

instance FromJSON TrackResponse where
  parseJSON = withObject "items" $ \v -> TrackResponse
    <$> v .: "items"

instance FromJSON DeliveryRouteItem where
  parseJSON = withObject "item" $ \v -> DeliveryRouteItem
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "release_date"

instance FromJSON DeliveryRouteResponse where
  parseJSON = withObject "items" $ \v -> DeliveryRouteResponse
    <$> v .: "items"

instance FromJSON DriverVisitItem where
  parseJSON = withObject "item" $ \v -> DriverVisitItem
    <$> v .: "id"
    <*> v .: "name"

instance FromJSON DriverVisitObject where
  parseJSON = withObject "items" $ \v -> DriverVisitObject
    <$> v .: "items"

instance FromJSON DriverVisitResponse where
  parseJSON = withObject "driverVisits" $ \v -> DriverVisitResponse
    <$> v .: "driverVisits"
