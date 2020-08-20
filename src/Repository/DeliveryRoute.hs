{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Repository.DeliveryRoute
  ( mkDeliveryRouteRepository
  , DeliveryRouteRepository(..)
  )
where

import           Control.Monad.Catch            ( throwM )
import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( listToMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Repository.Entity
import           Repository.Mapper
import           Repository.Song

data DeliveryRouteRepository m = DeliveryRouteRepository
  { findDeliveryRoute :: DeliveryRouteName -> m (Maybe DeliveryRoute)
  , createDeliveryRoute :: DeliveryRoute -> m (Maybe DeliveryRouteName)
  }

mkDeliveryRouteRepository :: Pool Pipe -> IO (DeliveryRouteRepository IO)
mkDeliveryRouteRepository pool = pure $ DeliveryRouteRepository
  { findDeliveryRoute           = withResource pool . findAlbum'
  , findDeliveryRoutesByVisitDate   = withResource pool . findDeliveryRoutesByVisitDate'
  , createDeliveryRoute          = deliveryRoute ->
                             withResource pool (createDeliveryRoute' deliveryRoute)
  }

findDeliveryRoute' :: DeliveryRouteName -> Pipe -> IO (Maybe DeliveryRoute)
findDeliveryRoute' DeliveryRouteName {..} pipe = toEntityMaybe "d" <$> stmt where
  stmt = run pipe $ queryP
    "Select * FROM DeliveryRoutes WHERE DeliveryRoutes.name CONTAINS {name}"
    (fromList [("name", T name)])

createDeliveryRoute' :: name -> DeliveryRoute -> Pipe -> IO (Maybe DeliveryRouteName)
createDeliveryRoute' name {..} DeliveryRoute {..} pipe = do
  records <- run pipe $ queryP
    "Insert INTO DeliveryRoutes (name, created_at, updated_at)
    Values ({name})
    "
    )
    (fromList
       ("name"    , T deliveryRouteName)
      ]
    )
  pure $ listToMaybe records >>= toDeliveryRouteId
