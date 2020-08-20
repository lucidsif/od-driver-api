{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE BlockArguments, LambdaCase, RecordWildCards #-}

-- | The GraphQL schema for queries
module Api.Schema.Query where

import           Api.Args.DeliveryRoute                 ( DeliveryRouteArgs )
import qualified Api.Args.DeliveryRoute                as DeliveryRouteArgs
import           Api.Args.Artist                ( ArtistArgs )
import qualified Api.Args.Artist               as Args
import           Api.Dependencies               ( Deps(..) )
import           Api.Domain.DeliveryRouteQL             ( DeliveryRouteQL
                                                , toDeliveryRouteQL
                                                )
import           Api.Domain.DriverVisitQL            ( DriverVisitQL
                                                , toDriverVisitQL
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Morpheus.Types            ( IORes
                                                , resolver
                                                )
import           GHC.Generics                   ( Generic )
import           Repository.DeliveryRoute
import           Repository.DriverVisit
import           Repository.Entity              ( DriverVisitName(..) )

data Query = Query
  { DriverVisit :: DriverVisitArgs -> IORes DriverVisitQL
  , DeliveryRoutesByDriverVisit :: DeliveryRouteArgs -> IORes [DeliveryRouteQL]
  } deriving Generic

resolveDriverVisit :: DriverVisitRepository IO -> DriverVisitArgs -> IORes DriverVisitQL
resolveDriverVisit DriverVisitRepository {..} args = resolver result where
  result = findDriverVisit (DriverVisitName $ Args.name args) <&> \case
    Just a  -> Right $ toDriverVisitQL a
    Nothing -> Left "No hits"

resolveDeliveryRoutesByDriverVisit :: DeliveryRouteRepository IO -> DeliveryRouteArgs -> IORes [DeliveryRouteQL]
resolveDeliveryRoutesByDriverVisit DeliveryRouteRepository {..} args = resolver result where
  result = findDeliveryRoutesByDriverVisit (DriverVisitName $ DeliveryRouteArgs.name args) <&> \case
    [] -> Left "No hits"
    xs -> Right $ toDeliveryRouteQL <$> xs

resolveQuery :: Deps -> Query
resolveQuery Deps {..} = Query
  { driverVisit         = resolveDriverVisit Repository
  , DeliveryRoutesByDriverVisit = resolveDeliveryRoutesByArtist DeliveryRouteRepository
  }
