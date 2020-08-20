{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies #-}

-- | The GraphQL schema for mutations
module Api.Schema.Mutation
  ( Mutation(..)
  , resolveMutation
  )
where

import           Api.Args.DriverVisit                ( DriverVisitIdArg
                                                , DriverVisitListArgs
                                                )
import qualified Api.Args.DriverVisit               as Args
import           Api.Dependencies               ( Deps(..) )
import           Api.Domain.DeliveryRouteQL             ( DeliveryRouteQL
                                                , toDeliveryRouteQL
                                                )
import           Api.Domain.DriverVisitQL            ( DriverVisitQL
                                                , toDriverVisitQL
                                                )
import           Control.Monad.Catch            ( Exception
                                                , handle
                                                )
import           Data.Morpheus.Types            ( IORes
                                                , resolver
                                                )
import           Data.Text
import           GHC.Generics                   ( Generic )
import qualified Http.Client.Params            as Http
import           Service.DataLoader             ( ExistingDeliveryRouteError(..)
                                                , ExistingDriverVisitError(..)
                                                , createDeliveryRoutes
                                                , createDriverVisits
                                                )

data Mutation = Mutation
  { newDriverVisit :: DriverVisitListArgs -> IORes [DriverVisitQL]
  , newDriverVisitDeliveryRoutes :: DriverVisitIdArg -> IORes [DeliveryRouteQL]
  } deriving Generic

baseHandle
  :: forall a b e
   . Exception e
  => IO [a]
  -> (a -> b)
  -> (e -> Text)
  -> IO (Either String [b])
baseHandle action f g =
  handle (pure . Left . unpack <$> g) ((\x -> Right $ f <$> x) <$> action)

newDriverVisitMutation :: Deps -> DriverVisitListArgs -> IORes [DriverVisitQL]
newDriverVisitMutation Deps {..} args =
  let DriverVisits = Http.DriverVisitName <$> Args.names args
      apiCall = createDriverVisits spotifyClient DriverVisitRepository DriverVisits
      errorFn ExistingDriverVisitError = "Failed to create new DriverVisit"
  in  resolver $ baseHandle apiCall toDriverVisitQL errorFn

newDriverVisitDeliveryRoutesMutation :: Deps -> DriverVisitIdArg -> IORes [DeliveryRouteQL]
newDriverVisitDeliveryRoutesMutation Deps {..} arg =
  let DriverVisitId = Http.DriverVisitId $ Args.spotifyId arg
      apiCall = createDeliveryRoutes spotifyClient DeliveryRouteRepository DriverVisitId
      errorFn ExistingDeliveryRouteError = "Failed to create DeliveryRoutes for DriverVisit"
  in  resolver $ baseHandle apiCall toDeliveryRouteQL errorFn

resolveMutation :: Deps -> Mutation
resolveMutation deps = Mutation
  { newDriverVisit       = newDriverVisitMutation deps
  , newDriverVisitDeliveryRoutes = newDriverVisitDeliveryRoutesMutation deps
  }
