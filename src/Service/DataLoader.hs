{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Service.DataLoader
  ( ExistingDeliveryRouteError(..)
  , ExistingDriverVisitError(..)
  , createDeliveryRoutes
  , createDriverVisits
  )
where

import           Config
import           Control.Concurrent.Async       ( mapConcurrently
                                                , mapConcurrently_
                                                )
import           Control.Monad.Catch            ( Exception
                                                , throwM
                                                )
import           Data.Functor                   ( void )
import           Data.List                      ( sort )
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Http.Client.Params             ( DeliveryRouteId(..)
                                                , DriverVisitId(..)
                                                , DriverVisitName(..)
                                                , AccessToken
                                                )
import qualified Http.Client.Response          as R
import           Http.Client.Response           ( DeliveryRouteItem(..)
                                                , DeliveryRouteResponse(..)
                                                , DriverVisitItem(..)
                                                , TrackResponse(..)
                                                )
import           Repository.DeliveryRoute
import           Repository.DriverVisit
import qualified Repository.Entity             as E
import           Repository.Entity              ( DeliveryRoute
                                                , DriverVisit
                                                )
import           Repository.Song
import           Text.Read                      ( readMaybe )

data ExistingDeliveryRouteError = ExistingDeliveryRouteError deriving Show
data ExistingDriverVisitError = ExistingDriverVisitError deriving Show

instance Exception ExistingDeliveryRouteError
instance Exception ExistingDriverVisitError

createDriverVisits
  :: DriverVisitRepository IO -> [DriverVisit] -> IO [DriverVisit]
createDriverVisits client driverVisitRepo names = do
  verifyDriverVisitDoesNotExist driverVisitRepo names
  token   <- login client
  driverVisits <- getDriverVisitsByName client token names
  persistDriverVisits driverVisits driverVisitRepo
  pure driverVisits

createDeliveryRoutes :: DeliveryRouteRepository IO -> DriverVisitId -> IO [DeliveryRoute]
createDeliveryRoutes repo@DeliveryRouteRepository {..} driverVisitId = do
  verifyDeliveryRouteDoesNotExist repo DeliveryRouteId
  token  <- login
  deliveryRoutes <- getDeliveryRouteId token driverVisitId
  let deliveryRouteIds = DeliveryRouteId . R.deliveryRouteId <$> R.deliveryRouteItems deliveryRoutes
  tracks <- mapConcurrently (getTracksForDeliveryRoute client token) deliveryRouteIds
  let sortedDeliveryRoutes   = sort $ R.deliveryRouteItems DeliveryRoutes
  let DeliveryRoutes' = uncurry toDeliveryRoute <$> sortedDeliveryRoutes `zip` 
  let driverVisitId'      = E.DriverVisitId (unDriverVisitId driverVisitId)
  persistDeliveryRoutes' driverVisitId' repo
  pure deliveryRoutes'

verifyDeliveryRouteDoesNotExist :: DeliveryRouteRepository IO -> [DriverVisitName] -> IO ()
verifyDriverVisitDoesNotExist DriverVisitRepository {..} names = do
  let repoNames = E.DriverVisitName . unDriverVisitName <$> names
  result <- traverse findDriverVisit repoNames
  let filtered = result >>= maybeToList
  if not (null filtered) then throwM ExistingDriverVisitError else pure ()

verifyDeliveryRouteDoesNotExist :: DeliveryRouteRepository IO -> DriverVisitId -> IO ()

toSeconds :: Int -> Int
toSeconds ms = ms `div` 1000

getDriverVisitsByName
  :: -> [DriverVisit] -> IO [DriverVisit]


persistDeliveryRoutes :: [DeliveryRoute] --> DeliveryRouteRepository IO -> IO ()
persistDeliveryRoutes [] _ _ = putStrLn "No delivery routes to persist"
persistDeliveryRoutes deliveryRoutes driverVisitId DeliveryRouteRepository {..} = do
  putStrLn $ "Persisting delivery routes: " <> show deliveryRoutes
  mapConcurrently_ (createDeliveryRoutes name) deliveryRoutes

persistDriverVisits :: [DriverVisit] -> DriverVisitRepository IO -> IO ()
persistDriverVisits []      _                     = putStrLn "No driver visits to persist"
persistDriverVisits driverVisits DriverVisitRepository {..} = do
  putStrLn $ "Persisting driver visits " <> show driverVisits
  mapConcurrently_ createDriverVisits driverVisits

toDriverVisit :: DriverVisitItem -> DriverVisit
toDriverVisit DriverVisitItem {..} = E.DriverVisit (driverVisitId) driverVisitName

toDeliveryRoute :: DeliveryRouteItem -> Int -> DeliveryRoute
toDeliveryRoute DeliveryRouteItem {..} =
  E.DeliveryRoute (deliveryRouteId) deliveryRouteName ()

dateToYear :: Text -> Int
dateToYear txt = fromMaybe 0 $ readMaybe (take 4 (unpack txt))
