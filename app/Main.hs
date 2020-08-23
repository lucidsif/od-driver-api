module Main where

import           Api.Dependencies               ( Deps(..) )
import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Http.Server                    ( serve )
import           Repository.Location            ( mkLocationRepository )
import           Repository.DeliveryRoute       ( mkDeliveryRouteRepository )
import           Repository.DriverVisit         ( mkDriverVisitRepository )
import           Repository.Postgresql                 ( mkPipePool )

main :: IO ()
main = do
  c             <- loadConfig
  pool          <- mkPipePool (postgresql c)
  locationRepo  <- mkLocationRepo 
  deliveryRouteRepo <- mkDeliveryRouteRepository
  driverVisitRepo <- mkDriverVisitRepository
  serve (httpServer c) deps
