module Main where

import           Api.Dependencies               ( Deps(..) )
import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Http.Client.Spotify            ( mkSpotifyClient )
import           Http.Server                    ( serve )
import           Repository.Location            ( mkLocationRepository )
import           Repository.DeliveryRoute       ( mkDeliveryRouteRepository )
import           Repository.DriverVisit         ( mkDriverVisitRepository )
import           Repository.VendorVisit         ( mkVendorVisitRepository )
import           Repository.Neo                 ( mkPipePool )

main :: IO ()
main = do
  c             <- loadConfig
  pool          <- mkPipePool (neo4j c)
  locationRepo  <- mkLocationRepo 
  deliveryRouteRepo <- mkDeliveryRouteRepository
  driverVisitRepo <- mkDriverVisitRepository
  vendorVisitRepo <- mkVendorVisitRepository
  artistRepo    <- mkArtistRepository pool
  albumRepo     <- mkAlbumRepository pool
  spotifyClient <- mkSpotifyClient (spotify c)
  let deps = Deps artistRepo albumRepo spotifyClient
  serve (httpServer c) deps
