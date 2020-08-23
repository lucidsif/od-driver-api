{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | The Neo4j repository for DriverVisit, including interface and cypher queries.
module Repository.DriverVisit
  ( mkDriverVisitRepository
  , DriverVisitRepository(..)
  )
where

import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( listToMaybe )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Repository.Entity
import           Repository.Mapper

data DriverVisitRepository m = DriverVisitRepository
  { findDriverVisit :: DriverVisitName -> m (Maybe DriverVisit)
  }

mkDriverVisitRepository :: Pool Pipe -> IO (DriverVisitRepository IO)
mkDriverVisitRepository pool = pure $ DriverVisitRepository
  { findDriverVisit   = withResource pool . findDriverVisit'
  , createDriverVisit = withResource pool . createDriverVisit'
  }

findDriverVisit' :: DriverVisitName -> Pipe -> IO (Maybe DriverVisit)
findDriverVisit' DriverVisitName {..} pipe = toEntityMaybe "a" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:DriverVisit) WHERE a.name CONTAINS {name} RETURN a"
    (fromList [("name", T unDriverVisitName)])

createDriverVisit' :: DriverVisit -> Pipe -> IO (Maybe DriverVisitId)
createDriverVisit' DriverVisit {..} pipe = do
  records <- run pipe $ queryP
    "CREATE (a:DriverVisit { name : {name}, spotifyId : {spotifyId} }) RETURN a.spotifyId"
    (fromList
      [ ("name"     , T driverVisitName)
      ]
    )
