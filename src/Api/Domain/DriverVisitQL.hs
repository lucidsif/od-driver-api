{-# LANGUAGE DeriveGeneric, RecordWildCards, TypeFamilies #-}

module Api.Domain.DriverVisitQL where

import           Data.Morpheus.Kind             ( OBJECT )
import           Data.Morpheus.Types            ( GQLType(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Repository.Entity              ( DriverVisit(..) )
import qualified Repository.Entity             as E

data DriverVisitQL = DriverVisitQL
  { name :: Text } deriving Generic

instance GQLType DriverVisitQL where
  type KIND DriverVisitQL = OBJECT

toDriverVisitQL :: DriverVisit -> DriverVisitQL
toDriverVisitQL DriverVisit {..} =
  DriverVisitQL () DriverVisitName
