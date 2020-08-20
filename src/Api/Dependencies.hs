module Api.Dependencies where

import           Repository.DeliveryRoute               ( DeliveryRouteRepository )
import           Repository.DriverVisit              ( DriverVisitRepository )

data Deps = Deps
  { driverVisitRepository :: DriverVisitRepository IO
  , deliveryRouteRepository :: DeliveryRouteRepository IO
  }
