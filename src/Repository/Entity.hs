-- | The Neo4j entity model.
module Repository.Entity where

import           Data.Text                      ( Text )


newtype DriverVisitName = DriverVisitName { unDriverVisitName :: Text } deriving Show
newtype DeliveryRouteName = DeliveryRouteName { unDeliveryRouteName :: Text } deriving Show

data DeliveryRoute = DeliveryRoute
  { 
  , deliveryRouteName :: Text
  } deriving Show

data DeliveryRoute = DeliveryRoute
  , name :: Text
  } deriving Show
