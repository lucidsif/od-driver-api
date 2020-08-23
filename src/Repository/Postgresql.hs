{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | The PostrgreSQL connection pool.
module Repository.PostrgreSQL
  ( mkPipePool
  )
where

import           Config                         ( PostgreSQLConfig(..) )
import           Data.Pool                      ( Pool
                                                , createPool
                                                )
import           Data.Text                      ( unpack )
import           Database.Bolt           hiding ( unpack )
import           GHC.Natural                    ( naturalToInt )

mkConfig :: PostgreSQLConfig -> BoltCfg
mkConfig PostgreSQLConfig {..} = BoltCfg { magic         = 1616949271
                                    , version       = 1
                                    , userAgent     = "hasbolt/1.3"
                                    , maxChunkSize  = 65535
                                    , socketTimeout = 5
                                    , host          = unpack postgresqlHost
                                    , port          = naturalToInt postgresqlPort
                                    , user          = postgresqlUser
                                    , password      = postgresqlPassword
                                    }

mkPipePool :: PostgreSQLConfig -> IO (Pool Pipe)
mkPipePool c = createPool acquire release 1 3600 10 where
  acquire = connect (mkConfig c)
  release = close
