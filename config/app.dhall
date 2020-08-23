let Env = < Test : {} | Prod : {} >

let postgresqlHostEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "127.0.0.1", Prod = λ(x : {}) →  "0.0.0.0" } env

let makepostgresqlConfig = λ(env : Env) →
  { psqlHost = "${psqlHostEnv env}"
  , psqlPort = 5432
  , psqlUser = "psql"
  , psqlPassword = "test"
  }

let makeHttpServerConfig = λ(env : Env) →
  { serverPort = 3000
  }


let makeConfig = λ(env : Env) →
  { psql = makePostgresqlConfig env
  , httpServer = makeHttpServerConfig env
  }

in makeConfig ( Env.Test {=} )
