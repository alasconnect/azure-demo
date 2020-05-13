{-# LANGUAGE FlexibleContexts #-}

module App where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad (unless)
import Data.Pool
import Data.Proxy
import Data.Text (pack, unpack)
import Dhall
import Database.Beam.Postgres
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Servant
--------------------------------------------------------------------------------
import Api
import Opts
import Routes
import Types
--------------------------------------------------------------------------------

-- Standard Servant setup
totalApi :: Proxy Routes
totalApi = Proxy

-- We defer running our code in a particular monad transformer stack
-- until later at the API level. Until then it is simply in a Servant Handler.
app :: AppContext -> Application
app ctx = serve totalApi (api ctx)

run :: OptConfig -> IO ()
run o = do
  -- Parse configuration file
  a <- input auto (pack (optConfigFile o))

  -- Check the configuration file for sanity without running the app.
  -- This is useful for double checking against a built binary in a CI server
  -- without actually starting the process.
  unless (optConfigCheck o) $ do
    -- Generate database connection pool
    p <- mkPool a
    putStrLn ("Running server on port " ++ show (view appPort a))
    -- Run webserver
    W.run (fromIntegral (view appPort a)) (app (AppContext p a))
  where
    mkPool a = do
      let i = ConnectInfo
                (unpack (view dbHost a))
                (fromIntegral (view dbPort a))
                (unpack (view dbUser a))
                (unpack (view dbPass a))
                (unpack (view dbName a))
      createPool (connect i) close
        (fromIntegral (view poolStripes a))
        (fromIntegral (view poolKillTime a))
        (fromIntegral (view poolCount a))
