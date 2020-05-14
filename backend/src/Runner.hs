module Runner where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Reader
import Data.Pool
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Transaction (withTransaction)
--------------------------------------------------------------------------------
import Types
--------------------------------------------------------------------------------

-- Rather than run our whole Servant computation in a concrete monad transformer
-- stack we elect to make custom runners which we call as needed.

runApp :: AppContext -> ReaderT (AppContext, Connection) IO a -> IO a
runApp ctx f =
  withResource (view pool ctx) $ \conn ->
    flip runReaderT (ctx, conn) f

runAppTrans :: AppContext -> ReaderT (AppContext, Connection) IO a -> IO a
runAppTrans ctx f =
  withResource (view pool ctx) $ \conn ->
    withTransaction conn (flip runReaderT (ctx, conn) f)
