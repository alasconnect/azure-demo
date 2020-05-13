module Runner where

--------------------------------------------------------------------------------
import Control.Monad.Reader
--------------------------------------------------------------------------------
import Types
--------------------------------------------------------------------------------

-- Rather than run our whole Servant computation in a concrete monad transformer
-- stack we elect to make custom runners which we call as needed.

runApp :: AppContext -> ReaderT AppContext IO a -> IO a
runApp = flip runReaderT
