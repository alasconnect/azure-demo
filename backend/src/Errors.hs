module Errors where

--------------------------------------------------------------------------------
import Control.Monad.Except
import Data.Aeson
import Servant
--------------------------------------------------------------------------------
-- import Logging
import Types
--------------------------------------------------------------------------------

handleError :: AppContext -> IO (Either AppError a) -> Handler a
handleError ctx f = Handler . ExceptT $ convert ctx f

convert :: AppContext -> IO (Either AppError a) -> IO (Either ServerError a)
convert _ctx f = f >>= \case
  Left e -> do
    -- errorLogger ctx e
    pure . Left $ translate e
  Right a -> pure . Right $ a

-- Translate the error to send the proper HTTP status code
translate :: AppError -> ServerError 
translate e =
  case e of
    ErrorQuery _ DoesNotExist ->
      err404 { errBody = enc 404 e }
    ErrorQuery _ RecordExists ->
      err409 { errBody = enc 409 e }
    ErrorGeneral _ _ ->
      err500 { errBody = enc 500 e }
  where
    enc code err = encode (ErrorJSON code err)

-- errorLogger :: (MonadIO m, ToJSON a) => AppContext -> a -> m ()
-- errorLogger ctx e = liftIO $ runContextLogT ctx ["api", "error"] $ logAttention "API Caught Error" e
