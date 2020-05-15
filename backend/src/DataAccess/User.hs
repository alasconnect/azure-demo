{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DataAccess.User where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Reader
import Database.Beam.Postgres (Connection)
import GHC.Stack
--------------------------------------------------------------------------------
import Database
import Database.User
import qualified Models.User as M
import Types
--------------------------------------------------------------------------------

toUserR :: User -> M.UserR
toUserR t =
   M.UserR (M.UserId $ userId t)
           (M.UserName $ userUserName t)
           (M.UserFullName $ userFullName t)

class Monad m => UserDAM m where
   getUsers   :: HasCallStack => m [M.UserR]
   getUser    :: HasCallStack => M.UserId -> m (Maybe M.UserR)
   createUser :: HasCallStack => M.UserC  -> m (Maybe M.UserR)
   updateUser :: HasCallStack => M.UserU  -> m (Maybe ())

instance UserDAM (ReaderT (AppContext, Connection) IO) where
   getUsers = do
      (_, conn) <- ask
      liftIO $ allUsers conn >>= pure . fmap toUserR

   getUser uid = do
      (_, conn) <- ask
      liftIO $ userById uid conn >>= pure . fmap toUserR

   createUser uc = do
      (_, conn) <- ask
      liftIO $ create uc conn >>= pure . fmap toUserR

   updateUser uu = do
      (_, conn) <- ask
      liftIO $ userById (view M.userId uu) conn >>= \case
         Nothing -> pure Nothing
         Just v  -> update (usr v) conn >> pure (Just ())
      where
         usr v = User (M.unUserId (view M.userId uu))
                     (maybe (M.userName v) M.unUserName (view M.userName uu))
                     (maybe (userFullName v) M.unUserFullName (view M.userFullName uu))
                     (maybe (userPassword v) M.unUserPassword (view M.userPassword uu))