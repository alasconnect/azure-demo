module Database.User where

--------------------------------------------------------------------------------
import Control.Lens
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import GHC.Stack
--------------------------------------------------------------------------------
import Database
import Models.User
--------------------------------------------------------------------------------

allUsers :: (HasCallStack, MonadIO m) => Connection -> m [User]
allUsers conn =
   liftIO $ runBeamPostgres conn $
      runSelectReturningList $
      select $ do
         t <- all_ (users todoDb)
         pure t

userById :: (HasCallStack, MonadIO m) => UserId -> Connection -> m (Maybe User)
userById (UserId uid) conn =
   liftIO $ runBeamPostgres conn $
      runSelectReturningOne $
      select $ do
         t <- all_ (users todoDb)
         guard_ (Database.userId t ==. val_ uid)
         pure t

create :: (HasCallStack, MonadIO m) => UserC -> Connection -> m (Maybe User)
create uc conn =
   liftIO $ runBeamPostgres conn $ do
      v <- runInsertReturningList $
         insert (users todoDb) $
         insertExpressions
            [ User default_
                   (val_ (unUserName $ view userName uc))
                   (val_ (unUserFullName $ view Models.User.userFullName uc))
                   (val_ (unUserPassword $ view Models.User.userPassword uc))
            ]
      case v of
         []    -> pure Nothing
         [a]   -> pure (Just a)
         (a:_) -> pure (Just a)

update :: (HasCallStack, MonadIO m) => User -> Connection -> m ()
update uu conn =
   liftIO $ runBeamPostgres conn $
      runUpdate $ save (users todoDb) uu