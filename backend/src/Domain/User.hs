module Domain.User where

--------------------------------------------------------------------------------
import GHC.Stack
--------------------------------------------------------------------------------
import qualified DataAccess.User as DA
import Models.User
import Types
--------------------------------------------------------------------------------
getUsers
   :: ( HasCallStack
      , DA.UserDAM m
      )
   => m (EitherAppError [UserR])
getUsers =
   Right <$> DA.getUsers

getUser
   :: ( HasCallStack
      , DA.UserDAM m
      )
   => m (Either AppError UserR)
getUser uid =
   DA.getUser uid >>= \case
      Nothing -> pure (Left (ErrorQuery callStack DoesNotExist))
      Just v  -> pure (Right v)

createUser
   :: ( HasCallStack
      , DA.UserDAM m
      )
   => m (Either AppError UserR)
createUser uc =
   DA.createUser uc >>= \case
      Nothing -> pure (Left (ErrorGeneral callStack ServerError))
      Just v  -> pure (Right v)

updateUser
   :: ( HasCallStack
      , DA.UserDAM m
      )
   => UserU
   -> m (Either AppError ())
updateUser uu =
   DA.updateUser uu >>= \case
      Nothing -> pure (Left (ErrorQuery callStack DoesNotExist))
      Just _  -> pure (Right ())