module Domain.Todo where

--------------------------------------------------------------------------------
import GHC.Stack
--------------------------------------------------------------------------------
import qualified DataAccess.Todo as DA
import Models.Todo
import Types
--------------------------------------------------------------------------------

-- This method of defining the code has been labeled "a pure specification
-- which can be ran in any arbitrary monad."
--
-- This restrictive code has some nice properties about it, such as the fact
-- that only constrained functions may be ran. It also makes it easier to do
-- code/security audits.

-- While these examples are relatively simple, this approach makes it easier
-- to test the entire surface area of the business logic using any arbitrary
-- monad, such as Identity or State. No integration testing necessary.

getTodos
  :: ( HasCallStack
     , DA.TodoDAM m
     )
  => m (Either AppError [TodoR])
getTodos =
  Right <$> DA.getTodos

getTodo
  :: ( HasCallStack
     , DA.TodoDAM m
     )
  => TodoId
  -> m (Either AppError TodoR)
getTodo tid =
  DA.getTodo tid >>= \case
    Nothing -> pure (Left (ErrorQuery callStack DoesNotExist))
    Just v  -> pure (Right v)

createTodo
  :: ( HasCallStack
     , DA.TodoDAM m
     )
  => TodoC
  -> m (Either AppError TodoR)
createTodo tdc =
  DA.createTodo tdc >>= \case
    Nothing -> pure (Left (ErrorGeneral callStack ServerError))
    Just v  -> pure (Right v)

updateTodo
  :: ( HasCallStack
     , DA.TodoDAM m
     )
  => TodoU
  -> m (Either AppError ())
updateTodo tdu =
  DA.updateTodo tdu >>= \case
    Nothing -> pure (Left (ErrorQuery callStack DoesNotExist))
    Just _  -> pure (Right ())
