{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DataAccess.Todo where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Reader
import Database.Beam.Postgres (Connection)
import GHC.Stack
--------------------------------------------------------------------------------
import Database
import Database.Todo
import qualified Models.Todo as M
import Types
--------------------------------------------------------------------------------

toTodoR :: Todo -> M.TodoR
toTodoR t =
  M.TodoR (M.TodoId $ todoId t)
          (M.TodoName $ todoName t)
          (M.TodoDesc $ todoDesc t)
          (toEnum $ todoStatus t)

-- While not "lawful" typeclasses, we take advantage of the abstraction
-- so that we can write an intermediate layer between our Domain and
-- actual datasource (in this particular case a Database layer).
--
-- This then allows us to test the Domain logic using the same typeclass
-- but in some alternative Monad, like Identity or State.

class Monad m => TodoDAM m where
  getTodos :: HasCallStack => m [M.TodoR]
  getTodo :: HasCallStack => M.TodoId -> m (Maybe M.TodoR)
  createTodo :: HasCallStack => M.TodoC -> m (Maybe M.TodoR)
  updateTodo :: HasCallStack => M.TodoU -> m (Maybe ())

instance TodoDAM (ReaderT (AppContext, Connection) IO) where
  getTodos = do
    (_, conn) <- ask
    liftIO $ allTodos conn >>= pure . fmap toTodoR

  getTodo tid = do
    (_, conn) <- ask
    liftIO $ todoById tid conn >>= pure . fmap toTodoR

  createTodo tdc = do
    (_, conn) <- ask
    liftIO $ create tdc conn >>= pure . fmap toTodoR

  updateTodo tdu = do
    (_, conn) <- ask
    liftIO $ todoById (view M.todoId tdu) conn >>= \case
      Nothing -> pure Nothing
      Just v  -> update (tu v) conn >> pure (Just ())
    where
      tu v = Todo (M.unTodoId (view M.todoId tdu))
                  (maybe (todoName v) M.unTodoName (view M.name tdu))
                  (maybe (todoDesc v) M.unTodoDesc (view M.desc tdu))
                  (maybe (todoStatus v) fromEnum (view M.status tdu))
