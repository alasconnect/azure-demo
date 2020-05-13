{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DataAccess.Todo where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Reader
import Data.Pool
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

class Monad m => TodoDAM m where
  getTodos :: HasCallStack => m [M.TodoR]
  getTodo :: HasCallStack => M.TodoId -> m (Maybe M.TodoR)
  createTodo :: HasCallStack => M.TodoC -> m (Maybe M.TodoR)
  updateTodo :: HasCallStack => M.TodoU -> m (Maybe ())

instance TodoDAM (ReaderT AppContext IO) where
  getTodos = do
    p <- asks appContextPool
    liftIO $ withResource p allTodos >>= pure . fmap toTodoR

  getTodo tid = do
    p <- asks appContextPool
    liftIO $ withResource p (todoById tid) >>= pure . fmap toTodoR

  createTodo tdc = do
    p <- asks appContextPool
    liftIO $ withResource p (create tdc) >>= pure . fmap toTodoR

  updateTodo tdu = do
    p <- asks appContextPool
    liftIO $ withResource p $ \conn ->
      todoById (view M.todoId tdu) conn >>= \case
        Nothing -> pure Nothing
        Just v  -> update (tu v) conn >> pure (Just ())
    where
      tu v = Todo (M.unTodoId (view M.todoId tdu))
                  (maybe (todoName v) M.unTodoName (view M.name tdu))
                  (maybe (todoDesc v) M.unTodoDesc (view M.desc tdu))
                  (maybe (todoStatus v) fromEnum (view M.status tdu))
