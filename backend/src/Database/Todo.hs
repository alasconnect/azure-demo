module Database.Todo where

--------------------------------------------------------------------------------
import Control.Lens
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import GHC.Stack
--------------------------------------------------------------------------------
import Database
import Models.Todo
--------------------------------------------------------------------------------

allTodos :: (HasCallStack, MonadIO m) => Connection -> m [Todo]
allTodos conn =
  liftIO $ runBeamPostgres conn $
    runSelectReturningList $
    select $ do
      t <- all_ (todos todoDb)
      pure t

todoById :: (HasCallStack, MonadIO m) => TodoId -> Connection -> m (Maybe Todo)
todoById (TodoId tid) conn =
  liftIO $ runBeamPostgres conn $
    runSelectReturningOne $
    select $ do
      t <- all_ (todos todoDb)
      guard_ (Database.todoId t ==. val_ tid)
      pure t

create :: (HasCallStack, MonadIO m) => TodoC -> Connection -> m (Maybe Todo)
create tc conn =
  liftIO $ runBeamPostgres conn $ do
    v <- runInsertReturningList $
      insert (todos todoDb) $
      insertExpressions
        [ Todo default_
               (val_ (unTodoName $ view name tc))
               (val_ (unTodoDesc $ view desc tc))
               (val_ (fromEnum New))
        ]
    case v of
      []    -> pure Nothing
      [a]   -> pure (Just a)
      (a:_) -> pure (Just a)

update :: (HasCallStack, MonadIO m) => Todo -> Connection -> m ()
update tu conn =
  liftIO $ runBeamPostgres conn $
    runUpdate $ save (todos todoDb) tu
