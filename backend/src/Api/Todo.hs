module Api.Todo where

--------------------------------------------------------------------------------
import Data.Text
import GHC.Stack
import Servant
--------------------------------------------------------------------------------
import qualified Domain.Todo as DA
import Errors
import Models.Todo
import Routes
import Runner
import Types
--------------------------------------------------------------------------------

-- We enable HasCallStack at this point so we can get decent error information
-- when we send it off to wherever we log, likely ElasticSearch.

-- For the most part API code isn't super specific. It calls the underlying
-- Domain (business logic) function inside a particular runner, and then
-- handles any errors that may come out.

getTodos
  :: HasCallStack
  => AppContext
  -> Handler [TodoR]
getTodos ctx =
  handleError ctx (runApp ctx DA.getTodos)

getTodo
  :: HasCallStack
  => AppContext
  -> TodoId
  -> Handler TodoR
getTodo ctx tid =
  handleError ctx (runApp ctx (DA.getTodo tid))

createTodo
  :: HasCallStack
  => AppContext
  -> TodoC
  -> Handler TodoR
createTodo ctx tdc =
  handleError ctx (runApp ctx (DA.createTodo tdc))

updateTodo
  :: HasCallStack
  => AppContext
  -> TodoU
  -> Handler NoContent
updateTodo ctx tdu = do
  handleError ctx (runApp ctx (DA.updateTodo tdu))
  pure NoContent
