module Api where

--------------------------------------------------------------------------------
import GHC.Stack
import Servant
--------------------------------------------------------------------------------
import Api.Todo
import Routes
import Types
--------------------------------------------------------------------------------

-- This file simply composes all of the Routes together.
-- It's easier to manage each feature in separate modules.

api :: AppContext -> Server Routes
api ctx =
       apiTodo ctx
  -- :<|> api*

apiTodo :: AppContext -> Server RoutesTodo
apiTodo ctx =
       getTodos ctx
  :<|> getTodo ctx
  :<|> createTodo ctx
  :<|> updateTodo ctx
