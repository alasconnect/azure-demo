{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

--------------------------------------------------------------------------------
import Servant
--------------------------------------------------------------------------------
import Models.Todo
--------------------------------------------------------------------------------

-- In order to get proper route type safety and avoid spelling mistakes like
-- "user" vs "uesr" in urls we create type synonyms which end up as a Symbol
type ApiTerm      = "api"
type Version1Term = "v1"

-- The top level composition of all of our routes appended together
type Routes =
  ApiTerm :>
  Version1Term :>
  (
         RoutesTodo
    -- Append additional routes here
    -- :<|> Routes*
  )

-- A sub-route definition
type RoutesTodo =
       GetTodos
  :<|> GetTodo
  :<|> CreateTodo
  :<|> UpdateTodo

--------------------------------------------------------------------------------

type TodoListTerm = "todos"
type TodoTerm     = "todo"

-- GET /api/v1/todos
type GetTodos =
  TodoListTerm :>
  Get '[JSON] [TodoR]

-- GET /api/v1/todo/{id}
type GetTodo =
  TodoTerm :>
  Capture "todo_id" TodoId :>
  Get '[JSON] TodoR

-- POST /api/v1/todo
type CreateTodo =
  TodoTerm :>
  ReqBody '[JSON] TodoC :>
  Post '[JSON] TodoR

-- PUT /api/v1/todo
type UpdateTodo =
  TodoTerm :>
  ReqBody '[JSON] TodoU :>
  PutNoContent
