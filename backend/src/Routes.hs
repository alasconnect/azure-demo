{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

--------------------------------------------------------------------------------
import Servant
--------------------------------------------------------------------------------
import Models.Todo
import Models.User
--------------------------------------------------------------------------------

-- In order to get proper route type safety and avoid spelling mistakes like
-- "user" vs "uesr" in urls we create type synonyms which end up as a Symbol
type ApiTerm      = "api"
type Version1Term = "v2"

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
  :<|> GetUsers
  :<|> GetUser
  :<|> CreateUser
  :<|> UpdateUser
  
--------------------------------------------------------------------------------

type TodoListTerm = "todos"
type TodoTerm     = "todo"

type GetTodos =
  TodoListTerm :>
  Get '[JSON] [TodoR]

-- GET /api/v2/todo/{id}
type GetTodo =
  TodoTerm :>
  Capture "todo_id" TodoId :>
  Get '[JSON] TodoR

type CreateTodo =
  TodoTerm :>
  ReqBody '[JSON] TodoC :>
  Post '[JSON] TodoR

type UpdateTodo =
  TodoTerm :>
  ReqBody '[JSON] TodoU :>
  PutNoContent

-------------------------------------------------------------------------------

type UserListTerm = "users"
type UserTerm     = "user"

type GetUsers =
  UserListTerm :>
  Get '[JSON] [UserR]

type GetUser =
  UserTerm :>
  Capture "user_id" UserId :>
  Get '[JSON] UserR

type CreateUser =
  UserTerm :>
  ReqBody '[JSON] UserC :>
  Post '[JSON] UserR

type UpdateUser =
  UserTerm :>
  ReqBody '[JSON] UserU :>
  PutNoContent