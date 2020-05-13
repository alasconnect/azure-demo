{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.DataAccess.Todo where
  
--------------------------------------------------------------------------------
import Data.Functor.Identity (Identity)
--------------------------------------------------------------------------------
import Models.Todo
import DataAccess.Todo
--------------------------------------------------------------------------------

todoMock :: TodoR
todoMock =
  TodoR (TodoId 1) (TodoName "name") (TodoDesc "desc") New

todoMocks :: [TodoR]
todoMocks =
  fmap (const todoMock) [1..]

instance TodoDAM Identity where
  getTodos =
    pure (take 5 todoMocks)

  getTodo _ =
    pure (Just todoMock)

  createTodo _ =
    pure (Just todoMock)

  updateTodo _ =
    pure (Just ())
