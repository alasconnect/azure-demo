{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database where

--------------------------------------------------------------------------------
import Data.Text
import Database.Beam
import Database.Beam.Postgres
import GHC.Generics
--------------------------------------------------------------------------------

data TodoT f
  = Todo
  { todoId     :: Columnar f Int
  , todoName   :: Columnar f Text
  , todoDesc   :: Columnar f Text
  , todoStatus :: Columnar f Int
  } deriving (Generic, Beamable)

type Todo = TodoT Identity
deriving instance Show Todo
deriving instance Eq Todo

instance Table TodoT where
  data PrimaryKey TodoT f
    = TodoKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = TodoKey <$> todoId

type TodoKey = PrimaryKey TodoT Identity
deriving instance Show TodoKey
deriving instance Eq TodoKey


--------------------------------------------------------------------------------

data TodoDb f
  = TodoDb
  { todos :: f (TableEntity TodoT)
  } deriving (Generic, Database be)

todoDb :: DatabaseSettings be TodoDb
todoDb = defaultDbSettings
