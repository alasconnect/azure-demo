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


data UserT f
  = User
  { userId           :: Columnar f Int
  , userUserName     :: Columnar f Text
  , userFullName     :: Columnar f Text
  , userPassword     :: Columnar f Text
  } deriving (Generic, Beamable)

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

instance Table UserT where
    data PrimaryKey UserT f
      = UserKey (Columnar f Int)
      deriving (Generic, Beamable)
    primaryKey = UserKey <$> userId

type UserKey = PrimaryKey UserT Identity
deriving instance Show UserKey
deriving instance Eq UserKey

--------------------------------------------------------------------------------

data TodoDb f
  = TodoDb
  { todos :: f (TableEntity TodoT)
  , users :: f (TableEntity UserT)
  } deriving (Generic, Database be)

todoDb :: DatabaseSettings be TodoDb
todoDb = defaultDbSettings
