{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Todo where

--------------------------------------------------------------------------------
import Control.Lens
import Data.Aeson
import Data.Text
import Data.Text.Read (decimal)
import Deriving.Aeson.Stock
import Servant.API

import Data.Int (Int32)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Primitives
--
-- We wrap primitive types with a newtype to provide type safety.
-- An alternative is the `tagged` library.
--------------------------------------------------------------------------------

newtype TodoId = TodoId { unTodoId :: Int32 }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
  via Vanilla TodoId
-- Create a proper instance so Servant can convert strict types.
instance FromHttpApiData TodoId where
  parseUrlPiece t =
    case decimal t of
      Right (v, _) -> Right . TodoId . fromIntegral $ v
      Left e       -> Left . pack $ e

newtype TodoName = TodoName { unTodoName :: Text }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
  via Vanilla TodoName

newtype TodoDesc = TodoDesc { unTodoDesc :: Text }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
  via Vanilla TodoDesc

data TodoStatus
  = New
  | InProgress
  | Complete
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
  via Vanilla TodoStatus

-- We convert statuses into integers instead of conversion to a string type.
-- This tends to benefit efficiency, data storage, and documentation.
instance Enum TodoStatus where
  toEnum 0 = New
  toEnum 1 = InProgress
  toEnum 2 = Complete
  toEnum _ = New
  fromEnum New        = 0
  fromEnum InProgress = 1
  fromEnum Complete   = 2

--------------------------------------------------------------------------------
-- Complex Types
--
-- We create "view models" which map to the different possible ways to use the
-- data being provided. This is a workaround for the lack of row polymorphism
-- in Haskell.
--
-- Four distinct types:
--   - Readable
--   - Creatable
--   - Updatable
--   - Deletable
--
-- Imagine a User type complete with login information and other abstract
-- information attached to it. A UserR(eadable) would omit the `password` field
-- and any other sensitive information that shouldn't be sent over the wire.
--
-- On the other hand, a UserU(pdatable) would contain the brief list of fields
-- that are able to be updated, likely all as `Maybe`s.
--------------------------------------------------------------------------------

data TodoR
  = TodoR
  { todoRTodoId :: TodoId
  , todoRName   :: TodoName
  , todoRDesc   :: TodoDesc
  , todoRStatus :: TodoStatus
  -- date created
  -- date updated
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON)
  via PrefixedSnake "todoR" TodoR
makeFields ''TodoR

data TodoC
  = TodoC
  { todoCName :: TodoName
  , todoCDesc :: TodoDesc
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON)
  via PrefixedSnake "todoC" TodoC
makeFields ''TodoC

data TodoU
  = TodoU
  { todoUTodoId :: TodoId
  , todoUName   :: Maybe TodoName
  , todoUDesc   :: Maybe TodoDesc
  , todoUStatus :: Maybe TodoStatus
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON)
  via PrefixedSnake "todoU" TodoU
makeFields ''TodoU

-- No TodoD since we don't need any meta data when/if we decide to delete things
