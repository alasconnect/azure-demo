{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.User where

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
--------------------------------------------------------------------------------

newtype UserId = UserId { unUserId :: Int32 }
   deriving (Eq, Show, Generic)
   deriving (FromJSON, ToJSON)
   via Vanilla UserId
instance FromHttpApiData UserId where
   parseUrlPiece t =
      case decimal t of
         Right (v, _) -> Right . UserId . fromIntegral $ v
         Left e       -> Left . pack $ e

newtype UserName = UserName { unUserName :: Text }
   deriving (Eq, Show, Generic)
   deriving (FromJSON, ToJSON)
   via Vanilla UserName

newtype UserFullName = UserFullName { unUserFullName :: Text }
   deriving (Eq, Show, Generic)
   deriving (FromJSON, ToJSON)
   via Vanilla UserFullName

newtype UserPassword = UserPassword { unUserPassword :: Text }
   deriving (Eq, Show, Generic)
   deriving (FromJSON, ToJSON)
   via Vanilla UserPassword

--------------------------------------------------------------------------------
-- Complex Types
--------------------------------------------------------------------------------
data UserR
   = UserR
   { userRUserId       :: UserId
   , userRUserName     :: UserName
   , userRUserFullName :: UserFullName
   -- date created
   -- effective date
   -- inactive date
   }
   deriving (Generic)
   deriving (FromJSON, ToJSON)
   via PrefixedSnake "userR" UserR
makeFields ''UserR

data UserC
   = UserC
   { userCUserName           :: UserName
   , userCUserFullName       :: UserFullName
   , userCUserPassword       :: UserPassword
   , userCUserVerifyPassword :: UserPassword
   }
   deriving (Generic)
   deriving (FromJSON, ToJSON)
   via PrefixedSnake "userC" UserC
makeFields ''UserC

data UserU
   = UserU
   { userUUserId             :: UserId
   , userUUserName           :: Maybe UserName
   , userUUserFullName       :: Maybe UserFullName
   , userUUserPassword       :: Maybe UserPassword
   , userUUserVerifyPassword :: Maybe UserPassword
   }
   deriving (Generic)
   deriving (FromJSON, ToJSON)
   via PrefixedSnake "userU" UserU
makeFields ''UserU
