{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

--------------------------------------------------------------------------------
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Pool
import Data.Text (Text, pack)
import Dhall
import Database.Beam.Postgres (Connection)
import Deriving.Aeson.Stock
import GHC.Stack
--------------------------------------------------------------------------------

-- Orphan to pretty print callstacks
instance ToJSON CallStack where
  toJSON = toJSON . prettyCallStack

--------------------------------------------------------------------------------

-- In order to provide rich error information we pass it back as a JSON object.
data ErrorJSON
  = ErrorJSON
  { statusCode :: Int
  , appError   :: AppError
  }
  deriving (Show, Generic)
  deriving (ToJSON)
  via Snake ErrorJSON

mkErrorJSON :: Text -> Text -> CallStack -> Value
mkErrorJSON c t s =
  object [ "error" .= String (c <> " " <> t)
         , "stack" .= String (pack . prettyCallStack $ s)
         ]

--------------------------------------------------------------------------------
-- For better or for worse we create a hierarchy of error types to track what
-- actually went wrong with the application.

data AppError
  = ErrorQuery CallStack QueryError
  | ErrorGeneral CallStack GeneralError
  deriving (Show, Generic)

instance ToJSON AppError where
  toJSON (ErrorQuery s t) = mkErrorJSON "ErrorQuery" (pack . show $ t) s
  toJSON (ErrorGeneral s t) = mkErrorJSON "ErrorGeneral" (pack . show $ t) s

data QueryError
  = DoesNotExist
  | RecordExists
  deriving (Show, Generic)
  deriving (ToJSON)
  via Snake QueryError

data GeneralError
  = ServerError
  deriving (Show, Generic)
  deriving (ToJSON)
  via Snake GeneralError

--------------------------------------------------------------------------------
-- We use Dhall for application configurations

data AppConfig
  = AppConfig
  { appConfigAppPort      :: Integer
  , appConfigDbHost       :: Text
  , appConfigDbPort       :: Integer
  , appConfigDbUser       :: Text
  , appConfigDbPass       :: Text
  , appConfigDbName       :: Text
  , appConfigPoolStripes  :: Integer
  , appConfigPoolKillTime :: Integer
  , appConfigPoolCount    :: Integer
  } deriving (Show, Generic)
instance FromDhall AppConfig
makeFields ''AppConfig

--------------------------------------------------------------------------------

-- This provides all the contextual data required to run an API endpoint.
--
-- It may include things like database connection pools, configuration data,
-- or carry forward an initialized random generator.
--
-- Typically pushed through the system in a ReaderT.
data AppContext
  = AppContext
  { appContextPool   :: Pool Connection
  , appContextConfig :: AppConfig
  } deriving (Generic)
makeFields ''AppContext
