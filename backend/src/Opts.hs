module Opts where

--------------------------------------------------------------------------------
import Options.Applicative
--------------------------------------------------------------------------------

-- Command line configuration parameters
data OptConfig
  = OptConfig
  { optConfigFile  :: String
  , optConfigCheck :: Bool
  }

optConfig :: Parser OptConfig
optConfig = OptConfig
  -- Path to our Dhall configuration file
  <$> strOption
      ( long "config"
     <> help "path to configuration file"
     <> metavar "CONFIG"
     <> showDefault
     <> value "./config.dhall"
      )
  -- Optionally do a configuration file sanity check only
  -- without running the actual web server process
  <*> switch
      ( long "check"
     <> help "check if configuration is sane"
      )

opts :: ParserInfo OptConfig
opts =
  info optConfig
     ( fullDesc
    <> progDesc "example todo backend api"
    <> header "todo"
     )
