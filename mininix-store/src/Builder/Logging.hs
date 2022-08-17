module Builder.Logging (BuildLogger (..)) where

data BuildLogger = BuildLogger
  { log :: String -> IO ()
  , logLine :: String -> IO ()
  }