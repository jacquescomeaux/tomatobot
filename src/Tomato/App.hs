module Tomato.App
  ( App (..)
  , runApp
  ) where

import RIO


data App = App
    { appLogFunc  :: !LogFunc
    , appToken    :: !ByteString
    , appBotId    :: !Text
    , appFile     :: !FilePath
    , appClientId :: !Text
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

runApp :: RIO App a -> IO a
runApp inner = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App
          { appLogFunc = logFunc
          , appToken = "t2YhlxxwZmn2cWfkAomjMc6BgVPMaC5NRkqHzGQl"
          , appBotId = "073b2ab07990ce95d7442a9d06"
          , appFile = "tomato.png"
          , appClientId = "FbzqI-oR7277JwL1ZGsyUw7yG1F5U0U3WhQ3kOW71Do"
          }
    runRIO app inner
