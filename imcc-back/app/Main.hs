{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import            Data.Aeson                    hiding (json)
import qualified  Data.ByteString               as B
import            Data.Monoid                   ((<>))
import            Data.Text                     (Text, pack)
import            Data.HashMap.Strict           ((!))
import            Network.HTTP.Types.Status
import            Network.Wai.Middleware.Static
import            Web.Spock
import            Web.Spock.Config
import            System.Directory

import            Control.Monad.IO.Class        (liftIO)
import            Control.Monad.Logger          (LoggingT, runStdoutLoggingT)
import            Database.Persist              hiding (get)
import qualified  Database.Persist              as P
import            Database.Persist.Sqlite       hiding (get)
import qualified  Database.Persist.Sqlite       as PS
import            Database.Persist.TH

data MySession = MySession {
  getValue :: Int
}

type Api = SpockM SqlBackend MySession () ()
type ApiAction a = SpockAction SqlBackend MySession () a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Paper json
  name Text
  auther Text
  deriving Show
|]

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg (MySession 233) (PCPool pool)()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  middleware (staticPolicy (addBase "static"))
  get root $ file "text/html" "static/index.html"
  get "pdf" $ do
    pdf <- liftIO $ B.readFile "Thesis.pdf"
    setHeader "Content-Type" "application/pdf"
    bytes pdf
  post ("upload" <//> var) $ \path -> do
    pdfs <- files
    liftIO $ print pdfs
    let pdf = pdfs ! "file"
    -- liftIO $ renameDirectory (uf_tempLocation pdf) (path ++ ".pdf")
    content <- liftIO $ B.readFile (uf_tempLocation pdf)
    let outPath = (path ++ ".pdf")
    liftIO $ print outPath
    liftIO $ B.writeFile outPath content
    succJson $ uf_name pdf
  get "debug" $ do
    s <- getSessionId
    liftIO $ print s
    ms <- readSession
    let msv = getValue ms
    liftIO $ print msv
    writeSession (MySession $ msv + 1)
    succJson (pack . show $ msv)

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn ->
  runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message = json $ object
  [ "result" .= String "failure"
  , "error" .= object ["code" .= code, "message" .= message]
  ]

succJson :: Text -> ApiAction ()
succJson msg = json $ object
  [
    "result" .= String "success"
  , "message" .= msg
  ]

notFound :: Status -> ApiAction()
notFound status = do
  setStatus status
  errorJson 2 "person id not found"
