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
import            Data.List.Split               (splitOn)
import            Data.Text                     (Text, pack, unpack)
import            Data.Time
import            Data.HashMap.Strict           ((!))
import            Network.HTTP.Types.Status
import            Network.Wai.Middleware.Static
import            Web.Spock
import            Web.Spock.Config
import            System.Directory
import            System.FilePath               ((</>))

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

thesisPath = "thesis"

type Api = SpockM SqlBackend MySession () ()
type ApiAction a = SpockAction SqlBackend MySession () a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Paper json
  title Text
  auther Text
  time UTCTime
  abstract Text
  keywords [Text]
  categary Int
  deriving Show
Categary json
  name Text
|]

main :: IO ()
main = do
  createDirectoryIfMissing True thesisPath
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
  post ("api" <//> "upload") $ do
    pdfs <- files
    -- liftIO $ print pdfs
    ps <- params
    let mTitle = lookup "title" ps
    let mAbstract = lookup "abstract" ps
    let mAuther = lookup "auther" ps
    let mKeywords = lookup "keywords" ps
    let mCategory = lookup "category" ps
    case (mTitle, mAbstract, mAuther, mKeywords, mCategory) of
      (Just title, Just abstract, Just auther, Just keywordsStr, Just categoryStr) -> do
        -- liftIO $ print title
        -- liftIO $ print abstract
        let keywords = map pack $ splitKeyWords $ unpack keywordsStr
        let category = read $ unpack categoryStr :: Int
        liftIO $ print category
        zonedTime <- liftIO getZonedTime
        let paper = Paper title auther (zonedTimeToUTC zonedTime) abstract keywords category
        id <- runSQL $ insert $ paper

        let pdf = pdfs ! "file"
        let outPath = thesisPath </> (show (PS.fromSqlKey id) ++ ".pdf")
        content <- liftIO $ B.readFile (uf_tempLocation pdf)
        -- liftIO $ print outPath
        liftIO $ B.writeFile outPath content
        succJson $ pack $ show id
      _ -> do
        setStatus status400
        errorJson 3 "invalid post"

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
  [ "result" .= String "success"
  , "message" .= msg
  ]

notFound :: Status -> ApiAction()
notFound status = do
  setStatus status
  errorJson 2 "person id not found"

splitKeyWords ks = splitOn ";" ks
