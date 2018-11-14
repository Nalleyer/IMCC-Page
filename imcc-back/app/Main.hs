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

import            Control.Monad                 (forM_)
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
Category json
  name Text
  UniqueCategory name
Paper json
  title Text
  auther Text
  year Int
  month Int
  day Int
  abstract Text
  categoryId CategoryId
  deriving Show
Keyword json
  name Text
  UniqueKeyword name
P2K json
  pId PaperId
  kId KeywordId
|]

main :: IO ()
main = do
  createDirectoryIfMissing True thesisPath
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg (MySession 233) (PCPool pool)()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8081 (spock spockCfg app)

app :: Api
app = do
  middleware (staticPolicy (addBase "static"))
  get root $ file "text/html" "static/index.html"
  -- TODO: delete this api
  get "pdf" $ do
    pdf <- liftIO $ B.readFile "Thesis.pdf"
    setHeader "Content-Type" "application/pdf"
    bytes pdf
  get ("api" <//> "list") $ do
    ps <- params
    liftIO $ print ps
    -- let mTitle = lookup "title" ps
    -- let mYear = lookup "year" ps
    -- let mAuther = lookup "auther" ps
    -- let mKeywords = lookup "keywords" ps
    -- let mCategory = lookup "category" ps
    -- let filter = makeFilter ps
    -- papers <- runSQL $ selectList filter []
    -- liftIO $ print papers
    succJson (pack "list")
  post ("api" <//> "upload") $ do
    pdfs <- files
    -- liftIO $ print pdfs
    ps <- params
    case parseUploadPs ps of
      Just (title, abstract, auther, keywordsStr, categoryStr) -> do
        let keywords = map pack $ splitKeyWords $ unpack keywordsStr
        let category = categoryStr
        cId <- runSQL $ insert' (UniqueCategory $ category) (Category category)
        zonedTime <- liftIO getZonedTime
        let (year, month, day) = utctGregorian (zonedTimeToUTC zonedTime)
        let paper = Paper title auther (fromInteger year) month day abstract cId
        pId <- runSQL $ insert $ paper
        forM_ keywords $ \k -> do
          kId <- runSQL $ insert' (UniqueKeyword k) (Keyword k)
          runSQL $ insert $ P2K pId kId
        let pdf = pdfs ! "file"
        let outPath = thesisPath </> (show (PS.fromSqlKey pId) ++ ".pdf")
        content <- liftIO $ B.readFile (uf_tempLocation pdf)
        liftIO $ B.writeFile outPath content
        succJson $ pack $ show pId
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

insert' uniqueField val = do
  mKv <- getBy uniqueField
  case mKv of
    Just (Entity key value) -> return key
    Nothing -> insert val

makeFilter ps = makeFilter' ps []
  where
    makeFilter' [] f = f
    makeFilter' ((key, value):xs) f = makeFilter' xs $ makeAFilter key value ++ f
    makeAFilter key value =
      case key of
      "category" -> [PaperCategoryId ==. (read $ unpack value)]
      "year"     -> [PaperYear ==. (read $ unpack value)]
      "auther"   -> [PaperAuther ==. value]
      -- "keywords" -> []
      _          -> []

parseUploadPs ps =
  let mTitle = lookup "title" ps
      mAbstract = lookup "abstract" ps
      mAuther = lookup "auther" ps
      mKeywords = lookup "keywords" ps
      mCategory = lookup "category" ps
  in case (mTitle, mAbstract, mAuther, mKeywords, mCategory) of
      (Just title, Just abstract, Just auther, Just keywordsStr, Just categoryStr)
        -> Just (title, abstract, auther, keywordsStr, categoryStr)
      _ -> Nothing

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

utctGregorian time = toGregorian $ utctDay time
utctYear time = let (year, _, _) = utctGregorian time in year
