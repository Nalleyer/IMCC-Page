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
import            Data.Maybe                    (catMaybes, fromJust)
import            Data.Monoid                   ((<>))
import            Data.List.Split               (splitOn)
import            Data.Text                     (Text, pack, unpack)
import            Data.Time
import qualified  Data.HashMap.Strict as HM
import            Network.HTTP.Types.Status
import            Network.Wai.Middleware.Static
import            Web.Spock
import            Web.Spock.Config
import            System.Directory
import            System.FilePath               ((</>))

import            Control.Monad                 (forM_, forM, liftM)
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

-- getter
getKeyword (Keyword k) = k
getCategoryIdFromPaper (Paper _ _ _ _ _ _ cId) = cId
getCategoryName (Category name) = name

main :: IO ()
main = do
  createDirectoryIfMissing True thesisPath
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg (MySession 233) (PCPool pool)()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8082 (spock spockCfg app)


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
    filter <- makeFilter ps
    papers <- runSQL $ selectList filter []
    papers' <- forM papers $ \(Entity pId paper) -> do
      p2ks <- runSQL $ selectList [P2KPId ==. pId] []
      categoryName <- fromJust `liftM` runSQL (P.get (getCategoryIdFromPaper paper))
      keywords <- forM p2ks $ \(Entity _ (P2K _ kId)) -> fromJust `liftM` runSQL (P.get kId)
      let keywords' = map getKeyword keywords
      let json' = addJsonKey "keywords" (toJSON keywords') (toJSON paper)
      let json'' = addJsonKey "pId" (toJSON pId) json'
      return $ addJsonKey "categoryName" (toJSON $ getCategoryName categoryName) json''
    json papers'
  post ("api" <//> "upload") $ do
    pdfs <- files
    -- liftIO $ print pdfs
    ps <- params
    case parseUploadPs ps of
      Just (title, abstract, auther, keywordsStr, categoryStr) -> do
        let keywords = map pack $ splitKeyWords $ unpack keywordsStr
        let category = categoryStr
        cId <- runSQL $ insert' (UniqueCategory category) (Category category)
        zonedTime <- liftIO getZonedTime
        let (year, month, day) = utctGregorian (zonedTimeToUTC zonedTime)
        let paper = Paper title auther (fromInteger year) month day abstract cId
        pId <- runSQL $ insert paper
        forM_ keywords $ \k -> do
          kId <- runSQL $ insert' (UniqueKeyword k) (Keyword k)
          runSQL $ insert $ P2K pId kId
        let pdf = pdfs HM.! "file"
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

getKey uniqueField = do
  mKv <- getBy uniqueField
  case mKv of
    Just (Entity key value) -> return $ Just key
    Nothing -> return Nothing

makeFilter ps = do
  filterList <- forM ps makeAFilter
  return $ concat filterList
    where
      makeAFilter ("year", value) = return [PaperYear ==. read (unpack value)]
      makeAFilter ("auther", value) = return [PaperAuther ==. value]
      makeAFilter ("category", value) = do
        mCKey <- runSQL $ getKey (UniqueCategory value)
        case mCKey of
          Just cId -> return [PaperCategoryId ==. cId]
          Nothing -> return []
      makeAFilter ("keywords", keywords) = do
        let splited = splitKeyWords $ unpack keywords
        kids <- catMaybes `liftM` forM splited (\k -> runSQL $ getKey (UniqueKeyword $ pack k))
        k2ps <- runSQL $ selectList [P2KKId <-. kids] []
        let pids = map (\(Entity key (P2K pId kId)) -> pId) k2ps
        return [PaperId <-. pids]
      makeAFilter _ = return []



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

addJsonKey :: Text -> Value -> Value -> Value
addJsonKey key val (Object xs) = Object $ HM.insert key val xs
addJsonKey _ _ xs = xs
