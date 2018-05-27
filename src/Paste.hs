{-# LANGUAGE OverloadedStrings #-}
module Paste
  ( mainFunc
  , newFile
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Lucid
import Network.HTTP.Types.Status
import System.Directory (doesPathExist)
import Web.Spock
import Web.Spock.Lucid (lucid)
import Web.Spock.Config

import Paste.Hash (Hash(..), toPath, hashBase64, unpackHash)


basePath :: String
basePath = ".files"

newFile :: T.Text -> IO Hash
newFile content = do
    let hash = hashBase64 content
    T.writeFile (basePath ++ "/" ++ unpackHash hash) content
    return hash


fileNotFound :: Html ()
fileNotFound = do
    h1_ "File not found..."
    p_ "Sorry, we couldn't find that file for you..."

app :: SpockM () () () ()
app = do
    get root . lucid $ do
        h2_ "Create a new paste:"
        form_ [method_ "post", action_ "new"] $ do
            textarea_ [name_ "contents"] ""
            input_ [type_ "submit", value_ "Submit"]
    get ("raw" <//> var) $ \name -> do
        let path = basePath ++ "/" ++ T.unpack name
        doesExist <- liftIO $ doesPathExist path
        if doesExist
            then do
                content <- liftIO $ T.readFile path
                text content
            else lucid fileNotFound
    post "new" $ do
        content <- param' "contents"
        hash <- liftIO $ newFile content
        redirect ("raw" <> toPath hash)


mainFunc :: IO ()
mainFunc = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 1337 (spock spockCfg app)