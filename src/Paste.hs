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

newFile :: B.ByteString -> IO Hash
newFile content = do
    let hash = hashBase64 content
    B.writeFile (basePath ++ "/" ++ unpackHash hash) content
    return hash


fileNotFound = do
    h1_ "File not found..."
    p_ "Sorry, we couldn't find that file for you..."

app :: SpockM () () () ()
app = do
    get root . lucid $ do
        h1_ "Hello!"
        p_ "How are you today?"
    get ("raw" <//> var) $ \name -> do
        let path = basePath ++ "/" ++ T.unpack name
        doesExist <- liftIO $ doesPathExist path
        if doesExist
            then do
                content <- liftIO $ T.readFile path
                text content
            else lucid fileNotFound
    post "new" $ do
        content <- body
        hash <- liftIO $ newFile content
        redirect ("raw" <> toPath hash)


mainFunc :: IO ()
mainFunc = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 1337 (spock spockCfg app)