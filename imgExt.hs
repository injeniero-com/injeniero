{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Text.IO      as T
import qualified Data.Text.Lazy.IO as TL
import qualified Text.MMark        as MMark
import qualified Text.Megaparsec   as M
import qualified Text.MMark.Extension as Ex
import qualified Data.Text as Te
import qualified Text.URI as URI
import qualified System.FilePath as FP
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Lucid.Base (makeAttribute)
import Lucid
import Data.Maybe (fromMaybe)
import Text.URI.Lens (uriPath)
import Lens.Micro ((^.))
import Data.Aeson (ToJSON, FromJSON, decode)
import System.Environment ( getArgs )

-- Define Config data structure
data Config = Config
  { confSizes :: String
  , confSet   :: [Int]
  } deriving (Show, Generic, Eq)

defaultConfig :: Config
defaultConfig = Config {confSizes = "(max-width:600px) 100vw, 850px", confSet = [400,850]}

-- Parsing the Config data
instance FromJSON Config
instance ToJSON Config

-- Function to read the config file
readConfig :: FilePath -> IO (Maybe Config)
readConfig path = do
  jsonData <- B.readFile path
  return (decode jsonData :: Maybe Config)

main :: IO ()
main = do
  args <- getArgs
  let input = head args
  txt <- T.readFile input
  config <- readConfig "config.json"
  let conf = case config of
              Just c -> c
              Nothing -> defaultConfig
  let sizes = confSizes conf
  let set = confSet conf
  case MMark.parse input txt of
    Left bundle -> putStrLn (M.errorBundlePretty bundle)
    Right r -> TL.writeFile (FP.takeBaseName input ++ ".html")
      . renderText -- from Lucid
      . MMark.render
      . MMark.useExtensions
             [ imgLazyExt
             , imgResExt' set sizes
             , audioExt
             ]
      $ r

-- Common function to extract base URL components
extractImageAttributes :: URI.URI -> (String, String, String)
extractImageAttributes url =
  let url' = clearStr $ show $ URI.render url
      file = FP.takeBaseName url'
      ext  = FP.takeExtension url'
      path = FP.takeDirectory url'
  in (file, ext, path)

--EXTENSIONS

-- Adding lazy attribute to images composable
imgLazyExt :: MMark.Extension
imgLazyExt = Ex.inlineRender $ \old inline ->
  case inline of
    l@(Ex.Image txt url (Just attr)) -> fromMaybe (old l) $ do
      let wo = words $ Te.unpack attr
      let mattr = if Te.null attr then Nothing else Just attr
      if "lazy" `elem` wo
        then return $ with (old (Ex.Image txt url mattr)) [loading_ "lazy"]
        else return $ old (Ex.Image txt url mattr)
    other -> old other

-- Adding srcset and sizes attributes to images composable
imgResExt :: [Int] -> String -> MMark.Extension
imgResExt set sizes = Ex.inlineRender $ \old inline ->
  case inline of
    l@(Ex.Image txt url (Just attr)) -> fromMaybe (old l) $ do
      let (file, ext, path) = extractImageAttributes url
      let mattr = if Te.null attr then Nothing else Just attr
      return $ with (old (Ex.Image txt url mattr))
                    [ srcset_ (imgSet file ext path set)
                    , sizes_ (Te.pack sizes)]
    other -> old other

-- Extension for images without title attribute but adding srcset and sizes attributes
imgResExt' :: [Int] -> String -> MMark.Extension
imgResExt' set sizes= Ex.inlineRender $ \old inline ->
  case inline of
    l@(Ex.Image txt url _) -> fromMaybe (old l) $ do
      let (file, ext, path) = extractImageAttributes url
      let src' = URI.render url
      return $ img_ [ alt_ (Ex.asPlainText txt)
                    , src_ src'
                    , srcset_ (imgSet file ext path set)
                    , sizes_ (Te.pack sizes)]
    other -> old other

-- imgSet function
imgSet :: String -> String -> String -> [Int] -> Te.Text
imgSet filebase ext path set = Te.pack $ concatMap formatSize (init set) ++ formatSize (last set)
  where
    comma size =  if size /= last set then "," else ""
    formatSize size = path ++ "/" ++ filebase ++ "_" ++ show size ++ ext ++  " " ++ show size ++ "w" ++ comma size

-- Helper function to create srcset attribute
srcset_ :: Te.Text -> Attribute
srcset_ = makeAttribute "srcset"

-- Clear quotes from string
clearStr :: String -> String
clearStr = filter (not . (`elem` ("\"" :: String)))

-- Bonus: Audio extension to render audio links
audioExt :: MMark.Extension
audioExt = Ex.inlineRender $ \old inline ->
  case inline of
    l@(Ex.Link txt uri _) ->
      case (uri ^. uriPath, Ex.asPlainText txt) of
        ([], _) -> old l
        (_, "audio") ->
          audio_ [controls_ "controls", preload_ "none"] $
               source_ [src_ (URI.render uri), type_ "audio/mp4"]
        (_, _) -> old l
    other -> old other
