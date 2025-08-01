{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO      as T
import qualified Data.Text.Lazy.IO as TL
import qualified Text.MMark        as MMark
import qualified Text.Megaparsec   as M
import qualified Text.MMark.Extension as Ex
import qualified Data.Text as Te
import qualified Text.URI as URI
import qualified System.FilePath as FP
import Lucid.Base (makeAttribute)
import Lucid
import Data.Maybe (fromMaybe)
import Text.URI.Lens (uriPath)
import Text.URI.QQ (scheme)
import Lens.Micro ((^.))

main :: IO ()
main = do
  let input = "input.md"
  txt <- T.readFile input
  case MMark.parse input txt of
    Left bundle -> putStrLn (M.errorBundlePretty bundle)
    Right r -> TL.writeFile "output.html"
      . renderText --from Lucid
      . MMark.render
      . MMark.useExtensions
             [  imgLazyExt
              , imgResExt'
              , audioExt
              ]
      $ r

--adding lazy attribute to images composable
imgLazyExt :: MMark.Extension
imgLazyExt = Ex.inlineRender $ \old inline ->
         case inline of
            l@(Ex.Image txt url (Just attr)) -> fromMaybe (old l) $ do
              let wo = (words $ Te.unpack attr)
              let mattr = if Te.null attr then Nothing else Just attr
              case "lazy" `elem` wo of
               True -> return $ with (old (Ex.Image txt url mattr)) [loading_ "lazy"]
               False -> return $ with (old (Ex.Image txt url mattr)) []
            other -> old other

--adding srcset and sizes attributes to images composable
imgResExt :: MMark.Extension
imgResExt = Ex.inlineRender $ \old inline ->
           case inline of
             l@(Ex.Image txt url (Just attr)) -> fromMaybe (old l) $ do
                let url' = clear_str $ show $ URI.render url
                let file = FP.takeBaseName url'
                let ext = FP.takeExtension url'
                let path = FP.takeDirectory url'
                let mattr = if Te.null attr then Nothing else Just attr
                return $ with (old (Ex.Image txt url mattr)) [srcset_ (imgSet file ext path), sizes_ defSizes]
             other -> old other


--adding srcset and sizes attributes
-- but wihtout title atribute
--and getting original alt and src
-- note the use of _ instead of (Just attr)
imgResExt' :: MMark.Extension
imgResExt' = Ex.inlineRender $ \old inline ->
           case inline of
             l@(Ex.Image txt url _) -> fromMaybe (old l) $ do
                let url' = clear_str $ show $ URI.render url
                let file = FP.takeBaseName url'
                let ext = FP.takeExtension url'
                let path = FP.takeDirectory url'
                let src' = URI.render url
                return $ img_ [alt_ (Ex.asPlainText txt),src_ src', srcset_ (imgSet file ext path), sizes_ defSizes]
             other -> old other

--defining sizes value
defSizes :: Te.Text
defSizes = Te.pack $ "(max-width:600px) 100vw, 850px"

--this function should be generalized
--according to name and file conventions by your org
--and the case use of your webpage
imgSet :: String -> String -> String -> Te.Text
imgSet filebase ext path = Te.pack $ (path ++ "/" ++ filebase ++ "_400" ++ ext ++ " 400w,"
                                      ++ path ++ "/" ++ filebase ++ "_850" ++ ext ++ " 850w")

-- The @srcset@ attribute
-- Lucid provide makeAttribute that allow us to create a new attribute
srcset_ :: Te.Text -> Attribute
srcset_ val = makeAttribute "srcset" val

--clear "" from show"
clear_str :: String -> String
clear_str xs = filter (not . (`elem` ("\""::String))) xs


--audio extension
--note the diff using Ex.Link vs Ex.Image from previous examples
audioExt :: MMark.Extension
audioExt = Ex.inlineRender $ \old inline ->
   case inline of
     l@(Ex.Link txt uri _ ) ->
         case (uri ^. uriPath, Ex.asPlainText txt) of
           ([], _) -> old l
           (_, "audio") ->
             let g x = URI.unRText x
              in audio_ [controls_ "controls", preload_ "none"] $ do
               source_ [src_ (URI.render uri) , type_ "audio/mp4"]
           (_,_) -> old l
     other -> old other
