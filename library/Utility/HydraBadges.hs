-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Utility/HydraBadges.hs
--
-- License:
--   Copyright 2017 Remy Goldschmidt
--
--   Permission is hereby granted, free of charge, to any person obtaining a
--   copy of this software and associated documentation files (the "Software"),
--   to deal in the Software without restriction, including without limitation
--   the rights to use, copy, modify, merge, publish, distribute, sublicense,
--   and/or sell copies of the Software, and to permit persons to whom the
--   Software is furnished to do so, subject to the following conditions:
--
--   The above copyright notice and this permission notice shall be included
--   in all copies or substantial portions of the Software.
--
--   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
--   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
--   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}

-- |
--   Module      : Utility.HydraBadges
--   Copyright   : Copyright 2017 Remy Goldschmidt
--   License     : MIT
--   Maintainer  : taktoa@gmail.com
--   Stability   : experimental
module Utility.HydraBadges where

import           Numeric                      (showHex)

import qualified Web.Scotty                   as S

import qualified "svg-tree" Graphics.Svg                 as SVG
import qualified Text.XML.Light               as XML

import qualified Graphics.Rasterific.Svg      as R

import qualified Graphics.Badge.Barrier       as B
import qualified Graphics.Badge.Barrier.Color as B (Color (Color))

import           Control.Lens                 hiding ((.>), (<.), (<|), (|>))
import qualified Control.Lens.Setter          as Lens (set)
import qualified Control.Lens.TH              as Lens

import qualified Codec.Picture.Png            as JP

import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS

import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map

import qualified Network.Wreq                 as Wreq

import qualified Data.Aeson                   as Aeson

import           Data.Aeson.Lens

import           Data.Maybe
import           Data.Monoid
import           Data.Word

import           Control.Arrow

import           Control.Monad.IO.Class

import           Flow

type ProjectName = Text
type JobsetName  = Text
type JobName     = Text
type Endpoint    = Text
type EvalID      = Int
type BuildID     = Int

hydraGET :: Endpoint -> IO Aeson.Value
hydraGET endpoint = do
  let baseURL = "https://hydra.angeldsis.com"
  let url = Text.unpack (baseURL <> "/" <> endpoint)
  let options = Wreq.defaults
                |> Wreq.header "Accept"  .~ ["application/json"]
                |> Wreq.header "Referer" .~ [Text.encodeUtf8 baseURL]
  resp <- Wreq.getWith options url
  (resp ^. Wreq.responseBody)
    |> Aeson.decode
    |> maybe (fail "get failed lol") pure

data BuildStatus
  = BuildStatusUnknown
  | BuildStatusFailure
  | BuildStatusSuccess
  deriving (Eq, Show)

requestBuildStatus :: (ProjectName, JobsetName) -> IO (BuildStatus, EvalID)
requestBuildStatus (project, jobset) = do
  let jsonCast :: (Aeson.ToJSON a, Aeson.FromJSON b) => a -> Maybe b
      jsonCast = Aeson.encode .> Aeson.decode

  evalsJSON <- hydraGET ("jobset/" <> project <> "/" <> jobset <> "/evals")

  evalID <- ((evalsJSON ^? key "evals" . nth 0 . key "id") >>= jsonCast)
            |> id @(Maybe EvalID)
            |> maybe (fail "lmao nothing") pure

  buildsList <- ((evalsJSON ^? key "evals" . nth 0 . key "builds") >>= jsonCast)
                |> id @(Maybe [BuildID])
                |> maybe (fail "lmao nothing 2") pure

  let requestStatus :: BuildID -> IO Word8
      requestStatus buildID = do
        buildJSON <- hydraGET (Text.pack ("build/" <> show buildID))
        ((buildJSON ^? key "buildstatus") >>= jsonCast)
          |> id @(Maybe Word8)
          |> maybe (fail "lmao nothing 3") pure

  case buildsList of
    [] -> pure (BuildStatusUnknown, evalID)
    bs -> do statuses <- mapM requestStatus bs
             pure $ if all (== 0) statuses
                    then (BuildStatusSuccess, evalID)
                    else (BuildStatusFailure, evalID)

data BadgeWrapper = forall b. (B.Badge b) => BadgeWrapper b

parseBadgeWrapper :: Text -> Maybe BadgeWrapper
parseBadgeWrapper "flat"        = Just (BadgeWrapper B.flat)
parseBadgeWrapper "flat-square" = Just (BadgeWrapper B.flatSquare)
parseBadgeWrapper "plastic"     = Just (BadgeWrapper B.plastic)
parseBadgeWrapper _             = Nothing

page :: Text -> Text
page url = Text.unlines
           [ "<!DOCTYPE html>"
           , "<html>"
           , "  <head>"
           , "    <meta charset=\"utf-8\">"
           , "  </head>"
           , "  <body>"
           , "    <img id=\"badge\" src=\"" <> url <> "\" />"
           , "  </body>"
           , "</html>"
           ]

badgeTypeParam :: S.ActionM BadgeWrapper
badgeTypeParam = do
  ty <- S.param "type"
  let invalidType = S.raise (LText.fromStrict ("invalid badge type: " <> ty))
  parseBadgeWrapper ty
    |> maybe invalidType pure

queryString :: S.ActionM Text
queryString = S.params
              |> fmap (map showParam .> LText.intercalate "&" .> LText.toStrict)
  where
    showParam :: S.Param -> LText.Text
    showParam (k, v) = k <> "=" <> v

htmlEscapeText :: Text -> Text
htmlEscapeText = LText.fromStrict .> htmlEscapeLText .> LText.toStrict

htmlEscapeLText :: LText.Text -> LText.Text
htmlEscapeLText = [ LText.replace "&"  "&amp;"
                  , LText.replace "<"  "&lt;"
                  , LText.replace ">"  "&gt;"
                  , LText.replace "'"  "&#39;"
                  , LText.replace "\"" "&quot;"
                  ] |> map Endo |> mconcat |> appEndo

htmlEscapeBS :: BS.ByteString -> BS.ByteString
htmlEscapeBS = Text.decodeUtf8 .> htmlEscapeText .> Text.encodeUtf8

htmlEscapeLBS :: LBS.ByteString -> LBS.ByteString
htmlEscapeLBS = LText.decodeUtf8 .> htmlEscapeLText .> LText.encodeUtf8

data Color
  = Color !Word8 !Word8 !Word8
  deriving (Eq, Read, Show)

renderColor :: Color -> Text
renderColor (Color r g b) = "#" <> render r <> render g <> render b
  where
    render :: Word8 -> Text
    render n = case showHex n "" of
                 [x]    -> "0" <> Text.singleton x
                 [x, y] -> Text.pack [x, y]
                 _      -> error "what the fuck"

data BadgeType
  = Flat
  | FlatSquare
  | Plastic
  deriving (Eq, Read, Show)

data Badge
  = Badge
    { _badgeType       :: BadgeType
    , _badgeLeftText   :: Text
    , _badgeLeftColor  :: Color
    , _badgeRightText  :: Text
    , _badgeRightColor :: Color
    }
  deriving (Eq, Read, Show)

Lens.makeLenses ''Badge

renderBadge :: Badge -> LBS.ByteString
renderBadge (Badge ty lT lC rT rC)
  = case ty of Flat       -> render B.flat
               FlatSquare -> render B.flatSquare
               Plastic    -> render B.plastic
  where
    toBColor :: Color -> B.Color
    toBColor = renderColor .> B.Color

    render :: (B.Badge b, B.HasLeftColor b, B.HasRightColor b)
           => b -> LBS.ByteString
    render = (\b -> B.renderBadge b lT rT)
             . Lens.set B.left  (toBColor lC)
             . Lens.set B.right (toBColor rC)

red :: Color
red = Color 255 0 0

green :: Color
green = Color 0 255 0

grey :: Color
grey = Color 120 120 120

defaultBadge :: Badge
defaultBadge = Badge Flat "" (Color 0 0 0) "" grey

modifyBadge :: Map Text Text -> Badge -> Badge
modifyBadge params = id -- FIXME: allow customization with params

badgeResponse :: [(Text, Text)]
              -> ProjectName
              -> JobsetName
              -> IO LBS.ByteString
badgeResponse params project jobset = do
  buildStatus <- requestBuildStatus (project, jobset)
  let (rT, rC) = case fst buildStatus of
                   BuildStatusUnknown -> ("unknown", grey)
                   BuildStatusFailure -> ("failing", red)
                   BuildStatusSuccess -> ("passing", green)
  modifyBadge (Map.fromList params) defaultBadge
    |> Lens.set badgeLeftText   (project <> "/" <> jobset)
    |> Lens.set badgeLeftColor  (Color 0 0 0)
    |> Lens.set badgeRightText  rT
    |> Lens.set badgeRightColor rC
    |> renderBadge
    |> pure

server :: S.ScottyM ()
server = do
  let fontCache = "fonty-texture-cache"

  S.get "/:project/:jobset/badge.svg" $ do
    project <- S.param "project"
    jobset  <- S.param "jobset"
    params  <- S.params |> fmap (map (LText.toStrict *** LText.toStrict))
    S.setHeader "Content-Type" "image/svg+xml"
    liftIO (badgeResponse params project jobset) >>= S.raw

  -- S.get "/badge.svg" $ do
  --   btype <- badgeTypeParam
  --   left  <- S.param "left"
  --   right <- S.param "right"
  --   S.setHeader "Content-Type" "image/svg+xml"
  --   S.raw (renderBadge btype left right)
  --
  -- S.get "/badge.png" $ do
  --   btype <- badgeTypeParam
  --   left  <- S.param "left"
  --   right <- S.param "right"
  --   query <- queryString
  --   let url = "/badge.svg?" <> query
  --   let svgBS = renderBadge btype left right
  --   let parseFailure = [ "failed to parse SVG: <div>"
  --                      , htmlEscapeLBS svgBS
  --                      , "</div>"
  --                      ] |> mconcat |> LText.decodeUtf8 |> S.raise
  --   doc <- SVG.parseSvgFile (Text.unpack url) (LBS.toStrict svgBS)
  --          |> maybe parseFailure pure
  --   img <- liftIO $ do
  --     cache <- R.loadCreateFontCache fontCache
  --     fst <$> R.renderSvgDocument cache (Just (1024, 768)) 96 doc
  --   S.setHeader "Content-Type" "image/png"
  --   S.raw (JP.encodePng img)
  --
  -- S.get "/badge.html" $ do
  --   query <- queryString
  --   let url = "/badge.svg?" <> query
  --   S.html (LText.fromStrict (page url))

main :: IO ()
main = S.scotty 1234 server
