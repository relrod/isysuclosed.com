{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format.Human
import qualified Data.Vector as V
import qualified Network.Wreq as W
import System.Directory
import Text.Blaze
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Regex (mkRegex, matchRegex)

main :: IO ()
main = scotty 3000 $
  get "/" $ do
    setHeader "Cache-Control" "no-cache"
    wkbn   <- liftIO $ W.get "http://wkbn.com/closings/"
    wundergroundKey <- liftIO $ readFile "/etc/isysuclosed/wunderground_api_key"
    wx     <- liftIO $ getConditions wundergroundKey
    alerts <- liftIO $ getAlerts wundergroundKey
    since  <- liftIO lastAlertsTime
    let closings = closingCount (wkbn ^. W.responseBody)
        alertCount = alerts ^? key "alerts" . _Array . to V.length
    html $ renderHtml $
      H.docTypeHtml $ do
        H.head $ do
          H.meta ! A.charset "utf-8"
          H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
          H.script ! A.src "//cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js" $ mempty
          H.script ! A.src "//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/js/bootstrap.min.js" $ mempty
          H.link ! A.href "//fonts.googleapis.com/css?family=Open+Sans" ! A.rel "stylesheet" ! A.type_ "text/css"
          H.link ! A.href "//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/css/bootstrap.min.css"
                 ! A.rel "stylesheet"
                 ! A.type_ "text/css"
          style
          H.title "YSU Closing Status"
          analytics
        H.body $
          H.div ! A.class_ "container" $ do
            H.h1 "YSU Closing Status"
            H.p ! A.class_ "t" $ "So, here's the deal:"
            H.p ! A.class_ "t" $ toMarkup (intersperse " "
              [ "The weather is currently"
              , H.strong . toMarkup $ fromMaybe "(unknown)" (wx ^? key "current_observation" . key "weather" . _String)
              , "and"
              , H.strong . toMarkup $ fromMaybe "(unknown)" (wx ^? key "current_observation" . key "temperature_string" . _String)
              ]) <> "."
            H.p ! A.class_ "t" $ toMarkup (intersperse " "
              [ "There are currently"
              , H.strong (toMarkup . show $ closings)
              , "delays/closings according to a local (Youngstown) news source."
              ])
            H.p ! A.class_ "t" $ do
              _ <- "Youngstown State University "
              H.strong $
                if isMentioned (wkbn ^. W.responseBody)
                then H.span ! A.style "color: green;" $ "WAS mentioned"
                else H.span ! A.style "color: red;" $ "was NOT mentioned"
              " among them."
            H.p ! A.class_ "t" $ toMarkup (intersperse " "
              [ "There are currently"
              , H.strong . toMarkup . maybe "unknown" show $ alertCount
              , "weather alert(s) covering Youngstown as of"
              , H.strong (toMarkup since)
              ]) <> "."
            when (fromMaybe 0 alertCount /= 0) $
              H.ul $
                mapM_ (\w -> H.li $ do
                         H.strong . toMarkup $ w ^. key "description" . _String
                         _ <- " expiring "
                         toMarkup $ w ^. key "expires" . _String) (alerts ^.. key "alerts" . values)
            H.hr
            H.p ! A.style "text-align: center;" $
              H.small $ "This website is not affiliated with Youngstown " <>
                        "State University in any way. It was "<>
                        (H.a ! A.href "https://github.com/relrod/isysuclosed.com/" $ "written") <>
                        " to " <>
                        "make a point."
            H.p ! A.style "text-align: center;" $
              H.small $ do
                _ <- "While hopefully accurate, this is NOT an official "
                _ <- "resource. Always confirm with "
                H.a ! A.href "https://swww.ysu.edu/downloads/closing_procedure.pdf" $ "official"
                " resources."
            H.p ! A.style "text-align: center; color: #888888" $
              H.small "Valid HTML5. Weather information via Weather Underground."
            H.img ! A.style "display: block; margin: 0 auto; width: 180px;"
                  ! A.src "http://icons.wxug.com/logos/images/wundergroundLogo_4c_horz.jpg"
                  ! A.alt "Weather Underground Logo"

analytics :: Html
analytics = H.script . H.preEscapedToHtml . T.concat $
            [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
            , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
            , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
            , "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"
            , "ga('create', 'UA-21458826-6', 'auto');"
            , "ga('send', 'pageview');"
            ]

style :: Html
style = H.style . H.preEscapedToHtml . T.concat $
        [ "* { font-family: \"Proxima Nova\", \"Open Sans\", sans-serif !important; }"
        , "h1 { font-weight: bold; text-align: center; }"
        , "p.t { font-size: 1.8em; text-align: center; }"
        ]

-- | First, try reading from the cache to get the last modified time.
-- In the case where the cache doesn't exist, or it exists, but is more than 30
-- minutes old, we write out a new cache and return its new contents.
-- Otherwise, we just return the cache.
--
-- We do this so we don't go over the wunderground API limit.
getAlerts :: String -> IO BL.ByteString
getAlerts key' = do
  current <- getCurrentTime
  a <- try (getModificationTime "/var/tmp/isysuclosed_alerts.json") :: IO (Either IOException UTCTime)
  case a of
    Left _  -> writeAlerts
    Right t ->
      let offset = fromRational . toRational $ diffUTCTime current t :: Double
      in if offset > 1800
         then writeAlerts
         else BL.readFile "/var/tmp/isysuclosed_alerts.json"
  where
    getAlertsFromWU = do
      r <- W.get ("https://api.wunderground.com/api/" <> key' <> "/alerts/q/OH/Youngstown.json")
      return $ BL.dropWhile (=='\n') (r ^. W.responseBody)

    writeAlerts = do
      alerts <- getAlertsFromWU
      BL.writeFile "/var/tmp/isysuclosed_alerts.json" alerts
      return alerts

lastAlertsTime :: IO String
lastAlertsTime = do
  a <- try (getModificationTime "/var/tmp/isysuclosed_alerts.json") :: IO (Either IOException UTCTime)
  case a of
    Left _  -> return "(never)"
    Right t -> humanReadableTime t

-- | First, try reading from the cache to get the last modified time.
-- In the case where the cache doesn't exist, or it exists, but is more than 30
-- minutes old, we write out a new cache and return its new contents.
-- Otherwise, we just return the cache.
--
-- We do this so we don't go over the wunderground API limit.
getConditions :: String -> IO BL.ByteString
getConditions key' = do
  current <- getCurrentTime
  a <- try (getModificationTime "/var/tmp/isysuclosed_wx.json") :: IO (Either IOException UTCTime)
  case a of
    Left _  -> writeConditions
    Right t ->
      let offset = fromRational . toRational $ diffUTCTime current t :: Double
      in if offset > 1800
         then writeConditions
         else BL.readFile "/var/tmp/isysuclosed_wx.json"
  where
    getConditionsFromWU = do
      r <- W.get ("https://api.wunderground.com/api/" <> key' <> "/conditions/q/OH/Youngstown.json")
      return $ BL.dropWhile (=='\n') (r ^. W.responseBody)

    writeConditions = do
      wx <- getConditionsFromWU
      BL.writeFile "/var/tmp/isysuclosed_wx.json" wx
      return wx

isMentioned :: BL.ByteString -> Bool
isMentioned y = B.pack "Youngstown State University:</strong>" `B.isInfixOf` BL.toStrict y

closingCount :: BL.ByteString -> Int
closingCount x =
  let regex' = mkRegex "([0-9]+) Closings &amp; Delays"
      matched = fmap (read . head) (matchRegex regex' (BL.unpack x))
  in fromMaybe 0 matched
