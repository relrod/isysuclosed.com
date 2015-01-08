{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format.Human
import qualified Data.Vector as V
import qualified Network.Wreq as W
import System.Directory
import Lucid
import Text.Regex (mkRegex, matchRegex)

main :: IO ()
main = scotty 3000 $
  get "/" $ do
    setHeader "Cache-Control" "no-cache"
    wkbn   <- liftIO $ W.get "http://wx.wkbn.com/weather/WKBN_closings_delays.html"
    wundergroundKey <- liftIO $ readFile "/etc/isysuclosed/wunderground_api_key"
    wx     <- liftIO $ getConditions wundergroundKey
    alerts <- liftIO $ getAlerts wundergroundKey
    since  <- liftIO lastAlertsTime
    let closings = closingCount (wkbn ^. W.responseBody)
        alertCount = alerts ^? key "alerts" . _Array . to V.length
    html $ renderText $
      doctypehtml_ $ do
        head_ $ do
          meta_ [charset_ "utf-8"]
          meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
          link_ [href_ "//fonts.googleapis.com/css?family=Open+Sans", rel_ "stylesheet", type_ "text/css"]
          link_ [ href_ "//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/css/bootstrap.min.css"
                , rel_ "stylesheet"
                , type_ "text/css"
                ]
          style
          title_ "YSU Closing Status"
          analytics
        body_ $
          div_ [class_ "container"] $ do
            h1_ "YSU Closing Status"
            p_ [class_ "t"] "So, here's the deal:"
            p_ [class_ "t"] $ do
              "The weather is currently "
              strong_ . toHtml $ fromMaybe "(unknown)" (wx ^? key "current_observation" . key "weather" . _String)
              " and "
              strong_ . toHtml $ fromMaybe "(unknown)" (wx ^? key "current_observation" . key "temperature_string" . _String)
              "."
            p_ [class_ "t"] $ do
              "With the windchill, it feels like "
              strong_ . toHtml $ fromMaybe "(unknown)" (wx ^? key "current_observation" . key "feelslike_string" . _String)
              "."
            p_ [class_ "t"] $ do
              "There are currently "
              strong_ . toHtml . show $ closings
              " delays/closings according to a local (Youngstown) news source."
            p_ [class_ "t"] $ do
              "Youngstown State University "
              strong_ $
                if isMentioned (wkbn ^. W.responseBody)
                then span_ [style_ "color: green;"] $ "WAS mentioned"
                else span_ [style_ "color: red;"] $ "was NOT mentioned"
              " among them."
            p_ [class_ "t"] $ do
              "There "
              if fromMaybe 0 alertCount == 1 then "is " else "are "
              "currently "
              strong_ . toHtml $ maybe "unknown" show alertCount
              " weather "
              if fromMaybe 0 alertCount == 1 then "alert " else "alerts "
              "covering Youngstown as of "
              strong_  . toHtml $ since
              "."
            when (fromMaybe 0 alertCount /= 0) $
              ul_ $
                mapM_ (\w -> li_ $ do
                         strong_ . toHtml $ w ^. key "description" . _String
                         " expiring "
                         w ^. key "expires" . _String . to toHtml) (alerts ^.. key "alerts" . values)
            hr_ []
            p_ [style_ "text-align: center;"] $
              small_ $ "This website is not affiliated Youngstown " <>
                        "State University in any way. It was "<>
                        (a_ [href_ "https://github.com/relrod/isysuclosed.com/"] "written") <>
                        " to " <>
                        "make a point."
            p_ [style_ "text-align: center;"] $
              small_ $ do
                "While hopefully accurate, this is NOT an official "
                "resource. Always confirm with "
                a_ [href_ "https://swww.ysu.edu/downloads/closing_procedure.pdf"] "official"
                " resources."
            p_ [style_ "text-align: center; color: #888888"] $
              small_ "Valid HTML5. Weather information via Weather Underground."
            img_ [ style_ "display: block; margin: 0 auto; width: 180px;"
                      , src_ "http://icons.wxug.com/logos/images/wundergroundLogo_4c_horz.jpg"
                      , alt_ "Weather Underground Logo"
                      ]

analytics :: HtmlT Identity ()
analytics = script_ . T.concat $
            [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
            , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
            , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
            , "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"
            , "ga('create', 'UA-21458826-6', 'auto');"
            , "ga('send', 'pageview');"
            ]

style :: HtmlT Identity ()
style = style_ . T.concat $
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
isMentioned y = B.pack "Youngstown State University</b>:" `B.isInfixOf` BL.toStrict y

closingCount :: BL.ByteString -> Int
closingCount x =
  let regex' = mkRegex "([0-9]+) closings?/delays? under All"
      matched = fmap (read . head) (matchRegex regex' (BL.unpack x))
  in fromMaybe 0 matched
