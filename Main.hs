{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format.Human
import qualified Data.Vector as V
import qualified Network.Wreq as W
import System.Directory
import Lucid
import Text.Regex (mkRegex, matchRegex)

data PreexistingClosing = PreexistingClosing {
    _preexistingClosingDay :: Day
  , _preexistingClosingReason :: T.Text
  } deriving (Eq, Ord, Show)

preexistingClosures :: [PreexistingClosing]
preexistingClosures =
  [ PreexistingClosing (fromGregorian 2015 11 09) "Veterans Day"
  , PreexistingClosing (fromGregorian 2015 11 25) "Thanksgiving (offices open; no classes)"
  , PreexistingClosing (fromGregorian 2015 11 26) "Thanksgiving"
  , PreexistingClosing (fromGregorian 2015 11 27) "Columbus Day Observed"
  , PreexistingClosing (fromGregorian 2016 01 18) "Martin Luther King Day"
  ]

data ClosingStatus = ClosingStatus {
    _isClosed :: Bool
  , _numClosings :: Int
  , _numAlerts :: Int
  } deriving (Eq, Ord, Show)

instance A.ToJSON ClosingStatus where
  toJSON (ClosingStatus i nc na) = A.object [ "is_closed" A..= i
                                            , "closings" A..= nc
                                            , "alerts" A..= na
                                            ]

main :: IO ()
main = scotty 3000 $ do
  get "/json" $ do
    setHeader "Cache-Control" "no-cache"
    wkbn   <- liftIO $ W.get "http://wx.wkbn.com/weather/WKBN_closings_delays.html"
    wundergroundKey <- liftIO $ readFile "/etc/isysuclosed/wunderground_api_key"
    alerts <- liftIO $ getAlerts wundergroundKey
    let closings = closingCount (wkbn ^. W.responseBody)
        alertCount = alerts ^? key "alerts" . _Array . to V.length
    json $ ClosingStatus (isMentioned (wkbn ^. W.responseBody)) closings (fromMaybe 0 alertCount)
  get "/" $ do
    setHeader "Cache-Control" "no-cache"
    wkbn   <- liftIO $ W.get "http://wx.wkbn.com/weather/WKBN_closings_delays.html"
    wundergroundKey <- liftIO $ readFile "/etc/isysuclosed/wunderground_api_key"
    wx     <- liftIO $ getConditions wundergroundKey
    alerts <- liftIO $ getAlerts wundergroundKey
    since  <- liftIO lastAlertsTime
    current <- liftIO getCurrentTime
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
        body_ $ do
          case isPreexistingClosure (utctDay current) of
            Just closure -> ysuClosed closure
            Nothing -> weatherCheckBody wx closings wkbn alertCount alerts since
          footer

isPreexistingClosure :: Day -> Maybe PreexistingClosing
isPreexistingClosure day =
  find (\x -> _preexistingClosingDay x == day) preexistingClosures

ysuClosed :: PreexistingClosing -> HtmlT Identity ()
ysuClosed closing =
  div_ [class_ "container"] $ do
    h1_ "YSU Closing Status"
    p_ [class_ "t"] $ do
      "YES! YSU is closed today for "
      strong_ (toHtml $ _preexistingClosingReason closing)
      "!"
    p_ [class_ "t"] "Enjoy your rare day off!"

weatherCheckBody
  :: (AsValue s, AsValue s1, ToHtml r,
      Term [Attribute] (HtmlT Identity () -> t)) =>
     s1 -> Int -> W.Response BL.ByteString -> Maybe Int -> s -> r -> t
weatherCheckBody wx closings wkbn alertCount alerts since = do
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
      "There "
      if closings == 1 then "is " else "are "
      "currently "
      strong_ . toHtml . show $ closings
      if closings == 1 then " delay or closing " else " delays/closings "
      "according to a local (Youngstown) news source."
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
    p_ [style_ "text-align: center;"] $ do
      constructTweet
        closings
        alertCount
        (wx ^? key "current_observation" . key "feelslike_string" . _String)
        (wx ^? key "current_observation" . key "weather" . _String)
        (isMentioned (wkbn ^. W.responseBody))

footer :: HtmlT Identity ()
footer = do
  br_ []
  small_ [style_ "display: block; text-align: center"] $ do
    "This website is not affiliated Youngstown State University in any way. It "
    "was "
    (a_ [href_ "https://github.com/relrod/isysuclosed.com/"] "written")
    " to make a point."
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
  twitter


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

-- | Let's start playing hard-ass. Here we make it easy to *share* YSU's stupid
-- decisions.

twitter :: HtmlT Identity ()
twitter = script_ . T.concat $
          [ "window.twttr=(function(d,s,id){var js,fjs=d.getElementsByTagName"
          , "(s)[0],t=window.twttr||{};if(d.getElementById(id))return;js=d."
          , "createElement(s);js.id=id;js.src=\"https://platform.twitter.com/"
          , "widgets.js\";fjs.parentNode.insertBefore(js,fjs);t._e=[];t.ready"
          , "=function(f){t._e.push(f);};return t;}(document,\"script\",\""
          , "twitter-wjs\"));"
          ]

constructTweet :: Int -> Maybe Int -> Maybe T.Text -> Maybe T.Text -> Bool -> HtmlT Identity ()
constructTweet nc na wc cond yClosed =
  a_ [ class_ "twitter-share-button"
     , href_ "https://twitter.com/share"
     , data_ "url" "https://isysuclosed.com"
     , data_ "text" (T.pack tweetText)
     ] (toHtml tweetText)
  where
    pluralityClosings = if nc == 1
                        then "closing"
                        else "closings"
    pluralityAlerts = if (fromMaybe 0 na) == 1
                      then "alert"
                      else "alerts"

    -- This is our ideal, but longest tweet. It probably spans > 140 chars.
    idealTweet = show nc ++ " " ++ pluralityClosings ++ ", " ++
                 show (fromMaybe 0 na) ++ " " ++ pluralityAlerts ++
                 ", " ++ T.unpack (fromMaybe "(unknown)" cond) ++ "/" ++
                 T.unpack (fromMaybe "(unknown)" wc) ++
                 " and still not closed. #YSU"

    -- Here's our fallback. We cut out the alert count.
    fallbackTweet = show nc ++ " " ++ pluralityClosings ++ ", " ++
                    T.unpack (fromMaybe "(unknown)" cond) ++ "/" ++
                    T.unpack (fromMaybe "(unknown)" wc) ++
                    " and still not closed. #YSU"

    -- If we're still too far over the limit, cut out weather conditions.
    fallbackTweet2 = show nc ++ " " ++ pluralityClosings ++ ", " ++
                     T.unpack (fromMaybe "(unknown)" wc) ++
                     " and still not closed. #YSU"

    -- Lastly, give up and __only__ show closing count.
    giveUpTweet = show nc ++ " " ++ pluralityClosings ++ " but still not #YSU. "


    -- We have to do some toying around here. First off...
    tweetText
      | yClosed == True = "Hell has frozen over! #YSU is #Closed!"
    -- But now we have to be careful about length. We have 140 chars to work
    -- with.
      | length idealTweet <= 140 = idealTweet
      | length fallbackTweet <= 140 = fallbackTweet
      | length fallbackTweet2 <= 140 = fallbackTweet2
      | otherwise = giveUpTweet
