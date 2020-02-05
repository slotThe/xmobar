{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Weather
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A weather monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Weather where

import Xmobar.Plugins.Monitors.Common

import qualified Control.Exception as CE

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)

import Text.ParserCombinators.Parsec
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(Option))


-- | Options the user may specify.
data WeatherOpts = WeatherOpts
  { weatherString :: String
  , useManager    :: Bool
  }

-- | Default values for options.
defaultOpts :: WeatherOpts
defaultOpts = WeatherOpts
  { weatherString = ""
  , useManager    = True
  }

-- | Apply options.
options :: [OptDescr (WeatherOpts -> WeatherOpts)]
options =
  [ Option "w" ["weathers"  ] (ReqArg (\s o -> o { weatherString = s   }) "") ""
  , Option "m" ["useManager"] (ReqArg (\b o -> o { useManager = read b }) "") ""
  ]

weatherConfig :: IO MConfig
weatherConfig = mkMConfig
       "<station>: <tempC>C, rh <rh>% (<hour>)" -- template
       ["station"                               -- available replacements
       , "stationState"
       , "year"
       , "month"
       , "day"
       , "hour"
       , "windCardinal"
       , "windAzimuth"
       , "windMph"
       , "windKnots"
       , "windKmh"
       , "windMs"
       , "visibility"
       , "skyCondition"
       , "skyConditionS"
       , "weather"
       , "tempC"
       , "tempF"
       , "dewPointC"
       , "dewPointF"
       , "rh"
       , "pressure"
       ]

data WindInfo =
    WindInfo {
         windCardinal :: String -- cardinal direction
       , windAzimuth  :: String -- azimuth direction
       , windMph      :: String -- speed (MPH)
       , windKnots    :: String -- speed (knot)
       , windKmh      :: String -- speed (km/h)
       , windMs       :: String -- speed (m/s)
    } deriving (Show)

data WeatherInfo =
    WI { stationPlace :: String
       , stationState :: String
       , year         :: String
       , month        :: String
       , day          :: String
       , hour         :: String
       , windInfo     :: WindInfo
       , visibility   :: String
       , skyCondition :: String
       , weather      :: String
       , tempC        :: Int
       , tempF        :: Int
       , dewPointC    :: Int
       , dewPointF    :: Int
       , humidity     :: Int
       , pressure     :: Int
       } deriving (Show)

pTime :: Parser (String, String, String, String)
pTime = do y <- getNumbersAsString
           char '.'
           m <- getNumbersAsString
           char '.'
           d <- getNumbersAsString
           char ' '
           (h:hh:mi:mimi) <- getNumbersAsString
           char ' '
           return (y, m, d ,h:hh:":"++mi:mimi)

noWind :: WindInfo
noWind = WindInfo "μ" "μ" "0" "0" "0" "0"

pWind :: Parser WindInfo
pWind =
  let tospace = manyTill anyChar (char ' ')
      toKmh knots = knots $* 1.852
      toMs knots  = knots $* 0.514
      ($*) :: String -> Double -> String
      op1 $* op2 = show (round ((read op1::Double) * op2)::Integer)

      -- Occasionally there is no wind and a METAR report gives simply, "Wind: Calm:0"
      wind0 = do manyTill skipRestOfLine (string "Wind: Calm:0")
                 return noWind
      windVar = do manyTill skipRestOfLine (string "Wind: Variable at ")
                   mph <- tospace
                   string "MPH ("
                   knot <- tospace
                   manyTill anyChar newline
                   return $ WindInfo "μ" "μ" mph knot (toKmh knot) (toMs knot)
      wind = do manyTill skipRestOfLine (string "Wind: from the ")
                cardinal <- tospace
                char '('
                azimuth <- tospace
                string "degrees) at "
                mph <- tospace
                string "MPH ("
                knot <- tospace
                manyTill anyChar newline
                return $ WindInfo cardinal azimuth mph knot (toKmh knot) (toMs knot)
  in try wind0 <|> try windVar <|> try wind <|> return noWind

pTemp :: Parser (Int, Int)
pTemp = do let num = digit <|> char '-' <|> char '.'
           f <- manyTill num $ char ' '
           manyTill anyChar $ char '('
           c <- manyTill num $ char ' '
           skipRestOfLine
           return (floor (read c :: Double), floor (read f :: Double))

pRh :: Parser Int
pRh = do s <- manyTill digit (char '%' <|> char '.')
         return $ read s

pPressure :: Parser Int
pPressure = do manyTill anyChar $ char '('
               s <- manyTill digit $ char ' '
               skipRestOfLine
               return $ read s

{-
    example of 'http://weather.noaa.gov/pub/data/observations/metar/decoded/VTUD.TXT':
        Station name not available
        Aug 11, 2013 - 10:00 AM EDT / 2013.08.11 1400 UTC
        Wind: from the N (350 degrees) at 1 MPH (1 KT):0
        Visibility: 4 mile(s):0
        Sky conditions: mostly clear
        Temperature: 77 F (25 C)
        Dew Point: 73 F (23 C)
        Relative Humidity: 88%
        Pressure (altimeter): 29.77 in. Hg (1008 hPa)
        ob: VTUD 111400Z 35001KT 8000 FEW030 25/23 Q1008 A2977 INFO R RWY30
        cycle: 14
-}
parseData :: Parser [WeatherInfo]
parseData =
    do (st, ss) <- try (string "Station name not available" >> return ("??", "??")) <|>
                   (do st <- getAllBut ","
                       space
                       ss <- getAllBut "("
                       return (st, ss)
                   )
       skipRestOfLine >> getAllBut "/"
       (y,m,d,h) <- pTime
       w <- pWind
       v <- getAfterString "Visibility: "
       sk <- getAfterString "Sky conditions: "
       we <- getAfterString "Weather: "
       skipTillString "Temperature: "
       (tC,tF) <- pTemp
       skipTillString "Dew Point: "
       (dC, dF) <- pTemp
       skipTillString "Relative Humidity: "
       rh <- pRh
       skipTillString "Pressure (altimeter): "
       p <- pPressure
       manyTill skipRestOfLine eof
       return [WI st ss y m d h w v sk we tC tF dC dF rh p]

defUrl :: String
defUrl = "https://tgftp.nws.noaa.gov/data/observations/metar/decoded/"

stationUrl :: String -> String
stationUrl station = defUrl ++ station ++ ".TXT"

-- | Get the decoded weather data from the given station.
getData :: Maybe Manager -> String -> IO String
getData weMan station = CE.catch
    (do man <- flip fromMaybe weMan <$> mkManager
        -- Create a new manager if none was present or the user does not want to
        -- use one.
        request <- parseUrlThrow $ stationUrl station
        res <- httpLbs request man
        return $ B.unpack $ responseBody res)
    errHandler
  where
    errHandler :: CE.SomeException -> IO String
    errHandler _ = return "<Could not retrieve data>"

formatSk :: Eq p => [(p, p)] -> p -> p
formatSk ((a,b):sks) sk = if a == sk then b else formatSk sks sk
formatSk [] sk = sk

formatWeather
    :: WeatherOpts        -- ^ Formatting options from the cfg file
    -> [(String,String)]  -- ^ 'SkyConditionS' for 'WeatherX'
    -> [WeatherInfo]      -- ^ The actual weather info
    -> Monitor String
formatWeather opts sks [WI st ss y m d h (WindInfo wc wa wm wk wkh wms) v sk we tC tF dC dF r p] =
    do cel <- showWithColors show tC
       far <- showWithColors show tF
       let sk' = formatSk sks (map toLower sk)
           we' = showWeather (weatherString opts) we
       parseTemplate [st, ss, y, m, d, h, wc, wa, wm, wk, wkh
                     , wms, v, sk, sk', we', cel, far
                     , show dC, show dF, show r , show p ]
formatWeather _ _ _ = getConfigValue naString

-- | Show the 'weather' field with a default string in case it was empty.
showWeather :: String -> String -> String
showWeather "" d = d
showWeather s  _ = s

-- | Start a weather monitor, create a new 'Maybe Manager', should the user have
-- chosen to use one.
startWeather'
    :: [(String, String)]  -- ^ 'SkyConditionS' replacement strings
    -> String              -- ^ Weather station
    -> [String]            -- ^ User supplied arguments
    -> Int                 -- ^ Update rate
    -> (String -> IO ())
    -> IO ()
startWeather' sks station args rate cb = do
    opts  <- parseOptsWith options defaultOpts (getArgvs args)
    weRef <- tryMakeManager opts
    runMD
        (station : args)
        weatherConfig
        (runWeather sks weRef opts)
        rate
        weatherReady
        cb

-- | Same as 'startWeather'', only for 'Weather' instead of 'WeatherX', meaning
-- no 'SkyConditionS'.
startWeather :: String -> [String] -> Int -> (String -> IO ()) -> IO ()
startWeather = startWeather' []

-- | Run a weather monitor.
runWeather
    :: [(String, String)]  -- ^ 'SkyConditionS' replacement strings
    -> Maybe Manager       -- ^ Whether to use a 'Manager'
    -> WeatherOpts         -- ^ Weather specific options
    -> [String]            -- ^ User supplied arguments
    -> Monitor String
runWeather sks weMan opts args = do
    d <- io $ getData weMan (head args)
    i <- io $ runP parseData d
    formatWeather opts sks i

weatherReady :: [String] -> Monitor Bool
weatherReady str = io $ do
    initRequest <- parseUrlThrow $ stationUrl $ head str
    let request = initRequest { method = methodHead }

    CE.catch
        (do man <- mkManager
            res  <- httpLbs request man
            return $ checkResult $ responseStatus res)
        errHandler
  where
    -- | If any exception occurs, indicate that the monitor is not ready.
    errHandler :: CE.SomeException -> IO Bool
    errHandler _ = return False

    -- | Check for and indicate any errors in the http response.
    checkResult :: Status -> Bool
    checkResult status
        | statusIsServerError status = False
        | statusIsClientError status = False
        | otherwise = True

-- | Possibly create a new 'Manager', based upon the users preference.  If one
-- is created, this 'Manager' will be used throughout the monitor.
tryMakeManager :: WeatherOpts -> IO (Maybe Manager)
tryMakeManager opts =
    if useManager opts
        then Just <$> mkManager
        else pure Nothing

-- | Create a new 'Manager' for managing network connections.
mkManager :: IO Manager
mkManager = newManager $ tlsManagerSettings { managerConnCount = 1 }
