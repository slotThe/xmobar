{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.UVMeter
-- Copyright   :  (c) Róman Joost
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Róman Joost
-- Stability   :  unstable
-- Portability :  unportable
--
-- An australian uv monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.UVMeter where

import Xmobar.Plugins.Monitors.Common

import qualified Control.Exception as CE
import Network.HTTP.Conduit
    ( Manager
    , httpLbs
    , managerConnCount
    , newManager
    , parseRequest
    , responseBody
    , tlsManagerSettings
    )
import Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(Option))
import Text.Read (readMaybe)
import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)


-- | Options the user may specify.
newtype UVMeterOpts = UVMeterOpts
  { useManager :: Bool
  }

-- | Default values for options.
defaultOpts :: UVMeterOpts
defaultOpts = UVMeterOpts
  { useManager = True
  }

-- | Apply options.
options :: [OptDescr (UVMeterOpts -> UVMeterOpts)]
options =
  [ Option "m" ["useManager"] (ReqArg (\m o -> o { useManager = read m }) "") ""
  ]

uvConfig :: IO MConfig
uvConfig = mkMConfig
       "<station>" -- template
       ["station"                               -- available replacements
       ]

newtype UvInfo = UV { index :: String }
    deriving (Show)

uvURL :: String
uvURL = "https://uvdata.arpansa.gov.au/xml/uvvalues.xml"

-- | Get the UV data from the given url.
getData :: Maybe Manager -> IO String
getData uvMan = CE.catch
    (do man <- flip fromMaybe uvMan <$> mkManager
        -- Create a new manager if none was present or the user does not want to
        -- use one, otherwise use the provided manager.
        request <- parseRequest uvURL
        res <- httpLbs request man
        return $ B.unpack $ responseBody res)
    errHandler
  where
    errHandler :: CE.SomeException -> IO String
    errHandler _ = return "<Could not retrieve data>"

textToXMLDocument :: String -> Either ParseError [XML]
textToXMLDocument = parse document ""

formatUVRating :: Maybe Float -> Monitor String
formatUVRating Nothing = getConfigValue naString
formatUVRating (Just x) = do
    uv <- showWithColors show x
    parseTemplate [uv]

getUVRating :: String -> [XML] ->  Maybe Float
getUVRating locID (Element "stations" _ y:_) = getUVRating locID y
getUVRating locID (Element "location" [Attribute attr] ys:xs)
    | locID == snd attr = getUVRating locID ys
    | otherwise = getUVRating locID xs
getUVRating _ (Element "index" [] [Body rate]:_) = readMaybe rate
getUVRating locID (_:xs) = getUVRating locID xs
getUVRating _ [] = Nothing

-- | Start the uvmeter monitor, create a new 'Maybe Manager', should the user have
-- chosen to use one.
startUVMeter
    :: String    -- ^ Station
    -> [String]  -- ^ User supplied arguments
    -> Int       -- ^ Update rate
    -> (String -> IO ())
    -> IO ()
startUVMeter station args rate cb = do
    opts  <- parseOptsWith options defaultOpts (getArgvs args)
    uvMan <- tryMakeManager opts
    runM (station : args) uvConfig (runUVMeter uvMan) rate cb

runUVMeter :: Maybe Manager -> [String] -> Monitor String
runUVMeter _ [] = return "N.A."
runUVMeter uvMan (s:_) = do
    resp <- io $ getData uvMan
    case textToXMLDocument resp of
        Right doc -> formatUVRating (getUVRating s doc)
        Left _ -> getConfigValue naString

-- | XML Parsing code comes here.
-- This is a very simple XML parser to just deal with the uvvalues.xml
-- provided by ARPANSA. If you work on a new plugin which needs an XML
-- parser perhaps consider using a real XML parser and refactor this
-- plug-in to us it as well.
--
-- Note: This parser can not deal with short tags.
--
-- Kudos to: Charlie Harvey for his article about writing an XML Parser
-- with Parsec.
--

type AttrName  = String
type AttrValue = String

newtype Attribute = Attribute (AttrName, AttrValue)
    deriving (Show)

data XML = Element String [Attribute] [XML]
         | Decl String
         | Body String
    deriving (Show)

-- | parse the document
--
document :: Parser [XML]
document = do
    spaces
    y <- try xmlDecl <|> tag
    spaces
    x <- many tag
    spaces
    return (y : x)

-- | parse any tags
--
tag :: Parser XML
tag  = do
    char '<'
    spaces
    name <- many (letter <|> digit)
    spaces
    attr <- many attribute
    spaces
    string ">"
    eBody <- many elementBody
    endTag name
    spaces
    return (Element name attr eBody)

xmlDecl :: Parser XML
xmlDecl = do
    void $ manyTill anyToken (string "<?xml") -- ignore the byte order mark
    decl <- many (noneOf "?>")
    string "?>"
    return (Decl decl)

elementBody :: Parser XML
elementBody = spaces *> try tag <|> text

endTag :: String -> Parser String
endTag str = string "</" *> string str <* char '>'

text :: Parser XML
text = Body <$> many1 (noneOf "><")

attribute :: Parser Attribute
attribute = do
    name <- many (noneOf "= />")
    spaces
    char '='
    spaces
    char '"'
    value <- many (noneOf "\"")
    char '"'
    spaces
    return (Attribute (name, value))

-- | Possibly create a new 'Manager', based upon the users preference.  If one
-- is created, this 'Manager' will be used throughout the monitor.
tryMakeManager :: UVMeterOpts -> IO (Maybe Manager)
tryMakeManager opts =
    if useManager opts
        then Just <$> mkManager
        else pure Nothing

-- | Create a new 'Manager' for managing network connections.
mkManager :: IO Manager
mkManager = newManager $ tlsManagerSettings {managerConnCount = 1}
