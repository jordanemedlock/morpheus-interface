{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Morpheus where


import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy         as H
import           Data.Monoid
import           Data.String
import           Data.Text                 as T
import           Data.Text.IO              as T
import           Network.HTTP.Client       hiding (httpNoBody)
import           Network.HTTP.Simple       (getResponseBody, httpJSON,
                                            httpNoBody)
import           Network.HTTP.Types.Status (statusCode)

data MorpheusConfig = MorpheusConfig { playerAddress :: Text
                                     , playerPort    :: Int
                                     , playerDebug   :: Bool
                                     } deriving Show

defaultConfig = MorpheusConfig "localhost" 3005 True

data Command  = Navigation { navigationCommand :: NavigationCommand }
              | JSONRPC { jsonRequest :: JSONRequest }
              | Playback { playbackCommand :: PlaybackCommand }
              deriving Show

data NavigationCommand = MoveRight | MoveLeft | MoveDown | MoveUp | Select | Back
                       | Home | NextLetter | PreviousLetter | ToggleOSD
                       deriving Show

data PlaybackCommand = Pause | Play | SkipNext | SkipPrevious | Stop | StepBack
                     | StepForward
                     deriving Show

data JSONRequest = JSONRequest { jsonMethod :: String
                               , jsonRPC    :: String
                               , jsonParams :: Value
                               , jsonID     :: Int
                               } deriving Show

instance ToJSON JSONRequest where
  toJSON (JSONRequest method rpc params id) =
    object [ "method" .= method
           , "jsonrpc" .= rpc
           , "params" .= params
           , "id" .= id
           ]

lowerFirst :: Text -> Text
lowerFirst xs = h <> t
  where h = T.toLower $ T.head xs `cons` T.empty
        t = T.tail xs

(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b


playerUrl :: MorpheusConfig -> Text
playerUrl (MorpheusConfig addr port _) = "http://" <> addr <> ":" <> pack (show port)

getUrl :: MorpheusConfig -> Command -> Text
getUrl c (Navigation cmd) = playerUrl c </> "player" </> "navigation" </> lowerFirst (pack $ show cmd)
getUrl c (Playback cmd) = playerUrl c </> "player" </> "playback" </> lowerFirst (pack $ show cmd)
getUrl c (JSONRPC rpc) = playerUrl c </> "jsonrpc"

getRequestBody :: Command -> RequestBody
getRequestBody (JSONRPC rpc) = RequestBodyLBS $ encode rpc
getRequestBody _             = ""

getRequestMethod :: (IsString a) => Command -> a
getRequestMethod (JSONRPC _) = "POST"
getRequestMethod _           = "POST"

runCommand :: MorpheusConfig -> Command -> IO Bool
runCommand c cmd = catch (callMorpheus c cmd) (if playerDebug c then handle else const (return False))
  where handle (ex :: SomeException) = T.putStrLn ("Morpheus not connected at url: " <> getUrl c cmd <> " with exception: " <> pack (show ex)) >> return False


callMorpheus :: MorpheusConfig -> Command -> IO Bool
callMorpheus c cmd = handle (\(x :: SomeException) -> return False) $ do
  let url = getUrl c cmd
  when (playerDebug c) $ T.putStrLn ("Running command with url: " <> url)
  initialRequest <- parseRequest $ unpack url
  let request = initialRequest { method = "POST"
                               , requestBody = getRequestBody cmd
                               }
  response <- httpLbs request =<< newManager defaultManagerSettings
  when (playerDebug c) $ print response
  return $ 200 == statusCode (responseStatus response)

moveRight c = runCommand c (Navigation MoveRight)
moveLeft c  = runCommand c (Navigation MoveLeft)
moveDown c  = runCommand c (Navigation MoveDown)
moveUp c    = runCommand c (Navigation MoveUp)
select c    = runCommand c (Navigation Select)
back c      = runCommand c (Navigation Back)
home c      = runCommand c (Navigation Home)
nextLetter c = runCommand c (Navigation NextLetter)
previousLetter c = runCommand c (Navigation PreviousLetter)
toggleOSD c = runCommand c (Navigation ToggleOSD)

pause c         = runCommand c (Playback Pause)
play c          = runCommand c (Playback Play)
skipNext c      = runCommand c (Playback SkipNext)
skipPrevious c  = runCommand c (Playback SkipPrevious)
stop c          = runCommand c (Playback Stop)
stepBack c      = runCommand c (Playback StepBack)
stepForward c   = runCommand c (Playback StepForward)

custom c meth par = runCommand c (JSONRPC JSONRequest { jsonMethod = meth
                                                      , jsonRPC = "2.0"
                                                      , jsonParams = par
                                                      , jsonID = 1
                                                      })

contextMenu c = custom c "Input.ExecuteAction" $ object [("action", "contextmenu")]
fullscreen c = custom c "Input.ExecuteAction" $ object [("action", "fullscreen")]

settings c = custom c "GUI.ActivateWindow" $ object [("window", "settings")]

fastForward c   = custom c "Input.ExecuteAction" $ object [("action", "fastforward")]
rewind c        = custom c "Input.ExecuteAction" $ object [("action", "rewind")]
nextSubtitle c  = custom c "Input.ExecuteAction" $ object [("action", "nextsubtitle")]
showSubtitles c = custom c "Input.ExecuteAction" $ object [("action", "showsubtitles")]
audioNextLanguage c = custom c "Input.ExecuteAction" $ object [("action", "audionextlanguage")]

mute c = custom c "Application.SetMute" $ object [("mute", "toggle")]
volumeUp c = custom c "Application.SetVolume" $ object [("volume", "increment")]
volumeDown c = custom c "Application.SetVolume" $ object [("volume", "decrement")]

aspectRatio c = custom c "Input.ExecuteAction" $ object [("action", "aspectratio")]
info c = custom c "Input.ExecuteAction" $ object [("action", "info")]

showNotification :: MorpheusConfig -> String -> String -> Int -> IO Bool
showNotification c title message displayTime =
  custom c "GUI.ShowNotification" $ object [ "title" .= title
                                           , "message" .= message
                                           , "displaytime" .= displayTime
                                           ]

getJSONResult :: (FromJSON a) => MorpheusConfig -> Command -> IO a
getJSONResult c cmd = do
  let url = getUrl c cmd
  when (playerDebug c) $ T.putStrLn ("Running command with url: " <> url)
  initialRequest <- parseRequest $ unpack url
  let request = initialRequest { method = "POST"
                               , requestBody = getRequestBody cmd
                               }
  response <- httpJSON request
  return $ getResponseBody response

getRPCConfiguration :: MorpheusConfig -> IO Object
getRPCConfiguration c = do
  let command = JSONRPC   JSONRequest { jsonMethod = "JSONRPC.GetConfiguration"
                                      , jsonParams = object []
                                      , jsonRPC = "2.0"
                                      , jsonID = 1
                                      }
  getJSONResult c command


getAppProperties :: MorpheusConfig -> [String] -> IO (Maybe Object)
getAppProperties c props = do
  let command = JSONRPC   JSONRequest { jsonMethod = "Application.GetProperties"
                                      , jsonParams = object ["properties" .= toJSON props]
                                      , jsonRPC = "2.0"
                                      , jsonID = 1
                                      }
  obj <- getJSONResult c command
  let res = parseMaybe (.: "result") obj
  return res

getVolume :: MorpheusConfig -> IO (Maybe Double)
getVolume c = do
  res <- getAppProperties c ["volume"]
  return $ parseMaybe (.: "volume") =<< res
