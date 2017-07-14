{-# LANGUAGE OverloadedStrings #-}
module Morpheus where


import           Network.HTTP.Simple (httpNoBody)
import           Data.Aeson
import           Network.HTTP.Client hiding (httpNoBody)
import           Network.HTTP.Types.Status (statusCode)
import           Data.Text as T
import           Data.Text.IO as T
import           Data.Monoid

data MorpheusConfig = MorpheusConfig { playerAddress :: Text
                                     , playerPort :: Int
                                     , playerDebug :: Bool
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
                               , jsonRPC :: String
                               , jsonParams :: Value
                               , jsonID :: Int
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
playerUrl (MorpheusConfig addr port _) = "http://" <> addr <> ":" <> (pack $ show port)

getUrl :: MorpheusConfig -> Command -> Text
getUrl c (Navigation cmd) = (playerUrl c) </> "navigation" </> (lowerFirst $ pack $ show cmd)
getUrl c (Playback cmd) = (playerUrl c) </> "playback" </> (lowerFirst $ pack $ show cmd)
getUrl c (JSONRPC rpc) = (playerUrl c) </> "jsonrpc"

runCommand :: MorpheusConfig -> Command -> IO Bool
runCommand c cmd@(JSONRPC rpc) = do
  let url = getUrl c cmd
  if playerDebug c then T.putStrLn $ "Running command with url: " <> url
                   else return ()
  initialRequest <- parseRequest $ unpack url
  let request = initialRequest { method = "POST"
                               , requestBody = RequestBodyLBS $ encode rpc
                               }
  response <- httpLbs request =<< newManager defaultManagerSettings
  return $ 200 == (statusCode $ responseStatus response)
runCommand c cmd = do
  let url = getUrl c cmd
  if playerDebug c then T.putStrLn $ "Running command with url: " <> url
                   else return ()
  response <- httpNoBody =<< (parseRequest $ unpack url)
  return $ 200 == (statusCode $ responseStatus response)

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

custom c meth par = runCommand c (JSONRPC (JSONRequest { jsonMethod = meth
                                                       , jsonRPC = "2.0"
                                                       , jsonParams = par
                                                       , jsonID = 1
                                                       }))

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


