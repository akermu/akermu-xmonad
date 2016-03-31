module XMonad.Prompt.AppPrompt (
  appPrompt
  ) where

import Data.List
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Common
import XMonad.Actions.SpawnOn  
import System.Directory
import System.FilePath
import Control.Monad


data App = App

instance XPrompt App where
  showXPrompt        App = "Launch: "
  nextCompletion       _ = getNextCompletion

appPrompt :: XPConfig -> X ()
appPrompt c = mkXPrompt App c appCompl startApp

appCompl :: ComplFunction
appCompl = makeCompletionFunc getApplicationList

getApplicationDirs :: IO [String]
getApplicationDirs = do
  homedir <- getHomeDirectory
  filterM doesDirectoryExist
    [homedir ++ "/.local/share/applications",
     "/usr/share/applications"]

getApplicationList :: IO [String]
getApplicationList = do
  dirs <- getApplicationDirs
  files <- forM dirs $ \name -> 
    filterDirRecursive (\file -> takeExtension file == ".desktop") name
  return $ sort $ nub $ takeBaseName <$> concat files

findApp :: String -> IO (Maybe String)
findApp app = do
  dirs <- getApplicationDirs
  possibleFiles <- filterM doesFileExist
                   [dir </> app <.> "desktop" | dir <- dirs]
  case possibleFiles of
    [] -> return Nothing
    _ -> return $ Just (head possibleFiles)

startApp :: String -> X ()
startApp input = do
  completions <- liftIO $ appCompl input
  unless (null completions) $ do
    let appName = head completions
    app <- liftIO (findApp appName)
    case app of
      Just fileName -> spawnHere ("dex " ++ fileName)
