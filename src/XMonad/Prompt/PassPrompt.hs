module XMonad.Prompt.PassPrompt (
  passPrompt
  ) where

import XMonad
import XMonad.Prompt hiding ( pasteString )
import XMonad.Prompt.Common
import System.Directory
import System.FilePath
import Control.Monad


data Pass = Pass

instance XPrompt Pass where
  showXPrompt       Pass = "Type: "
  nextCompletion       _ = getNextCompletion

passPrompt :: XPConfig -> X ()
passPrompt c = mkXPrompt Pass c passCompl typePass

getPassStore :: IO String
getPassStore = do
  homedir <- getHomeDirectory
  return $ homedir </> ".password-store/"

passCompl :: ComplFunction
passCompl = makeCompletionFunc getPassList

getPassList :: IO [String]
getPassList = do
  store <- getPassStore
  files <- filterDirRecursive (\file -> takeExtension file == ".gpg") store
  filterPassList files

filterPassList :: [FilePath] -> IO [String]
filterPassList files = do
  store <- getPassStore
  return $ fmap (dropExtension <$> makeRelative store) files
  
typePass :: String -> X ()
typePass input = do
  completions <- liftIO $ passCompl input
  unless (null completions) $ do
    let passFile = head completions
    spawn $ "/usr/bin/pass -c " ++ passFile
    spawn "notify-send 'On Clipboard'" 
