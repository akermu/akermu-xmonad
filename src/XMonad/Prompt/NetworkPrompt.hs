module XMonad.Prompt.NetworkPrompt (
  networkPrompt
  ) where

import XMonad
import XMonad.Prompt hiding ( pasteString )
import XMonad.Prompt.Common
import System.FilePath
import System.Directory
import Control.Monad


data Network = Network

instance XPrompt Network where
  showXPrompt       Network = "Network: "
  nextCompletion       _ = getNextCompletion

networkPrompt :: XPConfig -> X ()
networkPrompt c = mkXPrompt Network c networkCompl startNetwork

networkDir :: FilePath
networkDir = "/etc/netctl/"

networkCompl :: ComplFunction
networkCompl = makeCompletionFunc getNetworkList

getNetworkList :: IO [String]
getNetworkList = do
  names <- getDirectoryContents networkDir
  files <- filterM (\name -> doesFileExist $ networkDir </> name) names
  filterNetworkList files

filterNetworkList :: [FilePath] -> IO [String]
filterNetworkList files = return $ fmap takeBaseName files
  
startNetwork :: String -> X ()
startNetwork input = do
  completions <- liftIO $ networkCompl input
  unless (null completions) $ do
    let networkFile = head completions
    liftIO $ spawn $ "sudo netctl switch-to " ++ networkFile
