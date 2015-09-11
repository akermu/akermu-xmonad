module XMonad.Prompt.VpnPrompt (
  vpnPrompt
  ) where

import Data.List
import XMonad
import XMonad.Prompt hiding ( pasteString )
import XMonad.Prompt.Common
import System.FilePath
import System.Directory
import Control.Monad

data Vpn = Vpn
data StopVpn = StopVpn

instance XPrompt Vpn where
  showXPrompt       Vpn = "Vpn: "
  nextCompletion       _ = getNextCompletion

instance XPrompt StopVpn where
  showXPrompt    StopVpn = "Stop Vpn: "
  nextCompletion       _ = getNextCompletion

vpnPrompt :: XPConfig -> X ()
vpnPrompt c = do
  running <- liftIO isVpnRunning
  if running
     then mkXPrompt StopVpn c (mkComplFunFromList ["y", "yes", "n", "no"]) stopVpn        else mkXPrompt Vpn c vpnCompl startVpn

vpnDir :: FilePath
vpnDir = "/etc/openvpn/"

vpnCompl :: ComplFunction
vpnCompl = makeCompletionFunc getVpnList

getVpnList :: IO [String]
getVpnList = do
  contents <- getDirectoryContents vpnDir
  let files = filter (\file -> takeExtension file == ".conf") contents
  filterVpnList files

filterVpnList :: [FilePath] -> IO [String]
filterVpnList files = return $ fmap dropExtension files
  
startVpn :: String -> X ()
startVpn input = do
    completions <- liftIO $ vpnCompl input
    unless (null completions) $ do
      let vpnFile = head completions
      liftIO $ spawn $
        "sudo systemctl start openvpn@" ++ vpnFile ++ ".service"

stopVpn :: String -> X ()
stopVpn s = if s `elem` ["y", "yes"]
              then liftIO $ do
                vpn <- getRunningVpn
                case vpn of
                  Just s -> spawn $ "sudo systemctl stop openvpn@" ++ s ++ ".service"
              else spawn "notify-send -u critical Not found"

isVpnRunning :: IO Bool
isVpnRunning = do
  vpn <- getRunningVpn
  case vpn of
    Just _ -> return True
    Nothing -> return False

getRunningVpn :: IO (Maybe String)  
getRunningVpn = do
  names <- getDirectoryContents "/run"
  let xs = filter (\name -> isPrefixOf "openvpn@" name &&
                            isSuffixOf ".pid" name) names
  if length xs == 1
    then return $ stripPrefix "openvpn@" $ takeBaseName $ head xs
    else return Nothing

