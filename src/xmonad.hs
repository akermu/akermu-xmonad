import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Input
import XMonad.Prompt.AppPrompt
import XMonad.Prompt.PassPrompt
import XMonad.Prompt.VpnPrompt
import XMonad.Prompt.NetworkPrompt

import qualified Data.Map as M
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad
import Data.List
  
myWorkspaces :: [String]
myWorkspaces = ["1:code","2:term","3:web"] ++ fmap show [4..7] ++ ["8:vm","9:media"]

myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
               [ className =? "Emacs" --> viewShift "1:code"
               , title =? "tmux" --> viewShift "2:term"
               , className =? "Firefox" --> viewShift "3:web"
               , className =? "mpv" --> doFullFloat
               , className =? "mpv" --> viewShift "9:media"
               , className =? "Pinentry" --> doCenterFloat
               , isDialog --> doCenterFloat
               , isFullscreen --> doFullFloat
               , fmap not isDialog --> doF avoidMaster
               ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd          
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) ->  W.Stack t [r] rs
    _                   -> c
    
dmenuOptions :: String
dmenuOptions = " -i -t -fn 'xft:Inconsolata:size=16' -nb black -sb white -sf black   "

runDmenu :: String -> X ()
runDmenu program = spawnHere (program ++ dmenuOptions)

myPromptKeymap :: M.Map (KeyMask,KeySym) (XP ())
myPromptKeymap = M.union emacsLikeXPKeymap $ M.fromList
                 [ ((controlMask, xK_i) , moveCursor Next)
                 , ((controlMask, xK_m),  setSuccess True >> setDone True)
                 ]

findCompl :: String -> [FilePath] -> [String]
findCompl s = filter (\x -> isPrefixOf s x)
             
myPromptConfig :: XPConfig                   
myPromptConfig = defaultXPConfig { font = "xft:Inconsolata:size=16"
                                 -- , alwaysHighlight = True
                                 , position = Top
                                 , height = 28
                                 , promptKeymap = myPromptKeymap
                                 }

-- M-key = xmonad keys
-- M-S-key = Keys to start applications
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys c = mkKeymap c $
             [ ("M-S-<Return>", spawn $ terminal c)
             , ("M-<Space>", sendMessage NextLayout)
             , ("M-<Return>", windows W.swapMaster)
             , ("M-f", sendMessage ToggleStruts)
             , ("M-o", windows W.focusDown)
             , ("M-d", windows W.swapDown)
             , ("M-u", windows W.swapUp)
             , ("M-m", windows W.focusMaster)
             , ("M-e", sendMessage Expand)
             , ("M-s", sendMessage Shrink)
             , ("M-t", withFocused $ windows . W.sink)
             , ("M-k", kill)
             , ("M-r", toggleWS)
             , ("M-q", spawn "xmonad --recompile && xmonad --restart")
             , ("M-S-q", io exitSuccess)

               -- Prompts
             , ("M-g", windowPromptGoto myPromptConfig)
             , ("M-b", windowPromptBring myPromptConfig)

               -- Applications
             , ("M-p", appPrompt myPromptConfig)
             , ("M-i", spawnHere "(pgrep conky && pkill conky) || conky")
             , ("M-S-p", passPrompt myPromptConfig)
             -- , ("M-S-p" , runDmenu "passmenu --type")
             , ("M-S-w" , namedScratchpadAction scratchpads "wifi-menu")
             , ("M-S-m" , namedScratchpadAction scratchpads "music")
             , ("M-S-n" , networkPrompt myPromptConfig)
             , ("M-S-v" , vpnPrompt myPromptConfig)
             , ("M-S-l" , spawnHere "dm-tool lock")
             , ("M-S-t" , spawnHere (myTerminal ++ " -e tmux"))
             , ("M-S-<End>", runDmenu "dmenu_power")

               -- Media Keys
             , ("<XF86AudioPlay>", spawn "mpc toggle")
             , ("<XF86AudioNext>", spawn "mpc next")
             , ("<XF86AudioPrev>", spawn "mpc prev")
             , ("<XF86AudioRaiseVolume>", spawn "amixer set Master '2%+'")
             , ("<XF86AudioLowerVolume>", spawn "amixer set Master '2%-'")
             , ("<XF86AudioMute>", spawn "amixer set Master toggle")
             , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
             , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
             ]

            ++

            -- mod-[1..9], Switch to workspace N
            -- mod-shift-[1..9], Move client to workspace N
            [ (("M-" ++ m ++ k), windows $ f i)
              | (i, k) <- zip (XMonad.workspaces c) (fmap show [1 .. 9])
              , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]

            ++

            -- mod-{comma,dot,slash}, Switch to screens 1, 2, or 3
            -- mod-shift-{comma,dot,slash}, Move client to screen 1, 2, or 3
            [ (("M-" ++ m ++ key), screenWorkspace sc >>= flip whenJust (windows . f))
              | (key, sc) <- zip [",", "." , "/"] [0..]
              , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))]

emacsLayout :: Tall a
emacsLayout = Tall nmaster delta ratio
  where
    nmaster = 1
    ratio   = 70/100
    delta   = 3/100

-- myLayout ::
myLayout = onWorkspaces ["1:emacs"] (Full ||| emacsLayout) $
           onWorkspaces ["2:term", "3:web", "4:vm", "9:media"] (Full ||| tiled) $
           Full ||| tiled ||| simplestFloat
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "wifi-menu" (myTerminal ++ " -title network -e sudo wifi-menu") (title =? "network") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "music" (myTerminal ++ " -title music -e ncmpc") (title =? "music") (customFloating $ W.RationalRect (1/8) (1/16) (3/4) (15/16))
  ]

myTerminal :: String
myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 1

myModMask :: KeyMask
myModMask = mod4Mask

myPP :: PP
myPP = defaultPP { ppOrder = take 2
                 , ppSep = " | "
                 }

main :: IO ()
main = do
  h <- spawnPipe "xmobar"
  xmonad $ ewmh defaultConfig
             { terminal = myTerminal
             , focusFollowsMouse = myFocusFollowsMouse
             , borderWidth = myBorderWidth
             , modMask = myModMask
             , workspaces = myWorkspaces
             , keys = myKeys
             , mouseBindings = myMouseBindings
             , layoutHook = smartBorders $ avoidStruts myLayout
             , manageHook = myManageHook <+> manageSpawn <+> namedScratchpadManageHook scratchpads
             , startupHook = setWMName "LG3D"
             , logHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ myPP { ppOutput = hPutStrLn h}
             , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
             }