Config { font = "xft:Inconsolata:bold:size=16"
       , additionalFonts = []
       , borderColor = "grey"
       , border = BottomB
       , bgColor = "black"
       , fgColor = "white"
       , alpha = 255
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "/usr/local/share/icons/xpm"
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run StdinReader
         , Run Network "enp0s25" ["-t", "<txbar><icon=transfer.xpm/>", "-W", "0"] 10
         , Run Network "wlp3s0" ["-t", "<txbar><icon=wifi.xpm/>", "-W", "0"] 10
         , Run BatteryP ["BAT0", "BAT1"] ["-t", "<icon=battery-full.xpm/> <left>"] 60
         , Run Brightness ["-t", "<icon=lightbulb.xpm/> <percent>", "--", "-D", "intel_backlight"] 10
         , Run Com "ponymix" ["get-volume"] "volume" 10
         , Run Com "bash" ["-c", "emacsclient -e \"(progn (require 'org-clock) (if (org-clocking-p) (substring-no-properties (org-clock-get-clock-string)) \\\"\\\"))\" | tr -d '\"'"] "task" 60
         , Run Com "bash" ["-c", "if [ -e /run/openvpn-client@*.pid ]; then echo '<icon=lock-locked.xpm/>'; else echo '<icon=lock-unlocked.xpm/>'; fi"] "vpn" 60
         , Run Date "%a %d.%m.%y %H:%M" "date" 10
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% } %date% { %task% %enp0s25% %wlp3s0% %vpn% <icon=volume-high.xpm/> %volume% %bright% %battery%"
       }
