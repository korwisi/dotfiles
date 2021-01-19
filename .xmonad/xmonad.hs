import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

main = do
  xmprocs <- spawnPipe ("~/.cabal/bin/xmobar")
  xmonad =<< xmobar myConfig

myConfig = def {
  borderWidth = 2,
  manageHook = myManageHook,
  terminal = "alacritty",
  normalBorderColor = "#cccccc",
  focusedBorderColor = "#ffad00",
  modMask = mod4Mask } `additionalKeysP`
  [ ("M-l", spawn "slock")
  , ("M-e", spawn "emacs")
  , ("M-<KP_Add>", spawn "pactl -- set-sink-volume 1 +5%")
  , ("M-<KP_Subtract>", spawn "pactl -- set-sink-volume 1 -5%")
  , ("M-<KP_Multiply>", spawn "pactl -- set-sink-mute 1 toggle")
  ]

myManageHook = composeAll
   [ className =? "Firefox" <&&> resource =? "Dialog"      --> doFloat,
     className =? "Firefox" <&&> resource =? "Extension"   --> doFloat,
     className =? "Firefox" <&&> resource =? "Browser"     --> doFloat,
     className =? "Firefox" <&&> resource =? "Download"    --> doFloat
   ]
