import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)

main = do
  xmprocs <- spawnPipe ("~/.cabal/bin/xmobar")
  xmonad =<< xmobar def {
    borderWidth = 2,
    manageHook = myManageHook,
    terminal = "alacritty",
    normalBorderColor = "#cccccc",
    focusedBorderColor = "#ffad00",
    modMask = mod4Mask }

myManageHook = composeAll
   [ className =? "Firefox" <&&> resource =? "Dialog"      --> doFloat,
     className =? "Firefox" <&&> resource =? "Extension"   --> doFloat,
     className =? "Firefox" <&&> resource =? "Browser"     --> doFloat,
     className =? "Firefox" <&&> resource =? "Download"    --> doFloat
   ]
