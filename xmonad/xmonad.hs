--------------------------------------------------------------------------------
import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig

--------------------------------------------------------------------------------
main = do
  --spawn "xmobar" -- Start a task bar such as xmobar.

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad $ desktopConfig
    { terminal   = "xterm"
    , manageHook = myManageHook <+> manageHook desktopConfig
    , logHook    = dynamicLogString def >>= xmonadPropLog
    }

    `additionalKeysP` -- Add some extra key bindings:
      [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-q",   spawn "/home/strats/.local/bin/xmonad --recompile && /home/strats/.local/bin/xmonad --restart")
      , ("M-p",   shellPrompt myXPConfig)
      , ("M-a",   spawn "gnome-terminal")
      ]
    `removeKeys` -- remove tab keys, I want them available to switch back to host windows
      [ (modMask desktopConfig,               xK_Tab)
      , (modMask desktopConfig .|. shiftMask, xK_Tab)]

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeOne
  [ className =? "Pidgin" -?> doFloat
  , className =? "XCalc"  -?> doFloat
  , className =? "mpv"    -?> doFloat
  , isDialog              -?> doCenterFloat

    -- Move transient windows to their parent:
  , transience
  ]
