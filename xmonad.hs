import Data.Monoid (All (All))
import qualified Data.Monoid
import Network.HostName
import XMonad
import qualified XMonad as XMonad.Operations
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Prompt.Window (WindowPrompt (Goto), windowPrompt, wsWindows)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "kitty"

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "htop" "kitty --name=htop -e htop" (appName =? "htop") doCenterFloat,
    NS
      "telegram"
      "telegram-desktop"
      (appName =? "telegram-desktop")
      defaultFloating
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-p", spawn "rofi -show"),
    ("M-S-r", spawn "xmonad --restart"),
    ("M-s h", namedScratchpadAction myScratchpads "htop"),
    ("M-s t", namedScratchpadAction myScratchpads "telegram"),
    ("M-b", spawn "google-chrome-stable"),
    ("M-S-b", spawn "google-chrome-stable --incognito"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -10"),
    ("<XF86MonBrightnessUp>", spawn "xbacklight +10"),
    ("M-S-p", windowPrompt def Goto wsWindows)
  ]

defaultLayouts =
  lessBorders
    Screen
    ( avoidStruts
        ( smartSpacingWithEdge
            8
            ( centeredIfSingle
                1
                -- ThreeColMid layout puts the large master window in the center
                -- of the screen. As configured below, by default it takes of 3/4 of
                -- the available space. Remaining windows tile to both the left and
                -- right of the master window. You can resize using "super-h" and
                -- "super-l".
                ( ThreeColMid 1 (3 / 100) (3 / 7)
                    -- ResizableTall layout has a large master window on the left,
                    -- and remaining windows tile on the right. By default each area
                    -- takes up half the screen, but you can resize using "super-h" and
                    -- "super-l".
                    ||| ResizableTall 1 (3 / 100) (1 / 2) []
                    -- Mirrored variation of ResizableTall. In this layout, the large
                    -- master window is at the top, and remaining windows tile at the
                    -- bottom of the screen. Can be resized as described above.
                    ||| Mirror (ResizableTall 1 (3 / 100) (1 / 2) [])
                    -- Full layout makes every window full screen. When you toggle the
                    -- active window, it will bring the active window to the front.
                    ||| noBorders Full
                    -- Circle layout places the master window in the center of the screen.
                    -- Remaining windows appear in a circle around it
                    ||| Circle
                    -- Grid layout tries to equally distribute windows in the available
                    -- space, increasing the number of columns and rows as necessary.
                    -- Master window is at top left.
                    ||| Grid
                )
            )
        )
    )

myManageHooks :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHooks =
  composeAll
    [ insertPosition Below Newer,
      className =? "dialog" --> doFloat,
      resource =? "dialog" --> doCenterFloat
    ]
    <+> namedScratchpadManageHook myScratchpads
    <+> manageDocks

restartEventHook :: Event -> X All
restartEventHook ClientMessageEvent {ev_message_type = mt} = do
  a <- getAtom "XMONAD_RESTART"
  if mt == a
    then XMonad.Operations.restart "xmonad-kid" True >> return (All True)
    else return $ All True
restartEventHook _ = return $ All True

-- taffybar = statusBarGeneric "taffybar-kid" mempty
-- xmobar0 = statusBarPropTo "_XMONAD_LOG_0" "xmobar -x 0 $HOME/.config/xmobar/gruvbox-dark.xmobarrc" (pure xmobarPP)
-- xmobar1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 1 $HOME/.config/xmobar/gruvbox-dark.xmobarrc" (pure xmobarPP)
-- xmobar0 = statusBarPropTo "_XMONAD_LOG_0" "xmobar-kid top" (pure xmobarPP)
-- xmobar1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar-kid top" (pure xmobarPP)

polybarSpawner :: String -> ScreenId -> IO StatusBarConfig
polybarSpawner h n = pure $ polybar h n

polybar :: String -> ScreenId -> StatusBarConfig
polybar h (S n) = statusBarGeneric ("polybar-xmonad \"" ++ h ++ show n ++ "\"") mempty

xmobar :: ScreenId -> StatusBarConfig
xmobar (S n) = statusBarPropTo ("_XMONAD_LOG_" ++ show n) ("xmobar -x " ++ show n ++ " $HOME/.config/xmobar/gruvbox-dark.xmobarrc") (pure xmobarPP)

xmobarSpawner :: ScreenId -> IO StatusBarConfig
xmobarSpawner n = pure $ xmobar n

-- TODO look into https://github.com/disconsis/literate-xmonad-config/blob/master/src/config.org#dynamic-bar-highlighting-and-management

main :: IO ()
main = do
  hostName <- getHostName
  -- xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/gruvbox-dark.xmobarrc"
  -- spawn "taffybar-kid"
  dirs <- getDirectories
  (`launch` dirs)
    . dynamicSBs (polybarSpawner hostName)
    -- . dynamicSBs xmobarSpawner
    . docks
    . ewmh
    $ ewmhFullscreen
      def
        { manageHook = myManageHooks,
          modMask = myModMask,
          terminal = myTerminal,
          layoutHook = defaultLayouts,
          handleEventHook = restartEventHook,
          borderWidth = 2,
          normalBorderColor = "#282828",
          focusedBorderColor = "#8ec07c"
        }
      `additionalKeysP` myKeys
