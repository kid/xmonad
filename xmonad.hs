{-# OPTIONS_GHC -Wno-missing-signatures #-}

import qualified DBus.Client
import Data.Monoid
import Network.HostName (getHostName)
import XMonad
import qualified XMonad as XMonad.Operations
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Config.Desktop
import qualified XMonad.DBus as DC
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import qualified XMonad.StackSet as W hiding (filter)
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad hiding (cmd)

myModMask :: KeyMask
myModMask = mod4Mask

myFont :: String
myFont = "xft:FiraCode Nerd Font:regular:size:11:antialias=true:hinting=true"

myBorderWidth :: Dimension
myBorderWidth = 2

mySpacing :: Int
mySpacing = 8

myTerminal :: String
myTerminal = "kitty"

foreground :: String
foreground = "#ebdbb2"

background :: String
background = "#282828"

gray :: String
gray = "#a89984"

aqua :: String
aqua = "#8ec07c"

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "htop" "kitty --name=htop -e htop" (appName =? "htop") doCenterFloat
  , NS
      "telegram"
      "telegram-desktop"
      (appName =? "telegram-desktop")
      defaultFloating
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-p", spawn "rofi -show")
  , ("M-S-r", spawn "xmonad --restart")
  , ("M-s h", namedScratchpadAction myScratchpads "htop")
  , ("M-s t", namedScratchpadAction myScratchpads "telegram")
  , -- FIXME this is hiding desktopConfig's toggle struts binding
    ("M-b", spawn "google-chrome-beta")
  , ("M-S-b", spawn "google-chrome-beta --incognito")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight +10")
  , ("M-S-<Tab>", rotSlavesDown)
  , ("M-C-<Tab>", rotAllDown)
  , ("M-<Space>", sendMessage (T.Toggle "Full"))
  , ("M-\\", sendMessage NextLayout)
  , ("M-S-\\", sendMessage FirstLayout)
  , --
    -- KB_GROUP Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
    ("M-C-h", sendMessage $ pullGroup L)
  , ("M-C-l", sendMessage $ pullGroup R)
  , ("M-C-k", sendMessage $ pullGroup U)
  , ("M-C-j", sendMessage $ pullGroup D)
  , ("M-C-m", withFocused (sendMessage . MergeAll))
  , ("M-C-S-m", withFocused (sendMessage . UnMergeAll))
  , -- KB_GROUP Increase/decrease windows in the master pane or the stack
    ("M-S-]", sendMessage (IncMasterN 1)) -- Increase # of clients master pane
  , ("M-S-[", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
  , ("M-C-<Up>", increaseLimit) -- Increase # of windows
  , ("M-C-<Down>", decreaseLimit) -- Decrease # of windows
  , -- , ("M-C-u", withFocused (sendMessage . UnMerge))
    ("M-C-.", onGroup W.focusUp') -- Switch focus to next tab
  , ("M-C-,", onGroup W.focusDown') -- Switch focus to prev tab
  ]

layouts = T.toggleLayouts fullscreen tiled
 where
  fullscreen = noBorders Full
  tiled = ifWider (1440 + 1) (ifWider (1920 + 1) ultraWideScreen laptopScreen) verticalScreen
  ultraWideScreen = threeCol ||| threeColMid
  verticalScreen = grid
  laptopScreen = tall ||| threeCol

tall =
  renamed [Replace "tall"]
    . smartBorders
    . smartSpacing mySpacing
    . addTabs shrinkText myTabTheme
    . subLayout [] Simplest
    . limitWindows 12
    $ ResizableTall 1 (3 / 100) (1 / 2) []

grid =
  renamed [Replace "grid"]
    . smartBorders
    . smartSpacing mySpacing
    . addTabs shrinkText myTabTheme
    . subLayout [] Simplest
    . limitWindows 12
    . mkToggle (single MIRROR)
    $ Grid (16 / 10)

threeColMid =
  renamed [Replace "threeColMid"]
    . smartBorders
    . smartSpacing mySpacing
    . addTabs shrinkText myTabTheme
    . subLayout [] Simplest
    . limitWindows 7
    $ ThreeColMid 1 (3 / 100) (3 / 7)

threeCol =
  renamed [Replace "threeCol"]
    . smartBorders
    . smartSpacing mySpacing
    . addTabs shrinkText myTabTheme
    . subLayout [] Simplest
    . limitWindows 7
    $ ThreeCol 1 (3 / 100) (3 / 7)

myTabTheme :: Theme
myTabTheme =
  def
    { fontName = myFont
    , activeColor = aqua
    , inactiveColor = background
    , activeBorderColor = aqua
    , inactiveBorderColor = background
    , activeTextColor = background
    , inactiveTextColor = foreground
    }

myManageHooks :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHooks =
  composeAll
    [ insertPosition Below Newer
    , className =? "dialog" --> doFloat
    , resource =? "dialog" --> doCenterFloat
    ]
    <+> namedScratchpadManageHook myScratchpads
    <+> manageDocks

restartEventHook :: Event -> X All
restartEventHook ClientMessageEvent{ev_message_type = mt} = do
  a <- getAtom "XMONAD_RESTART"
  if mt == a
    then XMonad.Operations.restart "xmonad-kid" True >> return (All True)
    else return $ All True
restartEventHook _ = return $ All True

myLogHook :: ScreenId -> DBus.Client.Client -> PP
myLogHook s@(S n) dbus =
  def
    { ppOutput = DC.sendToPath dbus (show n)
    , ppOrder = \(_ : _ : _ : extras) -> extras
    , ppSep = "  "
    , ppExtras =
        [ wrapL ("%{F" ++ gray ++ "} ") " %{F-}" $ logLayoutOnScreen s
        , logTitleOnScreen s
        ]
    }

polybarSpawner :: DBus.Client.Client -> String -> ScreenId -> StatusBarConfig
polybarSpawner dbus hostname s@(S i) =
  def
    { sbLogHook = dynamicLogWithPP (myLogHook s dbus)
    , sbStartupHook = spawnStatusBar cmd
    , sbCleanupHook = killStatusBar cmd
    }
 where
  cmd = "polybar-xmonad " ++ hostname ++ show i

myHandleEventHook =
  mconcat
    [ restartEventHook
    , Hacks.windowedFullscreenFixEventHook -- FIXME Does not seems to work?
    ]

myConfig =
  desktopConfig
    { manageHook = myManageHooks <+> manageHook desktopConfig
    , handleEventHook = myHandleEventHook <+> handleEventHook desktopConfig
    , layoutHook = desktopLayoutModifiers layouts
    , modMask = myModMask
    , terminal = myTerminal
    , focusFollowsMouse = False
    , borderWidth = myBorderWidth
    , normalBorderColor = background
    , focusedBorderColor = aqua
    }
    `additionalKeysP` myKeys

main :: IO ()
main = do
  dbus <- DC.connect
  _ <- DC.requestAccess dbus
  hostName <- getHostName
  dirs <- getDirectories
  (`launch` dirs)
    . dynamicSBs (pure . polybarSpawner dbus hostName)
    . Hacks.javaHack
    $ ewmhFullscreen
      myConfig
