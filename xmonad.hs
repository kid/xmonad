import qualified DBus.Client
import Data.Monoid
import Network.HostName (getHostName)
import XMonad
import qualified XMonad as XMonad.Operations
import XMonad.Actions.MouseResize
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import qualified XMonad.DBus as DC
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts)
import XMonad.Layout.WindowArranger (windowArrange)
import qualified XMonad.StackSet as W hiding (filter)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad hiding (cmd)

myModMask :: KeyMask
myModMask = mod4Mask

myFont :: String
myFont = "xft:FiraCode Nerd Font:regular:size:11:antialias=true:hinting=true"

myFont' :: String
myFont' = "xft:FiraCode Nerd Font:bold:size:24:antialias=true:hinting=true"

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

green :: String
green = "#8ec07c"

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
  , ("M-b", spawn "google-chrome-beta")
  , ("M-S-b", spawn "google-chrome-beta --incognito")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight +10")
  , ("M-S-<Tab>", rotSlavesDown)
  , ("M-C-<Tab>", rotAllDown)
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

-- defaultLayouts =
--   lessBorders
--     Screen
--     ( avoidStruts
--         ( smartSpacingWithEdge
--             8
--             -- ThreeColMid layout puts the large master window in the center
--             -- of the screen. As configured below, by default it takes of 3/4 of
--             -- the available space. Remaining windows tile to both the left and
--             -- right of the master window. You can resize using "super-h" and
--             -- "super-l".
--             ( ThreeColMid 1 (3 / 100) (3 / 7)
--                 -- ResizableTall layout has a large master window on the left,
--                 -- and remaining windows tile on the right. By default each area
--                 -- takes up half the screen, but you can resize using "super-h" and
--                 -- "super-l".
--                 ||| ResizableTall 1 (3 / 100) (1 / 2) []
--                 -- Mirrored variation of ResizableTall. In this layout, the large
--                 -- master window is at the top, and remaining windows tile at the
--                 -- bottom of the screen. Can be resized as described above.
--                 ||| Mirror (ResizableTall 1 (3 / 100) (1 / 2) [])
--                 -- Full layout makes every window full screen. When you toggle the
--                 -- active window, it will bring the active window to the front.
--                 ||| noBorders Full
--             )
--         )
--     )
--
-- The layout hook
myLayoutHook =
  avoidStruts $
    mouseResize $
      windowArrange $
        T.toggleLayouts floats $
          mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
 where
  myDefaultLayout =
    withBorder
      myBorderWidth
      threeColMid
      ||| threeCol
      ||| tall
      ||| grid

tall =
  renamed [Replace "tall"]
    . smartBorders
    .
    -- windowNavigation .
    addTabs shrinkText myTabTheme
    . subLayout [] (smartBorders Simplest)
    . limitWindows 12
    . smartSpacing mySpacing
    $ ResizableTall 1 (3 / 100) (1 / 2) []

-- magnify =
--   renamed [Replace "magnify"] $
--     smartBorders $
--       -- windowNavigation $
--       addTabs shrinkText myTabTheme $
--         subLayout [] (smartBorders Simplest) $
--           magnifier $
--             limitWindows 12 $
--               smartSpacing mySpacing $
--                 ResizableTall 1 (3 / 100) (1 / 2) []

-- monocle =
--   renamed [Replace "monocle"] $
--     smartBorders $
--       -- windowNavigation $
--       addTabs shrinkText myTabTheme $
--         subLayout [] Simplest $
--           limitWindows 20 Full

floats =
  renamed [Replace "floats"] $
    smartBorders $
      limitWindows 20 simplestFloat

grid =
  renamed [Replace "grid"]
    . smartBorders
    . smartSpacing mySpacing
    -- windowNavigation $
    . addTabs shrinkText myTabTheme
    . subLayout [] (smartBorders Simplest)
    . limitWindows 12
    . mkToggle (single MIRROR)
    $ Grid (16 / 10)

-- spirals =
--   renamed [Replace "spirals"]
--     . smartBorders
--     . smartSpacing mySpacing
--     -- windowNavigation .
--     . addTabs shrinkText myTabTheme
--     . subLayout [] Simplest
--     $ spiral (6 / 7)

threeColMid =
  renamed [Replace "threeColMid"]
    . smartSpacing mySpacing
    . smartBorders
    -- . windowNavigation
    . addTabs shrinkText myTabTheme
    . subLayout [] Simplest
    . limitWindows 7
    $ ThreeColMid 1 (3 / 100) (3 / 7)

threeCol =
  renamed [Replace "threeCol"]
    . smartSpacing mySpacing
    . smartBorders
    -- . windowNavigation
    . addTabs shrinkText myTabTheme
    . subLayout [] Simplest
    . limitWindows 7
    $ ThreeCol 1 (3 / 100) (3 / 7)

-- threeRow =
--   renamed [Replace "threeRow"] $
--     smartSpacing mySpacing $
--       smartBorders $
--         -- windowNavigation $
--         addTabs shrinkText myTabTheme $
--           subLayout [] (smartBorders Simplest) $
--             limitWindows 7
--             -- Mirror takes a layout and rotates it by 90 degrees.
--             -- So we are applying Mirror to the ThreeCol layout.
--             $
--               Mirror $
--                 ThreeCol 1 (3 / 100) (1 / 2)
--
-- tabs =
--   renamed [Replace "tabs"]
--   -- I cannot add spacing to this layout because it will
--   -- add spacing between window and tabs which looks bad.
--   $ tabbed shrinkText myTabTheme
--
tallAccordion = renamed [Replace "tallAccordion"] Accordion

wideAccordion =
  renamed [Replace "wideAccordion"] $
    Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme =
  def
    { fontName = myFont
    , activeColor = green
    , inactiveColor = background
    , activeBorderColor = green
    , inactiveBorderColor = background
    , activeTextColor = background
    , inactiveTextColor = foreground
    }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def
    { swn_font = myFont'
    , swn_fade = 1.0
    , swn_bgcolor = "#1d2021"
    , swn_color = foreground
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
    , ppExtras =
        [ logLayoutOnScreen s
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
  cmd = "polybar --config=~/Code/kid/xmonad/polybar.ini --reload " ++ hostname ++ show i

-- TODO look into https://github.com/disconsis/literate-xmonad-config/blob/master/src/config.org#dynamic-bar-highlighting-and-management

main :: IO ()
main = do
  dbus <- DC.connect
  DC.requestAccess dbus
  hostName <- getHostName
  dirs <- getDirectories
  (`launch` dirs)
    . dynamicSBs (pure . polybarSpawner dbus hostName)
    . docks
    . ewmh
    $ ewmhFullscreen
      def
        { manageHook = myManageHooks
        , modMask = myModMask
        , terminal = myTerminal
        , focusFollowsMouse = False
        , layoutHook = showWName' myShowWNameTheme myLayoutHook
        , handleEventHook = restartEventHook
        , borderWidth = myBorderWidth
        , normalBorderColor = background
        , focusedBorderColor = green
        }
      `additionalKeysP` myKeys
