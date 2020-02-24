-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Locks
-- Copyright   :  (c) Patrick Chilton
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Patrick Chilton <chpatrick@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin that displays the status of the lock keys.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Locks(Locks(..)) where

import Graphics.X11
import Data.List
import Data.Bits
import Control.Monad
import Graphics.X11.Xlib.Extras
import System.Console.GetOpt
import Xmobar.Plugins.Monitors.Common (parseOptsWith)
import Xmobar.Run.Exec
import Xmobar.System.Kbd
import Xmobar.X11.Events (nextEvent')

data Locks = Locks [String]
    deriving (Read, Show)

data MOptions = MOptions
  { oNumOn :: String
  , oNumOff :: String
  , oCapsOn :: String
  , oCapsOff :: String
  , oScrollOn :: String
  , oScrollOff :: String
  , oDelimiter :: String
  }

defaults :: MOptions
defaults = MOptions
  { oNumOn = "NUM"
  , oNumOff = ""
  , oCapsOn = "CAPS"
  , oCapsOff = ""
  , oScrollOn = "SCROLL"
  , oScrollOff = ""
  , oDelimiter = " "
  }

options :: [OptDescr (MOptions -> MOptions)]
options =
  [ Option "n" ["numoff"] (ReqArg (\x o -> o { oNumOff = x }) "") ""
  , Option "N" ["numon"] (ReqArg (\x o -> o { oNumOn = x }) "") ""
  , Option "c" ["capsoff"] (ReqArg (\x o -> o { oCapsOff = x }) "") ""
  , Option "C" ["capson"] (ReqArg (\x o -> o { oCapsOn = x }) "") ""
  , Option "s" ["scrolloff"] (ReqArg (\x o -> o { oScrollOff = x }) "") ""
  , Option "S" ["scrollon"] (ReqArg (\x o -> o { oScrollOn = x }) "") ""
  , Option "d" ["delimiter"] (ReqArg (\x o -> o { oDelimiter = x }) "") ""
  ]

locks :: [ ( KeySym, (MOptions -> String, MOptions -> String) )]
locks = [ ( xK_Caps_Lock,   (oCapsOn, oCapsOff) )
        , ( xK_Num_Lock,    (oNumOn, oNumOff) )
        , ( xK_Scroll_Lock, (oScrollOn, oScrollOff) )
        ]

run' :: Display -> Window -> MOptions -> IO String
run' d root opts = do
    modMap <- getModifierMapping d
    ( _, _, _, _, _, _, _, m ) <- queryPointer d root

    ls <- mapM ( \( ks, fs ) -> do
        kc <- keysymToKeycode d ks
        return
          $ (\b -> if b then fst fs else snd fs)
          $ case find (elem kc . snd) modMap of
            Nothing       -> False
            Just ( i, _ ) -> testBit m (fromIntegral i)
        ) locks

    return $ intercalate (oDelimiter opts)
      $ filter (not . null)
      $ map (\f -> f opts) ls

instance Exec Locks where
    alias (Locks _) = "locks"
    start (Locks args) cb = do
        opts <- parseOptsWith options defaults args
        d <- openDisplay ""
        root <- rootWindow d (defaultScreen d)
        _ <- xkbSelectEventDetails d xkbUseCoreKbd xkbIndicatorStateNotify m m

        allocaXEvent $ \ep -> forever $ do
            cb =<< run' d root opts
            nextEvent' d ep
            getEvent ep

        closeDisplay d
        return ()
      where
        m = xkbAllStateComponentsMask
