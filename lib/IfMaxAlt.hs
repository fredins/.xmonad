-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IfMaxAlt
-- Copyright   :  (c) 2013 Ilya Portnov
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides IfMax layout, which will run one layout if there are maximum N
-- windows on workspace, and another layout, when number of windows is greater
-- than N.
--
-- ctrl-alt-etc Note:
-- This is a version of IfMax [1] patchedby GitHub user f1u77y [2] to enable it
-- to work with window Decorations. The original version causes decorations to
-- be draw even when those windows are on the currently visible workspace. This
-- version comes from xmonad-contrib Issue #75 [3]. I renamed the module to
-- avoid potential clashes with the original.
--
-- [1] https://github.com/xmonad/xmonad-contrib/blob/master/XMonad/Layout/IfMax.hs
-- [2] https://github.com/f1u77y
-- [3] https://github.com/xmonad/xmonad-contrib/issues/75#issuecomment-241734525
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}

module IfMaxAlt
    ( -- * Usage
      -- $usage
      IfMaxAlt (..)
    , ifMax
    ) where

import           Control.Arrow
import qualified Data.List       as L
import qualified Data.Map        as M
import           Data.Maybe
import           Prelude

import           XMonad
import qualified XMonad.StackSet as W

-- $usage
-- IfMaxAlt layout will run one layout if number of windows on workspace is as
-- maximum N, and else will run another layout.
--
-- You can use this module by adding folowing in your @xmonad.hs@:
--
-- > import XMonad.Layout.IfMaxAlt
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = IfMaxAlt 2 Full (Tall ...) ||| ...
--
-- In this example, if there are 1 or 2 windows, Full layout will be used;
-- otherwise, Tall layout will be used.
--

data IfMaxAlt l1 l2 w = IfMaxAlt Int (l1 w) (l2 w)
  deriving (Read, Show)

instance (LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (IfMaxAlt l1 l2) Window where

  runLayout (W.Workspace wname (IfMaxAlt n l1 l2) s) rect = withWindowSet $ \ws -> arrange (W.integrate' s) (M.keys . W.floating $ ws)
    where
      arrange ws fw | length (ws L.\\ fw) <= n = do
                                    (wrs, ml1') <- runLayout (W.Workspace wname l1 s) rect
                                    let l1' = fromMaybe l1 ml1'
                                    l2' <- fromMaybe l2 <$> handleMessage l2 (SomeMessage Hide)
                                    return (wrs, Just $ IfMaxAlt n l1' l2')
                    | otherwise      = do
                                    (wrs, ml2') <- runLayout (W.Workspace wname l2 s) rect
                                    l1' <- fromMaybe l1 <$> handleMessage l1 (SomeMessage Hide)
                                    let l2' = fromMaybe l2 ml2'
                                    return (wrs, Just $ IfMaxAlt n l1' l2')

  handleMessage (IfMaxAlt n l1 l2) m | Just ReleaseResources <- fromMessage m = do
      l1' <- handleMessage l1 (SomeMessage ReleaseResources)
      l2' <- handleMessage l2 (SomeMessage ReleaseResources)
      if isNothing l1' && isNothing l2'
         then return Nothing
         else return $ Just $ IfMaxAlt n (fromMaybe l1 l1') (fromMaybe l2 l2')

  -- This is the code added by f1u77y
  handleMessage (IfMaxAlt n l1 l2) m | Just Hide <- fromMessage m = do
      l1' <- handleMessage l1 (SomeMessage ReleaseResources)
      l2' <- handleMessage l2 (SomeMessage ReleaseResources)
      if isNothing l1' && isNothing l2'
         then return Nothing
         else return $ Just $ IfMaxAlt n (fromMaybe l1 l1') (fromMaybe l2 l2')

  handleMessage (IfMaxAlt n l1 l2) m = do
      (allWindows, floatingWindows) <- gets ((W.integrate' . W.stack . W.workspace . W.current &&& M.keys . W.floating) . windowset)
      if length (allWindows L.\\ floatingWindows) <= n
        then do
          l1' <- handleMessage l1 m
          return $ flip (IfMaxAlt n) l2 <$> l1'
        else do
          l2' <- handleMessage l2 m
          return $ IfMaxAlt n l1 <$> l2'

  description (IfMaxAlt n l1 l2) = "If number of windows is <= " ++ show n ++ ", then " ++
                                description l1 ++ ", else " ++ description l2

-- | Layout itself
ifMax :: (LayoutClass l1 w, LayoutClass l2 w)
      => Int            -- ^ Maximum number of windows for the first layout
      -> l1 w           -- ^ First layout
      -> l2 w           -- ^ Second layout
      -> IfMaxAlt l1 l2 w
ifMax n l1 l2 = IfMaxAlt n l1 l2
