{-# OPTIONS_HADDOCK not-home #-}

-- | This module provides a remote label interface for the Monitor system.
--   It allows setting and modifying labels in a remote context.
module Monitor.Remote.Label
    (
      Label.Label
    , Label.set
    , Label.modify
    ) where

import qualified Monitor.Metrics.Label as Label