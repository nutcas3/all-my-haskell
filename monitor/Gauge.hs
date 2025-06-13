{-# OPTIONS_HADDOCK not-home #-}


-- | This module provides a remote gauge interface for the Monitor system.
module Gauge
    ( Gauge.Gauge
    , Gauge.inc
    , Gauge.dec
    , Gauge.add
    , Gauge.subtract
    , Gauge.set
    ) where

import qualified Monitor.Metrics.Gauge as Gauge
-- TODO: Replace with the correct import for Gauge, e.g.:
-- import qualified Some.Existing.Gauge.Module as Gauge
