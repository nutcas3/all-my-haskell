{-# OPTIONS_HADDOCK not-home #-}




module Monitor.Gauge
    ( Gauge.Gauge
    , Gauge.inc
    , Gauge.dec
    , Gauge.add
    , Gauge.subtract
    , Gauge.set
    ) where

import qualified Monitor.Metrics.Gauge as Gauge
