module System.Remote.Monitoring
    (
      -- * Required configuration
      -- $configuration

      -- * Security considerations
      -- $security

      -- * REST API
      -- $api

      -- * The monitoring server
      Server
    , serverThreadId
    , serverMetricStore
    , forkServer
    , forkServerNoHostname
    , forkServerWith
    , forkServerNoHostnameWith

      -- * Defining metrics
      -- $userdefined
    , getCounter
    , getGauge
    , getLabel
    , getDistribution
    ) where