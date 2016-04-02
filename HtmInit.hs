module HtmInit (htmInit) where

import           CycleHistory
import           Data.List
import           Flow
import           FlexibleParallelism
import qualified HtmData    as Htm
import           System.Random
import           System.IO.Unsafe

htmInit :: Integer -- number of columns
        -> Integer -- number of cells per column
        -> Integer -- number of proximal synapses per column
        -> Integer -- number of distal dendrites per cell
        -> Integer -- number of distal synapses per dendrite
        -> Double  -- permanence threshold
        -> Bool    -- Learning on if True, off if false
        -> ParallelismMode
        -> Htm.Region

htmInit columns cells pSynapses dDendrites dSynapses permThreshold learning parallelism =
    Htm.Region { Htm.columns                     = [1..columns] |> map createColumns
               , Htm.desiredLocalActivity        = 0
               , Htm.inhibitionRadius            = 0
               , Htm.minimumOverlap              = 0.0
               , Htm.permanenceInc               = 0.0
               , Htm.permanenceDec               = 0.0
               , Htm.boostInc                    = 0.0
               , Htm.permanenceThreshold         = permThreshold
               , Htm.dendriteActivationThreshold = 0
               , Htm.dendriteMinThreshold        = 0
               , Htm.complianceSettings          = 0.0
               , Htm.parallelismMode             = parallelism
               , Htm.learningOn                  =  learning}

    where createColumns colId       = Htm.Column          { Htm.cells            = [1..cells] |> map createCells
                                                          , Htm.proximalSynapses = [1..pSynapses] |> map createPSynapses
                                                          , Htm.boost            = 0.0
                                                          , Htm.overlap          = 0.0
                                                          , Htm.key              = colId
                                                          , Htm.dutyCycles       = CycleHistory [] 0 0
                                                          , Htm.overlapCycles    = CycleHistory [] 0 0
                                                          , Htm.columnState      = Htm.InactiveColumn }

          createCells dummyArg      = Htm.Cell            { Htm.cellPredictiveState     = False
                                                          , Htm.cellLearnState          = False
                                                          , Htm.cellActiveState         = False
                                                          , Htm.cellPrevActiveState     = False
                                                          , Htm.cellPrevPredictiveState = False
                                                          , Htm.distalDendrites         = [1..dDendrites] |> map createDDendrites
                                                          , Htm.queuedDistalSynapses    = [] }

          createDDendrites dummyArg = Htm.DistalDendrite  { Htm.distalSynapses      = [1..dSynapses] |> map createDSynapses
                                                          , Htm.sequenceSegment     = False
                                                          , Htm.dendriteActiveState = False
                                                          , Htm.dendrtiteLearnState = False }

          createDSynapses dummyArg  = Htm.DistalSynapse   { Htm.dInput            = Htm.Off
                                                          , Htm.dSynapseState     = Htm.Potential
                                                          , Htm.dPrevSynapseState = Htm.Potential
                                                          , Htm.dPermanence       = getRndDouble (permThreshold - 0.1) (permThreshold - 0.01)
                                                          , Htm.dOriginatingCell  = Htm.Cell{}}

          createPSynapses dummyArg  = Htm.ProximalSynapse { Htm.pInput        = Htm.Off
                                                          , Htm.pSynapseState = Htm.Potential
                                                          , Htm.pPermanence   = getRndDouble (permThreshold - 0.1) (permThreshold - 0.01)}

          getRndDouble :: Double -> Double -> Double
          getRndDouble minVal maxVal =
              head $ randomRs (minVal, maxVal) getStdGen

          getRndInts :: Int -> Int -> Int -> [Int]
          getRndInts amount minVal maxVal =
              take amount $ randomRs (minVal, maxVal) getStdGen
