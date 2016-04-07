{-------------------------------------------------------------------------------
H2TM: A Haskell HTM/CLA Implementation
Copyright (C) 2016, Tom Poczos

Developed as part of a Final Year Project at Staffordshire University,
United Kingdom by Tom Poczos under the supervision of Dr. Mohamed Sedky,
based on the Hierarchical Temporal Memory paper released by Numenta Inc.:

http://numenta.com/assets/pdf/whitepapers/hierarchical-temporal-memory-cortical-learning-algorithm-0.2.1-en.pdf

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see http://www.gnu.org/licenses/agpl-3.0
-------------------------------------------------------------------------------}

module HtmInit (htmInit) where

import           CycleHistory
import           Data.List
import           Data.Maybe
import           FlexibleParallelism
import           Flow
import qualified HtmData             as Htm
import           System.Random

-- always initialised with learning on, which is necessary for training
-- but can switched off after that

htmInit :: Integer              -- number of columns
        -> Integer              -- number of cells per column
        -> Integer              -- number of proximal synapses per column
        -> Integer              -- number of distal dendrites per cell
        -> Integer              -- number of distal synapses per dendrite
        -> Double               -- permanence threshold
        -> Htm.ComplianceOption -- mode of icrease all permanences in SP phase 3
        -> Htm.ComplianceOption -- mode of decerasing boost value in SP phase 3
        -> ParallelismMode
        -> StdGen
        -> Htm.Region


htmInit columns cells pSynapses dDendrites dSynapses permThreshold permChangeCompliance boostDecCompliance parallelism stdGen
                                   = Htm.Region             { Htm.columns                     = [1..columns] |> map createColumns
                                                            , Htm.desiredLocalActivity        = 0
                                                            , Htm.inhibitionRadius            = 0
                                                            , Htm.minimumOverlap              = 0.0
                                                            , Htm.permanenceInc               = 0.0
                                                            , Htm.permanenceDec               = 0.0
                                                            , Htm.boostInc                    = 0.0
                                                            , Htm.permanenceThreshold         = permThreshold
                                                            , Htm.dendriteActivationThreshold = 0
                                                            , Htm.dendriteMinThreshold        = 0
                                                            , Htm.complianceSettings          = createComplianceSettings
                                                            , Htm.parallelismMode             = parallelism
                                                            , Htm.learningOn                  = True}
                                                            |> setUpOriginatinCells

    where createComplianceSettings = Htm.ComplianceSettings { Htm.permanenceBoost             = permChangeCompliance
                                                            , Htm.resetToFalse                = Htm.Modified -- Numenta implementation actually same as my modified version
                                                            , Htm.activeSegmentChoice         = Htm.Modified -- despite the fact that the pseudocode des not contain the changes
                                                            , Htm.boostDecrease               = boostDecCompliance
                                                            }

          createColumns colId       = Htm.Column            { Htm.cells                       = [1..cells] |> map createCells
                                                            , Htm.proximalSynapses            = [1..pSynapses] |> map createPSynapses
                                                            , Htm.boost                       = 0.0
                                                            , Htm.overlap                     = 0.0
                                                            , Htm.key                         = colId
                                                            , Htm.dutyCycles                  = CycleHistory [] 0 0
                                                            , Htm.overlapCycles               = CycleHistory [] 0 0
                                                            , Htm.columnState                 = Htm.InactiveColumn }

          createCells dummyArg      = Htm.Cell              { Htm.cellPredictiveState         = False
                                                            , Htm.cellLearnState              = False
                                                            , Htm.cellActiveState             = False
                                                            , Htm.cellPrevActiveState         = False
                                                            , Htm.cellPrevPredictiveState     = False
                                                            , Htm.distalDendrites             = [1..dDendrites] |> map createDDendrites
                                                            , Htm.queuedDistalSynapses        = [] }

          createDDendrites dummyArg = Htm.DistalDendrite    { Htm.distalSynapses              = [1..dSynapses] |> map createDSynapses
                                                            , Htm.sequenceSegment             = False
                                                            , Htm.dendriteActiveState         = False
                                                            , Htm.dendrtiteLearnState         = False }

          createDSynapses dummyArg  = Htm.DistalSynapse     { Htm.dInput                      = Htm.Off
                                                            , Htm.dSynapseState               = Htm.Potential
                                                            , Htm.dPrevSynapseState           = Htm.Potential
                                                            , Htm.dPermanence                 = getRndDouble (permThreshold - 0.1) (permThreshold - 0.01)}
                                                            {-originating cells are initialised after the entire region
                                                            (and particularly all the cells) is set up ; ignore compiler warning-}

          createPSynapses dummyArg  = Htm.ProximalSynapse   { Htm.pInput                      = Htm.Off
                                                            , Htm.pSynapseState               = Htm.Potential
                                                            , Htm.pPermanence                 = getRndDouble (permThreshold - 0.1) (permThreshold - 0.01)}

          setUpOriginatinCells region =
                    region { Htm.columns = region |> Htm.columns |> map (\column ->
                        column { Htm.cells = column |> Htm.cells |> map (\cell ->
                            cell { Htm.distalDendrites = cell |> Htm.distalDendrites |> map (\dendrite ->
                                dendrite { Htm.distalSynapses = dendrite |> Htm.distalSynapses |> map (\synapse ->
                                    synapse{ Htm.dOriginatingCell =
                                        region |> Htm.columns
                                               |> concatMap Htm.cells
                                               |> (\allCells -> allCells !! getRndInteger [fromJust $ elemIndex cell allCells] 0 (length allCells - 1))})})})})}

          getRndDouble :: Double -> Double -> Double
          getRndDouble minVal maxVal =
              head $ randomRs (minVal, maxVal) stdGen

          getRndInteger :: [Int] -> Int -> Int -> Int
          getRndInteger disallowed minVal maxVal =  getRnd |> (\x ->
              case () of
                  _ | x `notElem` disallowed -> x
                    | otherwise              -> getRnd)
              where getRnd = head $ randomRs (minVal, maxVal) stdGen
