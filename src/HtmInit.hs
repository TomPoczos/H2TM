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

import Debug.Trace
import           CycleHistory
import           Data.List
import           Data.Maybe
import           Data.UUID.Types
import           FlexibleParallelism
import           Data.Function
import           HtmUtils
import qualified HtmData             as Htm
import           System.Random

-- always initialised with learning on, which is necessary for training
-- but can switched off after that

htmInit :: Integer              -- number of columns
        -> Integer              -- number of cells per column
        -> Integer              -- number of proximal synapses per column
        -> Integer              -- number of distal dendrites per cell
        -> Integer              -- number of distal synapses per dendrite
        -> Int                  -- timestep size
        -> Double               -- permanence threshold
        -> Htm.ComplianceOption -- mode of icrease all permanences in SP phase 3
        -> Htm.ComplianceOption -- mode of decerasing boost value in SP phase 3
        -> ParallelismMode
        -> StdGen
        -> Htm.Region


htmInit columns cells pSynapses dDendrites dSynapses timeStepSize permThreshold permChangeCompliance boostDecCompliance parallelism stdGen
                                   = Htm.Region             { Htm.columns                     = [1..columns] & map createColumns
                                                            , Htm.desiredLocalActivity        = 15
                                                            , Htm.inhibitionRadius            = columns - 1
                                                            , Htm.minimumOverlap              = 3
                                                            , Htm.permanenceInc               = 0.1
                                                            , Htm.permanenceDec               = 0.05
                                                            , Htm.boostInc                    = 1
                                                            , Htm.permanenceThreshold         = permThreshold
                                                            , Htm.dendriteActivationThreshold = 2
                                                            , Htm.dendriteMinThreshold        = 2
                                                            , Htm.complianceSettings          = createComplianceSettings
                                                            , Htm.parallelismMode             = parallelism
                                                            , Htm.learningOn                  = True
                                                            , Htm.regionId                    = getUUID}
                                                            & setUpOriginatinCells
                                                            & pSynapseActiveStates
                                                            & dSynapseActiveStates

    where dSynapseActiveStates region =
              region {Htm.columns = region & Htm.columns & map (\column ->
                  column {Htm.cells = column & Htm.cells & map (\cell ->
                      cell{Htm.distalDendrites = cell & Htm.distalDendrites & map (\dendrite ->
                          dendrite {Htm.distalSynapses = dendrite & Htm.distalSynapses & map (\dSyn ->
                          dSyn {Htm.dSynapseState = if Htm.dPermanence dSyn >= permThreshold then Htm.Actual else Htm.Potential})})})})}

          pSynapseActiveStates region =
              region{ Htm.columns = region & Htm.columns & map (\col ->
                  col {Htm.proximalSynapses = col & Htm.proximalSynapses & map (\pSyn ->
                      pSyn {Htm.pSynapseState = if Htm.pPermanence pSyn >= permThreshold then Htm.Actual else Htm.Potential})})}




          createComplianceSettings = Htm.ComplianceSettings { Htm.permanenceBoost             = permChangeCompliance
                                                            , Htm.resetToFalse                = Htm.Modified -- Numenta implementation actually same as my modified version
                                                            , Htm.activeSegmentChoice         = Htm.Modified -- despite the fact that the pseudocode des not contain the changes
                                                            , Htm.boostDecrease               = boostDecCompliance
                                                            }

          createColumns _          = Htm.Column             { Htm.cells                       = [1..cells] & map createCells
                                                            , Htm.proximalSynapses            = [1..pSynapses] & map createPSynapses
                                                            , Htm.boost                       = 1
                                                            , Htm.overlap                     = 0.2
                                                            , Htm.columnId                    = getUUID
                                                            , Htm.dutyCycles                  = CycleHistory [] 0 1000
                                                            , Htm.overlapCycles               = CycleHistory [] 0 1000
                                                            , Htm.columnState                 = Htm.ActiveColumn }

          createCells _            = Htm.Cell               { Htm.cellPredictiveState         = False
                                                            , Htm.cellLearnState              = True
                                                            , Htm.cellActiveState             = True
                                                            , Htm.cellPrevActiveState         = False
                                                            , Htm.cellPrevPredictiveState     = False
                                                            , Htm.distalDendrites             = [1..dDendrites] & map createDDendrites
                                                            , Htm.queuedDistalSynapses        = []
                                                            , Htm.cellId                      = getUUID}

          createDDendrites _       = Htm.DistalDendrite     { Htm.distalSynapses              = [1..dSynapses] & map createDSynapses
                                                            , Htm.sequenceSegment             = False
                                                            , Htm.dendriteActiveState         = True
                                                            , Htm.dendrtiteLearnState         = True
                                                            , Htm.dendriteId                  = getUUID}

          createDSynapses _        = Htm.DistalSynapse      { Htm.dInput                      = Htm.Off
                                                            , Htm.dSynapseState               = Htm.Potential
                                                            , Htm.dPrevSynapseState           = Htm.Potential
                                                            , Htm.dPermanence                 = getRnd stdGen [] (permThreshold - 0.01) (permThreshold + 0.01)
                                                            , Htm.dOriginatingCell            = Htm.Cell False False False False False ([1..dDendrites] & map createDDendrites) [] getUUID
                                                            , Htm.dSyanpseId                  = getUUID}
                                                            {-temporary originating cell, replaced in last step of init-}

          createPSynapses _        = Htm.ProximalSynapse    { Htm.pInput                      = Htm.Off
                                                            , Htm.pSynapseState               = Htm.Potential
                                                            , Htm.timeStepIndex               = getRnd stdGen [] 0 (timeStepSize - 1) -- & (\a -> trace (show a) a)
                                                            , Htm.pPermanence                 = (getRnd stdGen [] (permThreshold - 0.01) (permThreshold + 0.01) )-- [] 0 14 :: Int) & (\x -> if x == 1 then 0.3 else 0.0)
                                                            , Htm.pSynapseId                  = getUUID}

          setUpOriginatinCells region =
                    region { Htm.columns = region & Htm.columns & map (\column ->
                        column { Htm.cells = column & Htm.cells & (\columnsCells ->
                            columnsCells & map (\cell ->
                                cell { Htm.distalDendrites = cell & Htm.distalDendrites & map (\dendrite ->
                                    dendrite { Htm.distalSynapses = dendrite & Htm.distalSynapses & map (\synapse ->
                                        synapse{ Htm.dOriginatingCell =
                                            region & Htm.columns
                                                   & map Htm.cells
                                                   & concat
                                                  -- & filter (flip notElem columnsCells)
                                                   & (\allCells ->  {-(trace ("length: " ++ (show $ length allCells ))-} allCells -- )


                                                    !! getRnd stdGen [] 0 (length allCells -1))



                                                        -- & (\ind -> trace ("Index: " ++ show ind) ind)


                                                        })})}))})}
