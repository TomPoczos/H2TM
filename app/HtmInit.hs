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

module HtmInit (htmInit, HtmConfig(..) ) where

import           Control.Monad
import           Control.Monad.Random
import           CycleHistory
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.UUID.Types
import qualified Data.UUID.V4         as UUID
import           Debug.Trace
import           FlexibleParallelism
import qualified HtmData              as Htm
import           System.Random

-- always initialised with learning on, which is necessary for training
-- but can switched off after that

data HtmConfig = HtmConfig {
      numOfColumns         :: Integer
    , numOfCells           :: Integer
    , pSynapses            :: Integer
    , numOfDDendrites      :: Integer
    , dSynapses            :: Integer
    , timeStepSize         :: Int
    , permThreshold        :: Double
    , permChangeCompliance :: Htm.ComplianceOption
    , boostDecCompliance   :: Htm.ComplianceOption
    , parallelism          :: ParallelismMode }

htmInit :: HtmConfig -> RandT StdGen IO Htm.Region
htmInit config = do
    uuid    <- liftIO UUID.nextRandom
    columns <- traverse createColumn [1..numOfColumns config]
    let initialRegion = Htm.Region {
          Htm.columns                     = columns
        , Htm.desiredLocalActivity        = 15
        , Htm.inhibitionRadius            = numOfColumns config - 1
        , Htm.minimumOverlap              = 3
        , Htm.permanenceInc               = 0.1
        , Htm.permanenceDec               = 0.05
        , Htm.boostInc                    = 1
        , Htm.permanenceThreshold         = permThreshold config
        , Htm.dendriteActivationThreshold = 2
        , Htm.dendriteMinThreshold        = 2
        , Htm.complianceSettings          = createComplianceSettings
        , Htm.parallelismMode             = parallelism config
        , Htm.learningOn                  = True
        , Htm.regionId                    = uuid }
    initialRegion & setUpOriginatinCells >>= pSynapseActiveStates >>= dSynapseActiveStates

    where

        dSynapseActiveStates :: Htm.Region -> RandT StdGen IO Htm.Region
        dSynapseActiveStates region = return
            region {Htm.columns = region & Htm.columns & map (\column ->
                column {Htm.cells = column & Htm.cells & map (\cell ->
                    cell{Htm.distalDendrites = cell & Htm.distalDendrites & map (\dendrite ->
                        dendrite {Htm.distalSynapses = dendrite & Htm.distalSynapses & map (\dSyn ->
                        dSyn {Htm.dSynapseState = if Htm.dPermanence dSyn >= permThreshold config then Htm.Actual else Htm.Potential })})})})}

        pSynapseActiveStates :: Htm.Region -> RandT StdGen IO Htm.Region
        pSynapseActiveStates region = return
            region{ Htm.columns = region & Htm.columns & map (\col ->
                col {Htm.proximalSynapses = col & Htm.proximalSynapses & map (\pSyn ->
                    pSyn {Htm.pSynapseState = if Htm.pPermanence pSyn >= permThreshold config then Htm.Actual else Htm.Potential })})}

        createComplianceSettings = Htm.ComplianceSettings {
              Htm.permanenceBoost     = permChangeCompliance config
            , Htm.resetToFalse        = Htm.Modified -- Numenta implementation actually same as my modified version
            , Htm.activeSegmentChoice = Htm.Modified -- despite the fact that the pseudocode des not contain the changes
            , Htm.boostDecrease       = boostDecCompliance config }

        createColumn :: a -> RandT StdGen IO Htm.Column
        createColumn _ = do
          uuid             <- liftIO UUID.nextRandom
          cells            <- traverse createCells [1..numOfCells config]
          proximalSynapses <- traverse createPSynapses [1..pSynapses config]
          return Htm.Column {
                Htm.cells            = cells
              , Htm.proximalSynapses = proximalSynapses
              , Htm.boost            = 1
              , Htm.overlap          = 0.2
              , Htm.columnId         = uuid
              , Htm.dutyCycles       = CycleHistory [] 0 1000
              , Htm.overlapCycles    = CycleHistory [] 0 1000
              , Htm.columnState      = Htm.ActiveColumn }

        createCells :: a -> RandT StdGen IO Htm.Cell
        createCells _ = do
            uuid       <- liftIO UUID.nextRandom
            dDendrites <- traverse createDDendrites [1..numOfDDendrites config]
            return Htm.Cell {
                  Htm.cellPredictiveState     = False
                , Htm.cellLearnState          = True
                , Htm.cellActiveState         = True
                , Htm.cellPrevActiveState     = False
                , Htm.cellPrevPredictiveState = False
                , Htm.distalDendrites         = dDendrites
                , Htm.queuedDistalSynapses    = []
                , Htm.cellId                  = uuid }

        createDDendrites :: a -> RandT StdGen IO Htm.DistalDendrite
        createDDendrites _ = do
            uuid           <- liftIO UUID.nextRandom
            distalSynapses <- traverse createDSynapses [1..dSynapses config]
            return Htm.DistalDendrite {
                  Htm.distalSynapses      = distalSynapses
                , Htm.sequenceSegment     = False
                , Htm.dendriteActiveState = True
                , Htm.dendrtiteLearnState = True
                , Htm.dendriteId          = uuid }

        createDSynapses :: a -> RandT StdGen IO Htm.DistalSynapse
        createDSynapses _ = do
            uuid                       <- liftIO UUID.nextRandom
            originatingCellsDDendrites <- traverse createDDendrites [1..numOfDDendrites config]
            x            <- getRandomR (permThreshold config - 0.01, permThreshold config + 0.01)
            return Htm.DistalSynapse {
                  Htm.dInput            = Htm.Off
                , Htm.dSynapseState     = Htm.Potential
                , Htm.dPrevSynapseState = Htm.Potential
                , Htm.dPermanence       = x
                , Htm.dOriginatingCell  = Htm.Cell False False False False False originatingCellsDDendrites [] uuid
                , Htm.dSyanpseId        = uuid }
                {-temporary originating cell, replaced in last step of init-}

        createPSynapses :: a -> RandT StdGen IO Htm.ProximalSynapse
        createPSynapses _ = do
            uuid <- liftIO UUID.nextRandom
            timeStepIndex <- getRandomR (0, timeStepSize config - 1) -- & (\a -> trace (show a) a)
            pPermanence <- getRandomR (permThreshold config - 0.01, permThreshold config + 0.01) -- [] 0 14 :: Int) & (\x -> if x == 1 then 0.3 else 0.0)
            return Htm.ProximalSynapse {
                  Htm.pInput        = Htm.Off
                , Htm.pSynapseState = Htm.Potential
                , Htm.timeStepIndex = timeStepIndex
                , Htm.pPermanence   = pPermanence
                , Htm.pSynapseId    = uuid }

        setUpOriginatinCells :: Htm.Region -> RandT StdGen IO Htm.Region
        setUpOriginatinCells region =
            return region { Htm.columns = region & Htm.columns & map (\column ->
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
                                            !! 0) --getRnd stdGen [] 0 (length allCells -1))
                                                -- & (\ind -> trace ("Index: " ++ show ind) ind)
                                                })})}))})}
