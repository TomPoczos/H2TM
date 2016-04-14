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

module HtmData
( CellState          (..)
, ColumnState        (..)
, SynapseState       (..)
, Cell               (..)
, DistalSynapse      (..)
, ProximalSynapse    (..)
, Input              (..)
, Column             (..)
, Region             (..)
, DistalDendrite     (..)
, ComplianceOption   (..)
, ComplianceSettings (..)
, AcquisitionTime    (..)
, Permanence
, Boost
, LocalActivity
, InhibitionRadius
, Overlap
) where

import           Control.DeepSeq
import           CycleHistory
import           FlexibleParallelism

type Permanence       = Double

type Boost            = Double

type LocalActivity    = Integer

type InhibitionRadius = Integer

type Overlap          = Double

data AcquisitionTime  = Current | Prev

data CellState        = ActiveCell | PredictiveCell | InactiveCell

data ColumnState      = ActiveColumn | InactiveColumn

data SynapseState     = Potential | Actual

data Input            = On | Off

data ComplianceOption = Compliant | Modified

data ComplianceSettings = ComplianceSettings { permanenceBoost             :: !ComplianceOption
                                             , resetToFalse                :: !ComplianceOption
                                             , activeSegmentChoice         :: !ComplianceOption
                                             , boostDecrease               :: !ComplianceOption
                                             }

data Cell             = Cell                 { cellPredictiveState         :: !Bool
                                             , cellLearnState              :: !Bool
                                             , cellActiveState             :: !Bool
                                             , cellPrevActiveState         :: !Bool
                                             , cellPrevPredictiveState     :: !Bool
                                             , distalDendrites             :: ![DistalDendrite]
                                             , queuedDistalSynapses        :: ![DistalSynapse]
                                             , cellId                      :: !Integer
                                             }

data Column           = Column               { cells                       :: ![Cell]
                                             , proximalSynapses            :: ![ProximalSynapse]
                                             , boost                       :: !Double
                                             , overlap                     :: !Overlap
                                             , dutyCycles                  :: !CycleHistory
                                             , overlapCycles               :: !CycleHistory
                                             , columnState                 :: !ColumnState
                                             , columnId                    :: !Integer
                                             }

data DistalDendrite  = DistalDendrite        { distalSynapses              :: ![DistalSynapse]
                                             , sequenceSegment             :: !Bool
                                             , dendriteActiveState         :: !Bool
                                             , dendrtiteLearnState         :: !Bool
                                             , dendriteId                  :: !Integer
                                             }

data DistalSynapse    = DistalSynapse        { dInput                      :: !Input
                                             , dSynapseState               :: !SynapseState
                                             , dPrevSynapseState           :: !SynapseState
                                             , dPermanence                 :: !Permanence
                                             , dOriginatingCell            :: !Cell
                                             , dSyanpseId                  :: !Integer
                                             }

data ProximalSynapse  = ProximalSynapse      { pInput                      :: !Input
                                             , pSynapseState               :: !SynapseState
                                             , pPermanence                 :: !Permanence
                                             , timeStepIndex               :: !Int
                                             , pSynapseId                  :: !Integer
                                             }

data Region           = Region               { columns                     :: ![Column]
                                             , desiredLocalActivity        :: !LocalActivity
                                             , inhibitionRadius            :: !InhibitionRadius
                                             , minimumOverlap              :: !Overlap
                                             , permanenceInc               :: !Permanence
                                             , permanenceDec               :: !Permanence
                                             , boostInc                    :: !Double
                                             , permanenceThreshold         :: !Double
                                             , dendriteActivationThreshold :: !Int
                                             , dendriteMinThreshold        :: !Int
                                             , complianceSettings          :: !ComplianceSettings
                                             , parallelismMode             :: !ParallelismMode
                                             , learningOn                  :: !Bool
                                             , regionId                    :: !Integer
                                             }

instance NFData Column

instance Eq DistalSynapse where
    DistalSynapse _ _ _ _ _ id1 == DistalSynapse _ _ _ _ _ id2 = id1 == id2

        -- do not compare originating cells. Causes infinite mutual recursion

instance Eq ProximalSynapse where
    ProximalSynapse _ _ _ _ id1 == ProximalSynapse _ _ _ _ id2 = id1 == id2

instance Eq Input where
    On == On = True
    Off == Off = True
    _ == _ = False

instance Eq Column where
    Column _ _ _ _ _ _ _ id1 == Column _ _ _ _ _ _ _ id2 = id1 == id2

instance Eq Region where
    Region _ _ _ _ _ _ _ _ _ _ _ _ _ id1 == Region _ _ _ _ _ _ _ _ _ _ _ _ _ id2 = id1 == id2

instance Eq Cell where
    Cell _ _ _ _ _ _ _ id1== Cell _ _ _ _ _ _ _ id2 = id1 == id2

instance Eq DistalDendrite where
    DistalDendrite _ _ _ _ id1 == DistalDendrite _ _ _ _ id2 = id1 == id2

instance Eq ComplianceSettings where
    ComplianceSettings a1 a2 a3 a4 == ComplianceSettings b1 b2 b3 b4 =
        (a1 == b1)
        && (a2 == b2)
        && (a3 == b3)
        && (a4 == b4)

instance Eq ComplianceOption where
    Compliant == Compliant =  True
    Modified == Modified = True
    _ == _ = False

instance Eq SynapseState where
    Potential == Potential = True
    Actual == Actual = True
    _ == _ = False

instance Eq ColumnState where
    ActiveColumn == ActiveColumn = True
    InactiveColumn == InactiveColumn = True
    _ == _ = False

instance Eq CellState where
    ActiveCell == ActiveCell = True
    InactiveCell == InactiveCell = True
    PredictiveCell == PredictiveCell = True
    _ == _ = False
