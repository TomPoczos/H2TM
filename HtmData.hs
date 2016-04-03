{-------------------------------------------------------------------------------
H2TM: A Haskell HTM/CLA Implementation
Copyright (C) 2015-2016, Tom Poczos

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
along with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0
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

import CycleHistory
import FlexibleParallelism
import Control.DeepSeq

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

data ComplianceSettings = ComplianceSettings { permanenceBoost             :: ComplianceOption
                                             , resetToFalse                :: ComplianceOption
                                             , activeSegmentChoice         :: ComplianceOption
                                             , boostDecrease               :: ComplianceOption
                                             }

data Cell             = Cell                 { cellPredictiveState         :: Bool
                                             , cellLearnState              :: Bool
                                             , cellActiveState             :: Bool
                                             , cellPrevActiveState         :: Bool
                                             , cellPrevPredictiveState     :: Bool
                                             , distalDendrites             :: [DistalDendrite]
                                             , queuedDistalSynapses        :: [DistalSynapse]
                                             }

data Column           = Column               { cells                       :: [Cell]
                                             , proximalSynapses            :: [ProximalSynapse]
                                             , boost                       :: Double
                                             , overlap                     :: Overlap
                                             , key                         :: Integer
                                             , dutyCycles                  :: CycleHistory
                                             , overlapCycles               :: CycleHistory
                                             , columnState                 :: ColumnState
                                             }

data DistalDendrite  = DistalDendrite        { distalSynapses              :: [DistalSynapse]
                                             , sequenceSegment             :: Bool
                                             , dendriteActiveState         :: Bool
                                             , dendrtiteLearnState         :: Bool
                                             }

data DistalSynapse    = DistalSynapse        { dInput                      :: Input
                                             , dSynapseState               :: SynapseState
                                             , dPrevSynapseState           :: SynapseState
                                             , dPermanence                 :: Permanence
                                             , dOriginatingCell            :: Cell
                                             }

data ProximalSynapse  = ProximalSynapse      { pInput                      :: Input
                                             , pSynapseState               :: SynapseState
                                             , pPermanence                 :: Permanence
                                             }

data Region           = Region               { columns                     :: [Column]
                                             , desiredLocalActivity        :: LocalActivity
                                             , inhibitionRadius            :: InhibitionRadius
                                             , minimumOverlap              :: Overlap
                                             , permanenceInc               :: Permanence
                                             , permanenceDec               :: Permanence
                                             , boostInc                    :: Double
                                             , permanenceThreshold         :: Double
                                             , dendriteActivationThreshold :: Int
                                             , dendriteMinThreshold        :: Int
                                             , complianceSettings          :: ComplianceSettings
                                             , parallelismMode             :: ParallelismMode
                                             , learningOn                  :: Bool
                                             }

instance NFData Column

instance Eq DistalSynapse where
    DistalSynapse a1 a2 a3 a4 a5 == DistalSynapse b1 b2 b3 b4 b5 =
        (a1 == b1)
        && (a2 == b2)
        && (a3 == b3)
        && (abs (a4 - b4) <= 0.001)
        && (a5 == b5)

instance Eq ProximalSynapse where
    ProximalSynapse a1 a2 a3 == ProximalSynapse b1 b2 b3 =
        (a1 == b1) && (a2 == b2) && (abs (a3 - b3) <= 0.001)

instance Eq Input where
    On == On = True
    Off == Off = True
    _ == _ = False

instance Eq Column where
    Column a1 a2 a3 a4 a5 a6 a7 a8 == Column b1 b2 b3 b4 b5 b6 b7 b8 =
        (a1 == b1)
        && (a2 == b2)
        && (abs (a3 - b3) <= 0.001)
        && (abs (a4 - b4) <= 0.001)
        && (a5 == b5)
        && (a6 == b6)
        && (a7 == b7)
        && (a8 == b8)

instance Eq Region where
    Region a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 == Region b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 =
        (a1 == b1)
        && (a2 == b2)
        && (a3 == b3)
        && (abs (a4 - b4) <= 0.001)
        && (abs (a5 - b5) <= 0.001)
        && (abs (a6 - b6) <= 0.001)
        && (abs (a7 - b7) <= 0.001)
        && (abs (a8 - b8) <= 0.001)
        && (a9 == b9)
        && (a10 == b10)
        && (a11 == b11)
        && (a12 == b12)
        && (a13 == b13)

instance Eq Cell where
    Cell a1 a2 a3 a4 a5 a6 a7 == Cell b1 b2 b3 b4 b5 b6 b7 =
        (a1 == b1)
        && (a2 == b2)
        && (a3 == b3)
        && (a4 == b4)
        && (a5 == b5)
        && (a6 == b6)
        && (a7 == b7)

instance Eq DistalDendrite where
    DistalDendrite a1 a2 a3 a4 == DistalDendrite b1 b2 b3 b4 =
        (a1 == b1)
        && (a2 == b2)
        && (a3 == b3)
        && (a4 == b4)

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
