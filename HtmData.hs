module HtmData
( CellState        (..)
, ColumnState      (..)
, SynapseState     (..)
, Cell             (..)
, DistalSynapse    (..)
, ProximalSynapse  (..)
, Input            (..)
, Column           (..)
, Region           (..)
, OperationMode    (..)
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

data OperationMode    = Compliant | Modified

data CellState        = ActiveCell | PredictiveCell | InactiveCell

data ColumnState      = ActiveColumn | InactiveColumn

data SynapseState     = Potential | Actual

data Input            = On | Off

data Cell             = Cell             { cellState             :: CellState
                                         , distalSynapses        :: [DistalSynapse]
                                         }

data Column           = Column           { cells                 :: [Cell]
                                         , proximalSynapses      :: [ProximalSynapse]
                                         , boost                 :: Double
                                         , overlap               :: Overlap
                                         , key                   :: Integer
                                         , dutyCycles            :: CycleHistory
                                         , overlapCycles         :: CycleHistory
                                         , columnState           :: ColumnState
                                         }

data DistalSynapse    = DistalSynapse    { dInput                :: Input
                                         , dSynapseState         :: SynapseState
                                         , dPermanence           :: Permanence
                                         }

data ProximalSynapse  = ProximalSynapse  { pInput                :: Input
                                         , pSynapseState         :: SynapseState
                                         , pPermanence           :: Permanence
                                         }

data Region           = Region           { columns               :: [Column]
                                         , activeColumns         :: [Column]
                                         , desiredLocalActivity  :: LocalActivity
                                         , inhibitionRadius      :: InhibitionRadius
                                         , minimumOverlap        :: Overlap
                                         , permanenceInc         :: Permanence
                                         , permanenceDec         :: Permanence
                                         , boostInc              :: Double
                                         , permanenceThreshold   :: Double
                                         , operationMode         :: OperationMode
                                         , parallelismMode       :: ParallelismMode
                                         }

instance NFData Column


instance Eq DistalSynapse where
    DistalSynapse a1 a2 a3 == DistalSynapse b1 b2 b3 =
        (a1 == b1) && (a2 == b2) && (abs (a3 - b3) <= 0.001)

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
    Region a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 == Region b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
        (a1 == b1)
        && (a2 == b2)
        && (a3 == b3)
        && (a4 == b4)
        && (abs (a5 - b5) <= 0.001)
        && (abs (a6 - b6) <= 0.001)
        && (abs (a7 - b7) <= 0.001)
        && (abs (a8 - b8) <= 0.001)
        && (abs (a9 - b9) <= 0.001)
        && (a10 == b10)
        && (a11 == b11)

instance Eq Cell where
    Cell a1 a2 == Cell b1 b2 = (a1 == b1) && (a2 == b2)

instance Eq OperationMode where
    Compliant == Compliant=  True
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
