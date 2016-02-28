module HtmData
( CellState        (..)
, SynapseState     (..)
, Cell             (..)
, DistalSynapse    (..)
, ProximalSynapse  (..)
, Input            (..)
, Column           (..)
, Region           (..)
, DutyCycleHistory (..)
, Permanence
, Boost
, LocalActivity
, InhibitionRadius
, Overlap
) where

type Permanence       = Double

type Boost            = Double

type LocalActivity    = Integer

type InhibitionRadius = Integer

type Overlap          = Double

data CellState        = Active | Predictive | Inactive

data SynapseState     = Potential | Actual

data Input            = On | Off

data Cell             = Cell             { cellState             :: CellState
                                         , distalSynapses        :: [DistalSynapse]
                                         }

data Column           = Column           { cells                 :: [Cell]
                                         , proximalSynapses      :: [ProximalSynapse]
                                         , boost                 :: Double
                                         , key                   :: Integer
                                         , pastCycles            :: DutyCycleHistory
                                         , pastOverlapCycles     :: DutyCycleHistory
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
                                         , previouslyActiveCells :: [Cell]
                                         , desiredLocalActivity  :: LocalActivity
                                         , inhibitionRadius      :: InhibitionRadius
                                         , minimumOverlap        :: Overlap
                                         , permanenceInc         :: Permanence
                                         , permanenceDec         :: Permanence
                                         , boostInc              :: Double
                                         }

data DutyCycleHistory = DutyCycleHistory { values                :: [Integer]
                                         , numOfVals             :: Integer
                                         , maxAmount             :: Integer
                                         }

instance Eq DutyCycleHistory where
    DutyCycleHistory a1 a2 a3 == DutyCycleHistory b1 b2 b3 =
        a1 == b1 && a2 == b2 && a3 == b3

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
 Column a1 a2 a3 a4 a5 a6 == Column b1 b2 b3 b4 b5 b6 =
     (a1 == b1)
     && (a2 == b2)
     && (abs (a3 - b3) <= 0.001)
     && (a4 == b4)
     && (a5 == b5)
     && (a6 == b6)

instance Eq Region where
 Region a1 a2 a3 a4 a5 a6 a7 a8 == Region b1 b2 b3 b4 b5 b6 b7 b8 =
     (a1 == b1)
     && (a2 == b2)
     && (a3 == b3)
     && (a4 == b4)
     && (abs (a5 - b5) <= 0.001)
     && (abs (a6 - b6) <= 0.001)
     && (abs (a7 - b7) <= 0.001)
     && (abs (a8 - b8) <= 0.001)

instance Eq Cell where
 Cell a1 a2 == Cell b1 b2 = (a1 == b1) && (a2 == b2)

instance Eq SynapseState where
 Potential == Potential = True
 Actual == Actual = True
 _ == _ = False

instance Eq CellState where
 Active == Active = True
 Inactive == Inactive = True
 Predictive == Predictive = True
 _ == _ = False
