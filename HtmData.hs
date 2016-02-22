module HtmData
( CellState (..)
, SynapseState (..)
, Cell (..)
, DistalSynapse (..)
, ProximalSynapse (..)
, Input  (..)
, Column (..)
, Region (..)
, Permanence
, Boost
, LocalActivity
, InhibitionRadius
, Overlap
) where

type Permanence = Double

type Boost = Double

type LocalActivity = Integer

type InhibitionRadius = Integer

type Overlap = Double

data CellState = Active
               | Predictive
               | Inactive

data SynapseState = Potential
                  | Actual

data Input = On | Off

data Cell = Cell { cellState        :: CellState
                 , proximalSynapses :: [ProximalSynapse]
                 }

data Column = Column { cells          :: [Cell]
                     , distalSynapses :: [DistalSynapse]
                     , boost          :: Double
                     , key            :: Integer
                     }

data DistalSynapse = DistalSynapse { dInput        :: Input
                                   , dOwner        :: Column
                                   , dSynapseState :: SynapseState
                                   , dPermanence   :: Permanence
                                   }
data ProximalSynapse = ProximalSynapse { pInput        :: Input
                                       , pOwner        :: Cell
                                       , pSynapseState :: SynapseState
                                       , pPermanence   :: Permanence
                                       }

data Region = Region { columns               :: [Column]
                     , previouslyActiveCells :: [Cell]
                     , desiredLocalActivity  :: LocalActivity
                     , inhibitionRadius      :: InhibitionRadius
                     , minimumOverlap        :: Overlap
                     , permanenceInc         :: Permanence
                     , permanenceDec         :: Permanence
                     }

instance Eq DistalSynapse where
 DistalSynapse a1 a2 a3 a4 == DistalSynapse b1 b2 b3 b4 =
     (a1 == b1) && (a2 == b2) && (a3 == b3) && (abs (a4 - b4) <= 0.001)

instance Eq ProximalSynapse where
 ProximalSynapse a1 a2 a3 a4 == ProximalSynapse b1 b2 b3 b4 =
     (a1 == b1) && (a2 == b2) && (a3 == b3) && (abs (a4 - b4) <= 0.001)

instance Eq Input where
 On == On = True
 Off == Off = True
 _ == _ = False

instance Eq Column where
 Column a1 a2 a3 a4 == Column b1 b2 b3 b4 =
     (a1 == b1) && (a2 == b2) && (abs (a3 - b3) <= 0.001) && (a4 == b4)

instance Eq Region where
 Region a1 a2 a3 a4 a5 == Region b1 b2 b3 b4 b5 =
     (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (abs (a5 - b5) <= 0.001)


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
