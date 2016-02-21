module HtmData
( CellState (..)
, SynapseState (..)
, Cell (..)
, DistalSynapse (..)
, ProximalSynapse (..)
, Input  (..)
, InhibitionRadius
, Column (..)
, Region (..)
, Permanence
) where


newtype Permanence = Permanence Double
    deriving {Num}

type Boost = Double

type DesiredLocalActivity = Integer

type InhibitionRadius = Integer

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
                     , desiredLocalActivity  :: DesiredLocalActivity
                     , inhibitionRadius      :: InhibitionRadius
                     }
