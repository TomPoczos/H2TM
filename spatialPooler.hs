module SpatialPooler
( updateOverlap
, setActiveState
, spatialPooler
) where

import           Data.List
import           Data.Maybe
import           Flow
import qualified CycleHistory as Ch
import qualified HtmData      as Htm


spatialPooler :: Htm.Region -> Htm.Region
spatialPooler region =
    Htm.Region (Htm.columns                                    region)
               (Htm.columns region
                    |> map (updateOverlap $ Htm.minimumOverlap region)
                    |> map (setActiveState region)
                    |> map (adjustPermanences region)
                    |> map (boostColumn region)
                    |> map (boostPermanences region))
               (Htm.desiredLocalActivity                       region)
               (Htm.inhibitionRadius                           region)
               (Htm.minimumOverlap                             region)
               (Htm.permanenceInc                              region)
               (Htm.permanenceDec                              region)
               (Htm.boostInc                                   region)
               (Htm.permanenceThreshold                        region)
               (Htm.operationMode                              region)
    --

-- PHASE 1: OVERLAP
-- The number of distal synapses with that ar active AND connected to the column
-- multiplied by the column's boost value

updateOverlap :: Htm.Overlap -> Htm.Column -> Htm.Column
updateOverlap minOverlap column
    | rawOverlap < minOverlap = Htm.Column (Htm.cells              column)
                                           (Htm.proximalSynapses   column)
                                           (Htm.boost              column)
                                           0
                                           (Htm.key                column)
                                           (Htm.dutyCycles         column)
                                           (column |> Htm.overlapCycles |> Ch.add False)
                                           (Htm.columnState        column)

    | otherwise               = Htm.Column (Htm.cells              column)
                                           (Htm.proximalSynapses   column)
                                           (Htm.boost              column)
                                           (rawOverlap * Htm.boost column)
                                           (Htm.key                column)
                                           (Htm.dutyCycles         column)
                                           (column |> Htm.overlapCycles |> Ch.add True)
                                           (Htm.columnState        column)

    where
          -- The number of distal synapses with that ar active AND connected to the column

          rawOverlap :: Htm.Overlap
          rawOverlap = fromInteger $ sum $ map (oneOnInput . Htm.pInput) connectedProximalSynapses :: Double

          -- The number of ALL distal synapses connected to the column

          connectedProximalSynapses :: [Htm.ProximalSynapse]
          connectedProximalSynapses = filter connected $ Htm.proximalSynapses column

          -- Converts SynapseState to 0 or 1

          oneOnInput :: Htm.Input -> Integer
          oneOnInput Htm.Off = 0
          oneOnInput Htm.On  = 1

          -- Determines whether a synapse is currently connected / Active

          connected :: Htm.ProximalSynapse -> Bool
          connected synapse = Htm.pSynapseState synapse == Htm.Actual

-- PHASE 2: INHIBITION
-- The list of columns within the inhibition radius of the column in question
-- whose overlap is larger than 0 and larger than the desired local activity

setActiveState :: Htm.Region -> Htm.Column -> Htm.Column
setActiveState region column
    | isWinner column =
        Htm.Column (Htm.cells             column)
                   (Htm.proximalSynapses  column)
                   (Htm.boost             column)
                   (Htm.overlap           column)
                   (Htm.key               column)
                   (column |> Htm.dutyCycles |> Ch.add True)
                   (Htm.overlapCycles     column)
                   Htm.ActiveColumn
    | not $ isWinner column =
        Htm.Column (Htm.cells             column)
                   (Htm.proximalSynapses  column)
                   (Htm.boost             column)
                   (Htm.overlap           column)
                   (Htm.key               column)
                   (column |> Htm.dutyCycles |> Ch.add False)
                   (Htm.overlapCycles     column)
                   Htm.InactiveColumn

          -- Determines whether the column's overlap is larger than 0 and
          -- larger than the kth highest overlap value of its neighbours
          -- where k is the desiredLocalActivity of the region/column

    where isWinner :: Htm.Column -> Bool
          isWinner c = Htm.overlap c > 0.0
                     && Htm.overlap c >=  Htm.overlap (head $ drop (fromInteger (Htm.desiredLocalActivity region - 1) :: Int) $ sortBy compareOverlaps $ neighbours region c)

          compareOverlaps columnA columnB
              | Htm.overlap columnA < Htm.overlap columnB                = LT
              | Htm.overlap columnA > Htm.overlap columnB                = GT
              | abs (Htm.overlap columnA - Htm.overlap columnB) <= 0.001 = EQ

-- PHASE 3.1: LEARNING
-- returns a modified column with the permanence of each of its distal syanpses updated

adjustPermanences :: Htm.Region -> Htm.Column -> Htm.Column
adjustPermanences region activeColumn
    | Htm.columnState activeColumn == Htm.InactiveColumn = activeColumn
    | Htm.columnState activeColumn == Htm.ActiveColumn   =
        Htm.Column (Htm.cells                             activeColumn)
                   (modifySynapses $ Htm.proximalSynapses activeColumn)
                   (Htm.boost                             activeColumn)
                   (Htm.overlap                           activeColumn)
                   (Htm.key                               activeColumn)
                   (Htm.dutyCycles                        activeColumn)
                   (Htm.overlapCycles                     activeColumn)
                   (Htm.columnState                       activeColumn)
    where
          -- changes permanence for all synapses in list base on their state

          modifySynapses :: [Htm.ProximalSynapse] -> [Htm.ProximalSynapse]
          modifySynapses = map changePermanence

          -- changes the value of a synapse based on its state

          changePermanence :: Htm.ProximalSynapse -> Htm.ProximalSynapse
          changePermanence synapse
            | Htm.pSynapseState synapse ==
                Htm.Actual    = Htm.ProximalSynapse (Htm.pInput                           synapse)
                                                    (Htm.pSynapseState                    synapse)
                                                    (increasePermanence $ Htm.pPermanence synapse)
            | Htm.pSynapseState synapse ==
                Htm.Potential = Htm.ProximalSynapse (Htm.pInput                           synapse)
                                                    (Htm.pSynapseState                    synapse)
                                                    (decreasePermanence $ Htm.pPermanence synapse)

          -- increases permanence based on the region's permanenceInc value

          increasePermanence :: Htm.Permanence -> Htm.Permanence
          increasePermanence permanence = min 1.0 $ permanence + Htm.permanenceInc region

          -- decreases permanence based on the region's permanenceDec value

          decreasePermanence :: Htm.Permanence -> Htm.Permanence
          decreasePermanence permanence = max 0.0 $ permanence - Htm.permanenceDec region

-- PHASE 3.2: LEARNING
-- returns the column passed to it with its boost value updated

boostColumn :: Htm.Region -> Htm.Column -> Htm.Column
boostColumn region column = Htm.Column (Htm.cells             column)
                                       (Htm.proximalSynapses  column)
                                       updateBoost
                                       (Htm.overlap           column)
                                       (Htm.key               column)
                                       (Htm.dutyCycles        column)
                                       (Htm.overlapCycles     column)
                                       (Htm.columnState       column)
    where

          -- 1 if the column's activeDutyCycle is larger then its minDutyCycle
          -- otherwise the column's current boost is increased by a value specified
          -- on per region basis

          updateBoost :: Double
          updateBoost
            | (column |> Htm.dutyCycles |> Ch.activeCycle) > minDutyCycle = 1
            | otherwise = Htm.boost column + Htm.boostInc region

          -- 1% of the highest DutyCycle of the column's neighbours' duty cycles

          minDutyCycle :: Double
          minDutyCycle = 0.01 * (column |> neighbours region
                                        |> map (Htm.dutyCycles .> Ch.activeCycle)
                                        |> maximum)

boostPermanences :: Htm.Region -> Htm.Column -> Htm.Column
boostPermanences region column = Htm.Column (Htm.cells         column)
                                            increasePermanences
                                            (Htm.boost         column)
                                            (Htm.overlap       column)
                                            (Htm.key           column)
                                            (Htm.dutyCycles    column)
                                            (Htm.overlapCycles column)
                                            (Htm.columnState   column)

    where increasePermanences :: [Htm.ProximalSynapse]
          increasePermanences = case region |> Htm.operationMode of
              Htm.Compliant ->
                  if (column |> Htm.overlapCycles |> Ch.activeCycle) < minDutyCycle
                      then map increasePermanence (column |> Htm.proximalSynapses)
                      else column |> Htm.proximalSynapses
              Htm.Modified  ->
                  if (column |> Htm.overlapCycles |> Ch.activeCycle) < minOverlapCycle
                      then map increasePermanence (column |> Htm.proximalSynapses)
                      else column |> Htm.proximalSynapses

          increasePermanence synapse =
              Htm.ProximalSynapse (Htm.pInput synapse)
                                  (if 0.1 * (region |> Htm.permanenceThreshold) > (region |> Htm.permanenceThreshold)
                                      then Htm.Actual
                                      else Htm.Potential)
                                  (0.1 * (region |> Htm.permanenceThreshold))

          minDutyCycle :: Double
          minDutyCycle = 0.01 * (column |> neighbours region
                                        |> map (Htm.dutyCycles .> Ch.activeCycle)
                                        |> maximum)

          minOverlapCycle :: Double
          minOverlapCycle = 0.01 * (column |> neighbours region
                                           |> map (Htm.overlapCycles .> Ch.activeCycle)
                                           |> maximum)


-- The list of columns that are within the inhibition radius of the column in question
-- Top level function as it is used during multiple phases

neighbours :: Htm.Region -> Htm.Column -> [Htm.Column]
neighbours region column= filter withinInhibitionRadius $ Htm.columns region
    where
          -- Determines whether the column is within the inhibition radius of the column in question

          withinInhibitionRadius :: Htm.Column -> Bool
          withinInhibitionRadius potentialNeighbor
              | isNothing $ indexOfColumn column            = False
              | isNothing $ indexOfColumn potentialNeighbor = False
              | potentialNeighbor == column                 = False
              -- FromJust can be used here safely as we already now that
              -- "indexOfColumn column" returns "Just Integer"
              | abs  (fromJust (indexOfColumn column) - fromJust (indexOfColumn potentialNeighbor)) <= Htm.inhibitionRadius region = True
              | otherwise = False

          -- Returns the index of column converted from Int to Integer
          -- Returns Nothing if the column is not in the list

          indexOfColumn :: Htm.Column -> Maybe Integer
          indexOfColumn c = case elemIndex c $ Htm.columns region of
              Nothing -> Nothing
              Just index -> Just (toInteger index)
