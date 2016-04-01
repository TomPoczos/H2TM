module SpatialPooler
( updateOverlap
, setActiveState
, spatialPooler
) where

import           Data.List
import           Data.Maybe
import           Flow
import           FlexibleParallelism
import qualified CycleHistory as Ch
import qualified HtmData      as Htm


spatialPooler :: Htm.Region -> Htm.Region
spatialPooler region = region {Htm.columns = runSpatialPooler}
    |> \r -> r {Htm.inhibitionRadius = newRadius r}

    -- Done in 2 steps to ensure inhibition radius is calculated after everything else
    -- the spatial pooler has to perform has been done. This could probably be done in
    -- one step but it is better to be on the safe side

    where runSpatialPooler :: [Htm.Column]
          runSpatialPooler = Htm.columns region
              |> flexibleParMap (Htm.parallelismMode region) (region |> Htm.minimumOverlap |> updateOverlap)
              |> flexibleParMap (Htm.parallelismMode region) (setActiveState region)
              |> flexibleParMap (Htm.parallelismMode region) (adjustPermanences region)
              |> flexibleParMap (Htm.parallelismMode region) (boostColumn region)
              |> flexibleParMap (Htm.parallelismMode region) (boostPermanences region)

-- Calculates the nwe inhibition radius for the region. Based on Numenta's Matlab
-- implementation rather than the description in Numenta's HTM paper 

newRadius :: Htm.Region -> Integer
newRadius region = (numOfRegionsActiveSynapses / numOfRegionsSynapses) * numOfRegionsInputs
    |> round
    |> min (((region |> Htm.columns |> length) - 1) |> toInteger)

    where numOfRegionsActiveSynapses :: Double
          numOfRegionsActiveSynapses = region |> Htm.columns
                                              |> concatMap Htm.proximalSynapses
                                              |> map Htm.pSynapseState
                                              |> filter (== Htm.Actual)
                                              |> length
                                              |> fromIntegral :: Double

          numOfRegionsSynapses       :: Double
          numOfRegionsSynapses       = region |> Htm.columns
                                              |> concatMap Htm.proximalSynapses
                                              |> length
                                              |> fromIntegral :: Double

          numOfRegionsInputs         :: Double
          numOfRegionsInputs         = region |> Htm.columns
                                              |> concatMap Htm.proximalSynapses
                                              |> map Htm.pInput
                                              |> filter (== Htm.On)
                                              |> length
                                              |> fromIntegral :: Double

-- PHASE 1: OVERLAP
-- The number of distal synapses with that ar active AND connected to the column
-- multiplied by the column's boost value

updateOverlap :: Htm.Overlap -> Htm.Column -> Htm.Column
updateOverlap minOverlap column =
    if rawOverlap < minOverlap
        then column { Htm.overlap       = 0
                    , Htm.overlapCycles = column |> Htm.overlapCycles |> Ch.add False }
        else column { Htm.overlap       = rawOverlap * Htm.boost column
                    , Htm.overlapCycles = column |> Htm.overlapCycles |> Ch.add True }

    where
          -- The number of distal synapses with that ar active AND connected to the column

          rawOverlap :: Htm.Overlap
          rawOverlap = connectedProximalSynapses |> map (Htm.pInput .> oneOnInput)
                                                 |> sum
                                                 |> fromInteger :: Double

          -- The number of ALL distal synapses connected to the column

          connectedProximalSynapses :: [Htm.ProximalSynapse]
          connectedProximalSynapses = column |> Htm.proximalSynapses |> filter connected

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
setActiveState region column =
    if isWinner column
        then column { Htm.dutyCycles  = column |> Htm.dutyCycles |> Ch.add True
                    , Htm.columnState = Htm.ActiveColumn}
        else column { Htm.dutyCycles  = column |> Htm.dutyCycles |> Ch.add False
                    , Htm.columnState = Htm.InactiveColumn}

          -- Determines whether the column's overlap is larger than 0 and
          -- larger than the kth highest overlap value of its neighbours
          -- where k is the desiredLocalActivity of the region/column

    where isWinner :: Htm.Column -> Bool
          isWinner c = Htm.overlap c > 0.0
                    && Htm.overlap c >= (c |> neighbours region
                                           |> sortBy compareOverlaps
                                           |> drop ((Htm.desiredLocalActivity region - 1) |> fromInteger :: Int)
                                           |> head
                                           |> Htm.overlap)

          compareOverlaps :: Htm.Column -> Htm.Column -> Ordering
          compareOverlaps columnA columnB
              | Htm.overlap columnA < Htm.overlap columnB                = LT
              | Htm.overlap columnA > Htm.overlap columnB                = GT
              | abs (Htm.overlap columnA - Htm.overlap columnB) <= 0.001 = EQ

-- PHASE 3.1: LEARNING
-- returns a modified column with the permanence of each of its distal syanpses updated

adjustPermanences :: Htm.Region -> Htm.Column -> Htm.Column
adjustPermanences region activeColumn = case Htm.columnState activeColumn of
    Htm.InactiveColumn -> activeColumn
    Htm.ActiveColumn   -> activeColumn {Htm.proximalSynapses = activeColumn |> Htm.proximalSynapses |> modifySynapses }

    where
          -- changes permanence for all synapses in list base on their state

          modifySynapses :: [Htm.ProximalSynapse] -> [Htm.ProximalSynapse]
          modifySynapses = map changePermanence

          -- changes the value of a synapse based on its state

          changePermanence :: Htm.ProximalSynapse -> Htm.ProximalSynapse
          changePermanence synapse = case Htm.pSynapseState synapse of
              Htm.Actual    -> synapse {Htm.pPermanence = synapse |> Htm.pPermanence |> increasePermanence}
              Htm.Potential -> synapse {Htm.pPermanence = synapse |> Htm.pPermanence |> decreasePermanence}

          -- increases permanence based on the region's permanenceInc value

          increasePermanence :: Htm.Permanence -> Htm.Permanence
          increasePermanence permanence = min 1.0 $ permanence + Htm.permanenceInc region

          -- decreases permanence based on the region's permanenceDec value

          decreasePermanence :: Htm.Permanence -> Htm.Permanence
          decreasePermanence permanence = max 0.0 $ permanence - Htm.permanenceDec region

-- PHASE 3.2: LEARNING
-- returns the column passed to it with its boost value updated

boostColumn :: Htm.Region -> Htm.Column -> Htm.Column
boostColumn region column = column {Htm.boost = updateBoost}

    where

          -- 1 if the column's activeDutyCycle is larger then its minDutyCycle
          -- otherwise the column's current boost is increased by a value specified
          -- on per region basis

          updateBoost :: Double
          updateBoost =
            if (column |> Htm.dutyCycles |> Ch.activeCycle) > minDutyCycle
                then case region |> Htm.complianceSettings |> Htm.boostDecrease of
                    Htm.Compliant -> 1
                    Htm.Modified  -> Htm.boost column - Htm.boostInc region
                else Htm.boost column + Htm.boostInc region

          -- 1% of the highest DutyCycle of the column's neighbours' duty cycles

          minDutyCycle :: Double
          minDutyCycle = 0.01 * (column |> neighbours region
                                        |> map (Htm.dutyCycles .> Ch.activeCycle)
                                        |> maximum)

boostPermanences :: Htm.Region -> Htm.Column -> Htm.Column
boostPermanences region column = column {Htm.proximalSynapses = increasePermanences}
    where increasePermanences :: [Htm.ProximalSynapse]
          increasePermanences = case region |> Htm.complianceSettings |> Htm.permanenceBoost of
              Htm.Compliant -> if (column |> Htm.overlapCycles |> Ch.activeCycle) < minDutyCycle
                                   then Htm.proximalSynapses column |> map increasePermanence
                                   else Htm.proximalSynapses column
              Htm.Modified  -> if (column |> Htm.overlapCycles |> Ch.activeCycle) < minOverlapCycle
                                   then Htm.proximalSynapses column |> map increasePermanence
                                   else Htm.proximalSynapses column

          increasePermanence synapse = synapse { Htm.pSynapseState =
              if increasedPermanence synapse > Htm.permanenceThreshold region
                  then Htm.Actual
                  else Htm.Potential}

          increasedPermanence :: Htm.ProximalSynapse -> Htm.Permanence
          increasedPermanence synapse = Htm.pPermanence synapse + 0.1 * Htm.permanenceThreshold region

          minDutyCycle :: Double
          minDutyCycle    = 0.01 * (column |> neighbours region
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
