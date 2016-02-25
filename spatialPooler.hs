module SpatialPooler
( overlap
, inhibition
) where

-- The functions exposed by this module representing the phases of spatial pooling
-- work on a per column basis. The spatialPooler function utilizes these
-- on a per region basis.

import           Data.List
import           Data.Maybe
import qualified HtmData    as Htm


-- PHASE 1: OVERLAP
-- The number of distal synapses with that ar active AND connected to the column
-- multiplied by the column's boost value

overlap :: Htm.Column -> Htm.Overlap -> Htm.Overlap
overlap column minOverlap
    | rawOverlap < minOverlap = 0
    | otherwise               =  rawOverlap * Htm.boost column

    where
          -- The number of distal synapses with that ar active AND connected to the column

          rawOverlap :: Htm.Overlap
          rawOverlap = fromInteger $ sum $ map (oneOnInput . Htm.dInput) connectedDistalSynapses :: Double

          -- The number of ALL distal synapses connected to the column

          connectedDistalSynapses :: [Htm.DistalSynapse]
          connectedDistalSynapses = filter connected $ Htm.distalSynapses column

          -- Converts SynapseState to 0 or 1

          oneOnInput :: Htm.Input -> Integer
          oneOnInput Htm.Off = 0
          oneOnInput Htm.On  = 1

          -- Determines whether a synapse is currently connected / Active

          connected :: Htm.DistalSynapse -> Bool
          connected synapse = Htm.dSynapseState synapse == Htm.Actual

-- PHASE 2: INHIBITION
-- The list of columns within the inhibition radius of the column in question
-- whose overlap is larger than 0 and larger than the desired local activity

inhibition :: Htm.Region -> Htm.Column -> [Htm.Column]
inhibition region column = filter isWinner $ neighbours region column
    where
          -- Determines whether the column's overlap is larger than 0 and larger than the desired local activity

          isWinner :: Htm.Column -> Bool
          isWinner c = overlap c (Htm.minimumOverlap region) > 0.0 && overlap c (Htm.minimumOverlap region) >= (fromInteger $ Htm.desiredLocalActivity region :: Double)

-- PHASE 3.1: LEARNING
-- returns a modified column with the permanence of each of its distal syanpses updated

adjustPermanences :: Htm.Region -> Htm.Column -> Htm.Column
adjustPermanences region activeColumn =
    Htm.Column (Htm.cells activeColumn) (modifySynapses $ Htm.distalSynapses activeColumn) (Htm.boost activeColumn) (Htm.key activeColumn) (Htm.pastCycles activeColumn) (Htm.pastOverlapCycles activeColumn)
    where
          -- changes permanence for all synapses in list base on their state

          modifySynapses :: [Htm.DistalSynapse] -> [Htm.DistalSynapse]
          modifySynapses = map changePermanence

          -- changes the value of a synapse based on its state

          changePermanence :: Htm.DistalSynapse -> Htm.DistalSynapse
          changePermanence synapse
            | Htm.dSynapseState synapse == Htm.Actual    = Htm.DistalSynapse (Htm.dInput synapse) (Htm.dSynapseState synapse) (increasePermanence $ Htm.dPermanence synapse)
            | Htm.dSynapseState synapse == Htm.Potential = Htm.DistalSynapse (Htm.dInput synapse) (Htm.dSynapseState synapse) (decreasePermanence $ Htm.dPermanence synapse)

          -- increases permanence based on the region's permanenceInc value

          increasePermanence :: Htm.Permanence -> Htm.Permanence
          increasePermanence permanence = min 1.0 $ permanence + Htm.permanenceInc region

          -- decreases permanence based on the region's permanenceDec value

          decreasePermanence :: Htm.Permanence -> Htm.Permanence
          decreasePermanence permanence = max 0.0 $ permanence - Htm.permanenceDec region

-- returns the column passed to it with its boost value updated

adjustBoost :: Htm.Region -> Htm.Column -> Htm.Column
adjustBoost region column = Htm.Column (Htm.cells column) (Htm.distalSynapses column) updateBoost (Htm.key column) (Htm.pastCycles column) (Htm.pastOverlapCycles column)
    where
          -- 1% of the highest DutyCycle of the column's neighbours' duty cycles

          updateBoost :: Double
          updateBoost
            | activeDutyCycle column > minDutyCycle = 1
            | otherwise                             = Htm.boost column + Htm.boostInc region

          -- 1 if the column's activeDutyCycle is larger then its minDutyCycle
          -- otherwise the column's current boost is increased by a value specified
          -- on per region basis

          minDutyCycle :: Double
          minDutyCycle = 0.01 * foldr (max . activeDutyCycle) 0.0 (neighbours region column)

          --------------------------------------------------------------------------------------------------------------------
          -- Need to ask about this average vs. sum thing !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          --------------------------------------------------------------------------------------------------------------------

          activeDutyCycle :: Htm.Column -> Double
          activeDutyCycle c = (fromInteger $ sum (Htm.values $ Htm.pastCycles c) :: Double) / (fromInteger $ Htm.numOfVals $ Htm.pastCycles c :: Double)



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
