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
    | otherwise =  rawOverlap * Htm.boost column

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
inhibition region column = filter isWinner neighbours
    where
          -- Determines whether the column's overlap is larger than 0 and larger than the desired local activity

          isWinner :: Htm.Column -> Bool
          isWinner c = overlap c (Htm.minimumOverlap region) > 0.0 && overlap c (Htm.minimumOverlap region) >= (fromInteger $ Htm.desiredLocalActivity region :: Double)

          -- The list of columns that are within the inhibition radius of the column in question

          neighbours :: [Htm.Column]
          neighbours = filter withinInhibitionRadius $ Htm.columns region

          -- Determines whether the column is within the inhibition radius of the column in question

          withinInhibitionRadius :: Htm.Column -> Bool
          withinInhibitionRadius potentialNeighbor
              | isNothing $ indexOfColumn column = False
              | isNothing $ indexOfColumn potentialNeighbor = False
              | potentialNeighbor == column = False
              -- FromJust can be used here safely as we already now that
              -- "indexOfColumn column" returns "Just Integer"
              | abs  (fromJust (indexOfColumn column) - fromJust (indexOfColumn potentialNeighbor)) <= Htm.inhibitionRadius region = True
              | otherwise = False

          -- Returns the index of column converted from Int to Integer
          -- Returns Nothing if the column is not in the list

          indexOfColumn :: Htm.Column ->Maybe Integer
          indexOfColumn c = case elemIndex c $ Htm.columns region of
              Nothing -> Nothing
              Just index -> Just (toInteger index)
