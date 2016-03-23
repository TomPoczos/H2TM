module TemporalPooler
( temporalPooler
) where

import           Data.List
import           Flow
import           FlexibleParallelism
import qualified HtmData    as Htm

temporalPooler :: Htm.Region -> Htm.Region
temporalPooler region = region {Htm.columns = runTemporalPooler}
    where runTemporalPooler :: [Htm.Column]
          runTemporalPooler = Htm.columns region
              |> flexibleParMap (Htm.parallelismMode region) (phase1              region)
              |> flexibleParMap (Htm.parallelismMode region) (phase2              region)
              |> flexibleParMap (Htm.parallelismMode region) (phase3              region)
              |> flexibleParMap (Htm.parallelismMode region) (resetQueuedSynapses region)

phase1 :: Htm.Region -> Htm.Column -> Htm.Column
phase1 region column = if column |> columnPredictedInput
    then column { Htm.cells = column |> Htm.cells |> map changeCellState}
    else column { Htm.cells = column |> Htm.cells |> map changeCellStateUnconditionally}



    where -- changes cell's active state to True unconditionally
          -- changes cell's learn state to False in non-compliant mode

          changeCellStateUnconditionally :: Htm.Cell -> Htm.Cell
          changeCellStateUnconditionally cell =
              case region |> Htm.complianceSettings |> Htm.resetToFalse of
                  Htm.Compliant -> cell {Htm.cellActiveState = True}
                  Htm.Modified  -> cell {Htm.cellActiveState = True, Htm.cellLearnState = False}

          -- If the cell predicted the input its activeState is always set to True
          -- If the dendrite segment selected to determine this is in learnState the cell's learnState is also set to true.
          -- In noncompliant/modified mode if any of the 2 states cannot be set to true it is explicitly set to False.
          -- These actions are not present in the pseudocode that the compliant version follows.

          changeCellState :: Htm.Cell -> Htm.Cell
          changeCellState cell = case region |> Htm.complianceSettings |> Htm.resetToFalse of
              Htm.Compliant -> if cell |> cellPredictedInput
                                   then if (region |> Htm.learningOn) && (cell |> isInputPredicted |> snd)
                                       then cell {Htm.cellActiveState = True, Htm.cellLearnState = True}
                                       else cell {Htm.cellActiveState = True}
                                   else cell
              Htm.Modified  -> if cell |> cellPredictedInput
                                   then if (region |> Htm.learningOn) && (cell |> isInputPredicted |> snd)
                                       then cell {Htm.cellActiveState = True, Htm.cellLearnState = True}
                                       else cell {Htm.cellActiveState = True, Htm.cellLearnState = False}
                                   else cell {Htm.cellActiveState = False, Htm.cellLearnState = False}

          -- Returns True if any of the cell is in predictive state
          -- AND has at least one dendrite that is both active AND a sequence segment

          cellPredictedInput :: Htm.Cell -> Bool
          cellPredictedInput cell = Htm.cellPredictiveState cell && (cell |> isInputPredicted |> fst)

          -- Returns True if any of the column's cells is in predictive state
          -- AND has at least one dendrite that is both active AND a sequence segment

          columnPredictedInput :: Htm.Column -> Bool
          columnPredictedInput col = col |> Htm.cells |> any cellPredictedInput

          -- returns 2 bools
          -- the first one indicates whether any segment predicted the input
          -- the second indicates the learning state of the checked segment
          -- the second bool depends on compliance settings

          isInputPredicted :: Htm.Cell -> (Bool, Bool)
          isInputPredicted cell = case region |> Htm.complianceSettings |> Htm.activeSegmentChoice of

              -- Checks if a dendrite exists that is active and sequenceSegment
              -- if it finds such a segment and it is in learningstate it returns (True, True)
              -- if it finds such a segment and it is NOT in learningstate it returns (True, False)
              -- otherwise if it does not find such a segment it returns (False, False)

              Htm.Compliant ->
                  cell |> Htm.distalDendrites
                       |> find (\dd -> Htm.dendriteActiveState dd && Htm.sequenceSegment dd)
                       |> \dendriteThatPredictedInput ->
                              case dendriteThatPredictedInput of
                                  Nothing       -> (False, False)
                                  Just dendrite -> if Htm.dendrtiteLearnState dendrite
                                                       then (True, True)
                                                       else (True, False)

              -- Checks if a dendrite exists that is active, sequenceSegment and in learningstate.
              -- returns (True, True) if it finds such a segment
              -- Otherwise it checks if a dendrite exists that is active and sequenceSegment
              -- returns (True, False) if it finds such a segment
              -- Otherwise it returns (False, False)

              Htm.Modified  ->
                  cell |> Htm.distalDendrites
                       |> find (\dd -> Htm.dendriteActiveState dd && Htm.sequenceSegment dd && Htm.dendrtiteLearnState dd)
                       |> \dendriteThatPredictedInputWithLearnState ->
                              case dendriteThatPredictedInputWithLearnState of
                                  Just _  -> (True, True)
                                  Nothing -> cell |> Htm.distalDendrites
                                                  |> find (\dd -> Htm.dendriteActiveState dd && Htm.sequenceSegment dd)
                                                  |> \dendriteThatPredictedInput ->
                                                         case dendriteThatPredictedInput of
                                                             Nothing -> (False, False)
                                                             Just _  -> (True, False)

phase2 :: Htm.Region -> Htm.Column -> Htm.Column
phase2 region column = column { Htm.cells = column |> Htm.cells |> map changePredictiveState}
    where changePredictiveState cell =
              if region |> Htm.learningOn
                  then case region |> Htm.complianceSettings |> Htm.resetToFalse of
                      Htm.Compliant -> if cell |> Htm.distalDendrites |> any Htm.dendriteActiveState
                                           then cell {Htm.cellPredictiveState = True} |> queueReinforcements
                                           else cell |> queueReinforcements
                      Htm.Modified  ->  if cell |> Htm.distalDendrites |> any Htm.dendriteActiveState
                                           then cell {Htm.cellPredictiveState = True} |> queueReinforcements
                                           else cell {Htm.cellPredictiveState = False} |> queueReinforcements
                  else case region |> Htm.complianceSettings |> Htm.resetToFalse of
                      Htm.Compliant -> if cell |> Htm.distalDendrites |> any Htm.dendriteActiveState
                                           then cell {Htm.cellPredictiveState = True}
                                           else cell
                      Htm.Modified  -> if cell |> Htm.distalDendrites |> any Htm.dendriteActiveState
                                           then cell {Htm.cellPredictiveState = True}
                                           else cell {Htm.cellPredictiveState = False}

          queueReinforcements :: Htm.Cell -> Htm.Cell
          queueReinforcements cell =
              case getBestMatchingSegment Htm.Prev cell of
                  Nothing       -> cell {Htm.queuedDistalSynapses = selectActive Htm.Current cell}
                  Just dendrite -> cell {Htm.queuedDistalSynapses = selectActive Htm.Current cell ++ Htm.distalSynapses dendrite}

          selectActive :: Htm.AcquisitionTime -> Htm.Cell -> [Htm.DistalSynapse]
          selectActive time cell =
              cell |> Htm.distalDendrites
                   |> concatMap Htm.distalSynapses
                   |> filter (\ds -> case time of
                          Htm.Current -> Htm.dSynapseState ds == Htm.Actual && (ds |> Htm.dOriginatingCell |> Htm.cellActiveState)
                          Htm.Prev    -> Htm.dSynapseState ds == Htm.Actual && (ds |> Htm.dOriginatingCell |> Htm.cellPrevActiveState))

          getBestMatchingSegment :: Htm.AcquisitionTime -> Htm.Cell -> Maybe Htm.DistalDendrite
          getBestMatchingSegment time cell =
             if (getSegmentWithMostActiveSynapses time cell |> getNumOfActiveSynapses time) < Htm.dendriteMinThreshold region
                 then Nothing
                 else Just (getSegmentWithMostActiveSynapses time cell)

          getSegmentWithMostActiveSynapses time col =
             col |> Htm.distalDendrites |> maximumBy (compareByActiveSynapses time)


          compareByActiveSynapses time col1 col2
              | getNumOfActiveSynapses time col1 > getNumOfActiveSynapses time col2 = GT
              | getNumOfActiveSynapses time col1 < getNumOfActiveSynapses time col2 = LT
              | getNumOfActiveSynapses time col1 == getNumOfActiveSynapses time col2 = EQ


          getNumOfActiveSynapses time col = col
              |> Htm.distalSynapses
              |> map (\synapse -> case time of
                     Htm.Current -> Htm.dSynapseState synapse
                     Htm.Prev    -> Htm.dPrevSynapseState synapse)
              |> filter (== Htm.Actual)
              |> length

phase3 :: Htm.Region -> Htm.Column -> Htm.Column
phase3 region column =
    column { Htm.cells = column |> Htm.cells |> map (\cell ->
        cell { Htm.distalDendrites = Htm.distalDendrites cell |> map (\dendrite ->
            dendrite { Htm.distalSynapses = dendrite |> Htm.distalSynapses |> map (\synapse ->
                if Htm.cellLearnState cell
                    then if synapse `elem` Htm.queuedDistalSynapses cell
                        then synapse {Htm.dPermanence = Htm.dPermanence synapse + Htm.permanenceInc region}
                        else synapse {Htm.dPermanence = Htm.dPermanence synapse - Htm.permanenceDec region}
                    else if not $ Htm.cellPredictiveState cell && Htm.cellPrevPredictiveState cell && synapse `elem` Htm.queuedDistalSynapses cell
                        then synapse {Htm.dPermanence = Htm.dPermanence synapse - Htm.permanenceDec region}
                        else synapse)})})}

resetQueuedSynapses ::Htm.Region -> Htm.Column -> Htm.Column
resetQueuedSynapses region column =
    column { Htm.cells = column |> Htm.cells |> map (\cell ->
        cell {Htm.queuedDistalSynapses = []})}
