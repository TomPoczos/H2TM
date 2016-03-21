module TemporalPooler
( phase1
) where

import           Data.List
-- import           Data.Maybe
import           Flow
import qualified HtmData    as Htm

phase1 :: Htm.Region -> Htm.Column -> [Htm.Cell]
phase1 region column = if column |> columnPredictedInput
    then column |> Htm.cells |> map changeCellState
    else column |> Htm.cells |> map  (\cell -> cell {Htm.cellActiveState = True})


    -- If the cell predicted the input its activeState is always set to True
    -- If the dendrite segment selected to determine this is in learnState the cell's learnState is alsoset to true.
    -- In noncompliant/modified mode if any of the 2 states cannot be set to true it is explicitly set to False.
    -- These actions are not present in the pseudocode that the compliant version follows.

    where changeCellState :: Htm.Cell -> Htm.Cell
          changeCellState cell = case region |> Htm.complianceSettings |> Htm.cellLearnStateChange of
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
          columnPredictedInput col = case  col |> Htm.cells |> find cellPredictedInput of
              Nothing -> False
              _       -> True

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
                       |> find (\dd -> Htm.dendrtiteActiveState dd && Htm.sequenceSegment dd)
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
                       |> find (\dd -> Htm.dendrtiteActiveState dd && Htm.sequenceSegment dd && Htm.dendrtiteLearnState dd)
                       |> \dendriteThatPredictedInputWithLearnState ->
                              case dendriteThatPredictedInputWithLearnState of
                                  Just _  -> (True, True)
                                  Nothing -> cell |> Htm.distalDendrites
                                                  |> find (\dd -> Htm.dendrtiteActiveState dd && Htm.sequenceSegment dd)
                                                  |> \dendriteThatPredictedInput ->
                                                         case dendriteThatPredictedInput of
                                                             Nothing -> (False, False)
                                                             Just _  -> (True, False)
