module TemporalPooler
( phase1
) where

import           Data.List
-- import           Data.Maybe
import           Flow
import qualified HtmData    as Htm

phase1 :: Htm.Region -> Htm.Column -> [Htm.Cell]
phase1 region column = if column |> columnPredictedInput
    then column |> Htm.cells |> map changeStateIfCellPredictedInput
    else column |> Htm.cells |> map  (\cell -> cell {Htm.cellActiveState = True})

    where
          changeStateIfCellPredictedInput :: Htm.Cell -> Htm.Cell
          changeStateIfCellPredictedInput cell = case region |> Htm.complianceSettings |> Htm.cellLearnStateChange of
              Htm.Compliant -> if cell |> cellPredictedInput
                                   then if region |> Htm.learningOn
                                       then cell {Htm.cellActiveState = True, Htm.cellLearnState = True}
                                       else cell {Htm.cellActiveState = True}
                                   else cell
              Htm.Modified -> if cell |> cellPredictedInput
                                   then if region |> Htm.learningOn
                                       then cell {Htm.cellActiveState = True, Htm.cellLearnState = True}
                                       else cell {Htm.cellActiveState = True, Htm.cellLearnState = False}
                                   else cell {Htm.cellLearnState = False}





          cellPredictedInput :: Htm.Cell -> Bool
          cellPredictedInput cell = Htm.cellPredictiveState cell && (cell |> isInputPredicted |> fst)

          -- Returns True if cell is in predictive state AND has at least one dendrite that is both active AND a sequence segment

          columnPredictedInput :: Htm.Column -> Bool
          columnPredictedInput col = case  col |> Htm.cells |> find cellPredictedInput of
              Nothing -> False
              _       -> True

          -- returns true if cell has at least one dendrite that is both active AND a sequence segment

          isInputPredicted :: Htm.Cell -> (Bool, Bool)
          isInputPredicted cell = cell
              |> Htm.distalDendrites
              |> find (\dd -> Htm.dendrtiteActiveState dd && Htm.sequenceSegment dd)
              |> \dendriteThatPredictedInput -> case dendriteThatPredictedInput of
                    Nothing       -> (False, False)
                    Just dendrite -> if Htm.dendrtiteLearnState dendrite
                                         then (True, True)
                                         else (True, False)
