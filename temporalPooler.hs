module TemporalPooler
( phase1
) where

import           Data.List
import           Data.Maybe
import           Flow
import qualified HtmData    as Htm

phase1 column = if column |> columnPredictedInput
    then column |> Htm.cells |> map changeStateToActive
    else column |> Htm.cells |> map changeStateIfCellPredictedInput

    where
          changeStateIfCellPredictedInput :: Htm.Cell -> Htm.Cell
          changeStateIfCellPredictedInput cell = if cell |> cellPredictedInput
              then changeStateToActive cell
              else cell

          cellPredictedInput :: Htm.Cell -> Bool
          cellPredictedInput cell = isPredictiveCell cell && isInputPredicted cell

          -- Unconditionally changes the cell's state to active

          changeStateToActive :: Htm.Cell -> Htm.Cell
          changeStateToActive cell = cell {Htm.cellState = Htm.ActiveCell}

          -- Returns True if cell is in predictive state AND has at least one dendrite that is both active AND a sequence segment

          columnPredictedInput :: Htm.Column -> Bool
          columnPredictedInput col = case  col |> Htm.cells |> find cellPredictedInput of
                  Nothing -> False
                  _       -> True

          -- Returns True if Cell is in predictive state

          isPredictiveCell :: Htm.Cell -> Bool
          isPredictiveCell cell = Htm.cellState cell == Htm.PredictiveCell

          -- returns true if cell has at least one dendrite that is both active AND a sequence segment

          isInputPredicted :: Htm.Cell -> Bool
          isInputPredicted cell = cell
              |> Htm.distalDendrites
              |> any (\dd -> Htm.distalDendriteState dd == Htm.ActiveDendrite && Htm.sequenceSegment dd)
