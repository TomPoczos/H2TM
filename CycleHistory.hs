{-------------------------------------------------------------------------------
H2TM: A Haskell HTM/CLA Implementation
Copyright (C) 2016, Tom Poczos

Developed as part of a Final Year Project at Staffordshire University,
United Kingdom by Tom Poczos under the supervision of Dr. Mohamed Sedky,
based on the Hierarchical Temporal Memory paper released by Numenta Inc.:

http://numenta.com/assets/pdf/whitepapers/hierarchical-temporal-memory-cortical-learning-algorithm-0.2.1-en.pdf

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see http://www.gnu.org/licenses/agpl-3.0
-------------------------------------------------------------------------------}

module CycleHistory
    ( CycleHistory (CycleHistory)
    , activeCycle
    , add
    ) where

import           Flow

data CycleHistory = CycleHistory { values    :: [Bool]
                                 , numOfVals :: Int
                                 , maxAmount :: Int
                                 } deriving (Show)

instance Eq CycleHistory where
    CycleHistory a1 a2 a3 == CycleHistory b1 b2 b3 =
        a1 == b1 && a2 == b2 && a3 == b3

add :: Bool -> CycleHistory -> CycleHistory
add val ch
    | numOfVals ch < maxAmount ch = CycleHistory (val : values ch)
                                                 (numOfVals ch + 1)
                                                 (maxAmount ch)
    | otherwise                   = CycleHistory (val : init (values ch))
                                                 (numOfVals ch)
                                                 (maxAmount ch)

activeCycle :: CycleHistory -> Double
activeCycle ch = fromIntegral (values ch !> filter (== True) !> length) / fromIntegral (numOfVals ch)
