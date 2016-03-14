module CycleHistory
    ( CycleHistory (CycleHistory)
    , activeCycle
    , add
    ) where

import Flow

data CycleHistory = CycleHistory { values                :: [Bool]
                                 , numOfVals             :: Int
                                 , maxAmount             :: Int
                                 }

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
activeCycle ch = fromIntegral (values ch |> filter (== True) |> length) / fromIntegral (numOfVals ch)
