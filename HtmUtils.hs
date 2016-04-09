module HtmUtils (getRnd) where

import           System.Random
import           Flow

getRnd :: (Eq a, Random a) => StdGen -> [a] -> a -> a -> a
getRnd stdGen disallowed minVal maxVal =  rnd |> (\x ->
    if x `notElem` disallowed
        then x
        else rnd)
    where rnd = head $ randomRs (minVal, maxVal) stdGen
