module HtmUtils (getRnd, getUUID) where

import           Data.Function
import           Data.UUID.Types
import           Data.UUID.V4
import           System.IO.Unsafe
import           System.Random

{-# NOINLINE getRnd #-}
{-# NOINLINE getUUID #-}

getRnd :: (Eq a, Random a) => StdGen -> [a] -> a -> a -> a
getRnd stdGen disallowed minVal maxVal =  rnd & (\x -> if x `notElem` disallowed then x else rnd)
    where rnd = randomR (minVal, maxVal) (unsafePerformIO newStdGen) & fst

getUUID :: UUID
getUUID = unsafePerformIO nextRandom
