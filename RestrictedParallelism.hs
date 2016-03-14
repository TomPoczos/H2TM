module RestrictedParallelism
( flexibleParMap
, ParallelismMode(..)
, NumOfThreads) where

import qualified Control.Monad.Par as Par
import Data.List.Split

type NumOfThreads = Int

data ParallelismMode  = Agressive | Limited NumOfThreads | None

-- maps the function to the list parallely in a way so that only the specified number of threads is created

-- Implementation of the limited option:
-- list is divided into chunks of required size
-- the function is "parMapped" to every sublist and the result retrieved
-- this ensures that the number of threads created is not larger than the size of sublists
-- this action of "parMapping" is mapped to every sublist (yeah, gets a bit complicated )
-- the resulting list of sublists is concatenated and returned

flexibleParMap :: Par.NFData a => ParallelismMode -> (a -> a) ->[a] -> [a]
flexibleParMap Agressive fun list              = Par.runPar $ Par.parMap fun list
flexibleParMap (Limited numOfThreads) fun list = concatMap (Par.runPar . Par.parMap fun) $ chunksOf numOfThreads list
flexibleParMap Agressive fun list              = map fun list
