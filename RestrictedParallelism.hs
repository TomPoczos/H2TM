module RestrictedParallelism(restrictedParMap) where

import qualified Control.Monad.Par as Par
import Data.List.Split

-- maps the function to the list parallely in a way so that only the specified number of threads is created

-- Implementation:
-- list is divided into chunks of requierd size
-- the function is "parMapped" to every sublist and the result retrieved
-- this ensures that the number of threads created is not larger than the size of sublists
-- this action of "parMapping" is mapped to every sublist (yeah, gets a bit complicated )
-- the resulting list of sublists is concatenated and returned

restrictedParMap :: Par.NFData a => Int -> (a -> a) ->[a] -> [a]
restrictedParMap num fun list = concatMap (Par.runPar . Par.parMap fun) $ chunksOf num list
