{-------------------------------------------------------------------------------
H2TM: A Haskell HTM/CLA Implementation
Copyright (C) 2015-2016, Tom Poczos

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
along with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0
-------------------------------------------------------------------------------}

module FlexibleParallelism
( flexibleParMap
, ParallelismMode(..)
, NumOfThreads
) where

import qualified Control.Monad.Par as Par
import Data.List.Split

type NumOfThreads = Int

data ParallelismMode  = Agressive | Limited NumOfThreads | None

instance Eq ParallelismMode where
    Agressive == Agressive = True
    None == None = True
    Limited num1 == Limited num2 = num1 == num2
    _ == _ = False

-- maps the function to the list parallely in a way so that only the specified number of threads is created

-- Implementation of the limited option:
-- list is divided into chunks of required size
-- the function is "parMapped" to every sublist and the result retrieved
-- this ensures that the number of threads created is not larger than the size of sublists
-- this action of "parMapping" is mapped to every sublist (yeah, gets a bit complicated )
-- the resulting list of sublists is concatenated and returned

flexibleParMap :: Par.NFData a => ParallelismMode -> (a -> a) ->[a] -> [a]
flexibleParMap Agressive              fun list = Par.runPar $ Par.parMap fun list
flexibleParMap (Limited numOfThreads) fun list = concatMap (Par.runPar . Par.parMap fun) $ chunksOf numOfThreads list
flexibleParMap None                   fun list = map fun list
