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

module Main where

import           Data.List
import qualified Data.Text as Text
import           Data.Tuple.Select
import           FlexibleParallelism
import           Flow
import qualified HtmData             as Htm
import           HtmInit
import           System.IO

main :: IO ()
main = do
    instrHandle    <- openFile "instructions.txt" ReadMode
    instrContent   <- hGetContents instrHandle
    putStrLn instrContent
    hClose instrHandle
    settingsPath   <- getLine
    settings       <- readFile settingsPath
    region         <- settings |> lines |> setup |> sel1 |> return
    learning       <- settings |> lines |> setup |> sel2 |> return
    trainingFileH  <- settings |> lines |> setup |> sel3 |> flip openFile ReadMode
    testingFileH   <- settings |> lines |> setup |> sel4 |> flip openFile ReadMode
    trainingString <- hGetContents trainingFileH
    trainingData   <- processData trainingString |> return

    -- trainingData <- processData trainingString



    putStr settings
    hClose trainingFileH
    --cols <- hGetLine :: Integer settings

trainRegion :: Htm.Region -> [[Htm.Input]] -> Htm.Region
trainRegion region input =
    input map (\timePoint -> region )

processData :: String -> [[Htm.Input]]
processData dataString =
    dataString |> Text.pack
               |> Text.lines
               |> map (Text.splitOn (Text.pack ","))
               |> map (map (\element ->
                          case Text.unpack element of
                              "1" -> Htm.On
                              "0" -> Htm.Off))

setup :: [String] -> (Htm.Region, Bool, String, String)
setup (cols:cells:psyn:dDend:dSyn:permThreshold:learning:permCompl:boostComp:parallelism:trainingFile:testingFile:[]) =
    case learning of
        "True"  -> (createInitialRegion, True, trainingFile, testingFile)
        "False" -> (createInitialRegion, False, trainingFile, testingFile)

    where createInitialRegion = htmInit (read cols :: Integer)
                (read cells :: Integer)
                (read psyn :: Integer)
                (read dDend :: Integer)
                (read dSyn :: Integer)
                (read permThreshold :: Double)
                (readComplianceOption permCompl)
                (readComplianceOption boostComp)
                (readParallelismMode parallelism)

          readComplianceOption :: String -> Htm.ComplianceOption
          readComplianceOption "Compliant" = Htm.Compliant
          readComplianceOption "Modified"  = Htm.Modified

          readParallelismMode :: String -> ParallelismMode
          readParallelismMode "None" = None
          readParallelismMode "Agressive" = Agressive
          readParallelismMode numOfThreads = Limited (read numOfThreads :: Int)

          readLearningMode :: String -> Bool
          readLearningMode "True" = True
          readLearningMode "False" = False
