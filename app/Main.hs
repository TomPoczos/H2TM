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

import           Control.Exception
import           Control.Monad.Random
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Text            as Text
import           Data.Tuple.Select
import           Debug.Trace
import           FlexibleParallelism
import qualified HtmData              as Htm
import           HtmInit
import           SpatialPooler
import           System.IO
import           System.Random
import           TemporalPooler

main :: IO ()
main = do
    instrHandle       <- openFile "instructions.txt" ReadMode
    instrContent      <- hGetContents instrHandle
    putStrLn instrContent
    hClose instrHandle
    settingsPath      <- getLine
    settings          <- readFile settingsPath
    stdGen            <- newStdGen
    region            <- evalRandT (settings & lines & setup stdGen & sel1) stdGen
    let learning      =  settings & lines & setup stdGen & sel2
    trainingFileH     <- settings & lines & setup stdGen & sel3 & flip openFile ReadMode
    testingFileH      <- settings & lines & setup stdGen & sel4 & flip openFile ReadMode
    let repetitions   =  settings & lines & setup stdGen & sel5
    trainingString    <- hGetContents trainingFileH
    let trainingData  =  trainingString & processData
    let trainedRegion =  trainRegion region trainingData repetitions & changeLearningState learning
    testingString     <- hGetContents testingFileH
    let testingData   =  testingString & processData
    print $ testRegion trainedRegion testingData
    hClose trainingFileH
    hClose testingFileH

changeLearningState :: Bool -> Htm.Region -> Htm.Region
changeLearningState shouldLearn region = region {Htm.learningOn = shouldLearn}

testRegion :: Htm.Region -> [[Htm.Input]] -> [Double]
testRegion region testingData = timeStepTest testingData [] region
    where timeStepTest :: [[Htm.Input]] -> [Double] -> Htm.Region -> [Double]
          timeStepTest [] results _ = results
          timeStepTest (timeStep:timeSteps) results reg =
              reg {Htm.columns = reg & Htm.columns & map (\column ->
                  column {Htm.proximalSynapses = column & Htm.proximalSynapses & map (\synapse ->
                      synapse{Htm.pInput = timeStep !! Htm.timeStepIndex synapse})})}
              & spatialPooler  & traceStack "TEST - SP"
              & temporalPooler  & traceStack "TEST - TP"
              & timeStepTest timeSteps (noveltyRatio reg : results)

          noveltyRatio :: Htm.Region -> Double
          noveltyRatio reg =
              (reg & Htm.columns
                     & map Htm.cells
                     & filter (all Htm.cellActiveState)
                     & length
                     & fromIntegral :: Double)
              / (reg & Htm.columns & length & fromIntegral :: Double)

trainRegion :: Htm.Region -> [[Htm.Input]] -> Int -> Htm.Region
trainRegion region trainingData reps = train reps region
    where train:: Int -> Htm.Region -> Htm.Region
          train numOfReps reg =
              case traceShow numOfReps numOfReps of
                  0 -> reg
                  _ -> train  (numOfReps - 1) (timeStepTrain trainingData reg)

          timeStepTrain :: [[Htm.Input]] -> Htm.Region -> Htm.Region
          timeStepTrain [] reg = reg
          timeStepTrain (timeStep:timeSteps) reg =
              (reg {Htm.columns = reg & Htm.columns & map (\column ->
                  column {Htm.proximalSynapses = column & Htm.proximalSynapses & map (\synapse ->
                      synapse{Htm.pInput = timeStep !! Htm.timeStepIndex synapse})})})
              & spatialPooler   -- & traceStack ("TRAIN - SP" ++ (show reps))
              & temporalPooler -- & traceStack ("TRAIN - TP" ++ (show reps))
              & timeStepTrain timeSteps



processData :: String -> [[Htm.Input]]
processData dataString =
    dataString & Text.pack
               & Text.lines
               & map (Text.splitOn (Text.pack ","))
               & map (map (\element ->
                          case Text.unpack element of
                              "1" -> Htm.On
                              _   -> Htm.Off))

setup :: StdGen -> [String] -> (RandT StdGen IO Htm.Region, Bool, String, String, Int)
setup stdGen [cols,cells,psyn,dDend,dSyn,timeStepSize,permThreshold
             ,learning,permCompl,boostComp,parallelism,trainingFile
             ,testingFile,learningRepetitions] =
    case learning of
        "True"  -> (htmInit config, True,  trainingFile, testingFile, read learningRepetitions)
        "False" -> (htmInit config, False, trainingFile, testingFile, read learningRepetitions)

    where

        config = HtmConfig {
            numOfColumns           = read cols
            , numOfCells           = read cells
            , pSynapses            = read psyn
            , numOfDDendrites      = read dDend
            , dSynapses            = read dSyn
            , timeStepSize         = read timeStepSize
            , permThreshold        = read permThreshold
            , permChangeCompliance = readComplianceOption permCompl
            , boostDecCompliance   = readComplianceOption boostComp
            , parallelism          = readParallelismMode parallelism }

        readComplianceOption :: String -> Htm.ComplianceOption
        readComplianceOption "Compliant" = Htm.Compliant
        readComplianceOption "Modified"  = Htm.Modified

        readParallelismMode :: String -> ParallelismMode
        readParallelismMode "None"       = None
        readParallelismMode "Agressive"  = Agressive

        readParallelismMode numOfThreads = Limited (read numOfThreads :: Int)
