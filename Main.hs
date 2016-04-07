module Main where

import System.IO
import           Data.List
import           Flow
import           FlexibleParallelism
import HtmInit
import qualified HtmData    as Htm

main :: IO ()
main = do
    instrHandle <- openFile "instructions.txt" ReadMode
    instrContent <- hGetContents instrHandle
    putStrLn instrContent
    hClose instrHandle
    settingsPath <- getLine
    settings <- readFile settingsPath


    putStr settings

    --cols <- hGetLine :: Integer settings

initRegion :: [String] -> Htm.Region
initRegion (cols:cells:psyn:dDend:dSyn:permThreshold:learning:permCompl:boostComp:parallelism) =
    htmInit (read cols :: Integer)
            (read cells :: Integer)
            (read psyn :: Integer)
            (read dDend :: Integer)
            (read dSyn :: Integer)
            (read permThreshold :: Double)
            (readComplianceOption permCompl)
            (readComplianceOption boostComp)
            (readParallelismMode parallelism)
    where readComplianceOption "Compliant" = Htm.Compliant
          readComplianceOption "Modified"  = Htm.Modified
