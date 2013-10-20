module Main where

import Control.Monad
import JSON.Archetype
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Text.Parsec
import qualified Data.Map as M

-- | The command line option type
data CmdOption = OptHelp | OptArchetypeFile FilePath

-- | Read from command line parameters, parse the JSON archetype and them get
--   the resulting JSON validator and uses it to validate the JSON input files
--   (or from standard input if none is provided).
main :: IO ()
main =
    do args <- getArgs
       let (options, inputFiles, errors) = getOpt Permute optDescriptions args
       mapM_ putStrLn errors
       case options of
        (OptArchetypeFile f:_) -> readFile f >>=
                                  either print (flip validateFiles inputFiles) .
                                  (runParser archetype M.empty f)
        _ -> do progName <- getProgName
                putStrLn $ usageInfo ("Usage: " ++ progName ++ " <options> [file [file ...]]") optDescriptions
  where
    validateFiles  parser [] = getContents >>= validateSource parser "input"
    validateFiles  parser inputs = forM_ inputs $ \f -> readFile f >>= validateSource parser f
    validateSource parser source contents = either print (const $ putStrLn $ source ++ ": valid") $
                                                   parse parser source contents
    optDescriptions = [Option "h" ["help"] (NoArg OptHelp) "Display help information",
                       Option "a" ["archetype"] (ReqArg OptArchetypeFile "FILENAME") "The JSON archetype file name"]


