module Main where

import System.Exit
import Control.Monad
import Data.Version (showVersion)
import JSON.Archetype
import Paths_json_archetype (version)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Text.Parsec
import qualified Data.Map as M

-- | The command line option type
data CmdOption = OptHelp | OptVersion | OptArchetypeFile FilePath

-- | Read from command line parameters, parse the JSON archetype and them get
--   the resulting JSON validator and uses it to validate the JSON input files
--   (or from standard input if none is provided).
main :: IO ()
main =
    do args <- getArgs
       let (options, inputFiles, errors) = getOpt Permute optDescriptions args
       unless (null errors) $ forM_ errors putStrLn
       case options of
        (OptVersion:_)         -> putStrLn $ showVersion version
        (OptArchetypeFile f:_) -> do a <- readFile f
                                     case runParser archetype M.empty f a of
                                      Left  e -> print e >> exitWith (ExitFailure 255)
                                      Right p -> do c <- validateFiles p inputFiles
                                                    if c == 0 then exitSuccess else exitWith (ExitFailure c)
        (OptHelp:_) -> do progName <- getProgName
                          putStrLn $ usageInfo ("Usage: " ++ progName ++ " <options> [file [file ...]]") optDescriptions
        _ -> return ()
  where
    validateFiles  parser [] = getContents >>= validateSource parser "input"
    validateFiles  parser inputs = do codes <- forM inputs $ \f -> readFile f >>= validateSource parser f
                                      return $ sum codes
    validateSource parser source contents = case parse parser source contents of
                                             Left  e -> print e >> return 1
                                             Right _ -> putStrLn (source ++ ": valid") >> return 0
    optDescriptions = [Option "h" ["help"] (NoArg OptHelp) "Display help information",
                       Option ""  ["version"] (NoArg OptVersion) "Display program version",
                       Option "a" ["archetype"] (ReqArg OptArchetypeFile "FILENAME") "The JSON archetype file name"]


