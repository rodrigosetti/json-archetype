module Main where

import Control.Monad
import Data.Functor.Identity (Identity)
import Data.List (intersperse)
import Prelude hiding (null)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Text.Parsec hiding (string)
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as T
import qualified Data.Map as M

-- The JSON (Java style) lexer
lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser javaStyle

braces         :: Parsec String u a -> Parsec String u a
brackets       :: Parsec String u a -> Parsec String u a
symbol         :: String -> Parsec String u String
float          :: Parsec String u Double
integer        :: Parsec String u Integer
commaSep       :: Parsec String u a -> Parsec String u [a]
stringLiteral  :: Parsec String u String
naturalOrFloat :: Parsec String u (Either Integer Double)

braces         = T.braces         lexer
brackets       = T.brackets       lexer
symbol         = T.symbol         lexer
float          = T.float          lexer
integer        = T.integer        lexer
commaSep       = T.commaSep       lexer
stringLiteral  = T.stringLiteral  lexer
naturalOrFloat = T.naturalOrFloat lexer

type ArchetypeParser = Parsec String () JSONParser

type JSONParser = Parsec String () ()

json :: ArchetypeParser
json =
    do spaces
       o <- object
       eof
       return $ jsonDocument o
  where
    jsonDocument o = void $ spaces >> o >> eof

value :: ArchetypeParser
value = object       <|>
        list         <|>
        string       <|>
        number       <|>
        boolean      <|>
        null

null :: ArchetypeParser
null =
    symbol "null" >> return jsonNull
  where
    jsonNull = void $ symbol "null"

boolean :: ArchetypeParser
boolean =
    true <|> false
  where
    true = symbol "true" >> return (jsonSymbol "true")
    false = symbol "false" >> return (jsonSymbol "false")
    jsonSymbol = void . symbol

number :: ArchetypeParser
number =
    liftM jsonNumber naturalOrFloat
  where
    jsonNumber (Left i) = do i' <- integer
                             when (i /= i') (unexpected (show i) <?> show i')
    jsonNumber (Right f) = do f' <- float
                              when (f /= f') (unexpected (show f) <?> show f')
string :: ArchetypeParser
string =
    liftM jsonString stringLiteral
  where
    jsonString s = do s' <- stringLiteral
                      when (s /= s') (unexpected (show s) <?> show s')

list :: ArchetypeParser
list =
    do vals <- brackets (commaSep value)
       return $ jsonList vals
  where
    jsonList = void . brackets . sequence_ . intersperse (void $ symbol ",")

object :: ArchetypeParser
object =
    do pairs <- braces (commaSep parsePair)
       return $ jsonObject $ M.fromList pairs
  where
    parsePair = do k <- stringLiteral
                   _ <- symbol ":"
                   v <- value
                   return (k, v)
    jsonObject pairs = symbol "{" >> jsonPairs pairs
    jsonPairs pairs
        | M.null pairs = void $ symbol "}"
        | otherwise = do k <- stringLiteral
                         case M.lookup k pairs of
                            Nothing -> unexpected k
                            Just p -> do _ <- symbol ":"
                                         _ <- p
                                         let pairs' = M.delete k pairs
                                         unless (M.null pairs') $ void $ symbol ","
                                         jsonPairs pairs'

-- | The command line option type
data CmdOption = OptHelp | OptArchetypeFile FilePath

-- | get, from command line arguments, the archetype file path and the list of
--   json file names to validate
getArchetypeAndInputs :: IO (Maybe (FilePath, [FilePath]))
getArchetypeAndInputs =
    do args <- getArgs
       let (options, inputFiles, errors) = getOpt Permute optDescriptions args
       mapM_ putStrLn errors
       case options of
        (OptArchetypeFile s:_) -> return $ Just (s, inputFiles)
        _ -> do progName <- getProgName
                putStrLn $ usageInfo ("Usage: " ++ progName ++ " <options> [file [file ...]]") optDescriptions
                return Nothing
  where
    optDescriptions = [Option "h" ["help"] (NoArg OptHelp) "Display help information",
                       Option "a" ["archetype"] (ReqArg OptArchetypeFile "FILENAME") "The JSON archetype file name"]

main :: IO ()
main =
    do maybeOptions <- getArchetypeAndInputs
       case maybeOptions of
        Nothing -> return ()
        Just (archetype, inputs) -> do contents <- readFile archetype
                                       case parse json archetype contents of
                                         Left e -> print e
                                         Right p -> validateFiles p inputs
  where
    validateFiles  parser [] = getContents >>= validateSource parser "input"
    validateFiles  parser inputs = forM_ inputs $ \f -> readFile f >>= validateSource parser f
    validateSource parser name contents = case parse parser name contents of
                                            Left e -> print e
                                            Right _ -> putStrLn $ name ++ ": success"


