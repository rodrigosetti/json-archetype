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
lexer = T.makeTokenParser javaStyle { T.reservedNames = ["string",
                                                         "number",
                                                         "object",
                                                         "array",
                                                         "boolean",
                                                         "any",
                                                         "true",
                                                         "false",
                                                         "null"] }

braces         :: Parsec String u a -> Parsec String u a
brackets       :: Parsec String u a -> Parsec String u a
commaSep       :: Parsec String u a -> Parsec String u [a]
float          :: Parsec String u Double
integer        :: Parsec String u Integer
naturalOrFloat :: Parsec String u (Either Integer Double)
reserved       :: String -> Parsec String u String
stringLiteral  :: Parsec String u String
symbol         :: String -> Parsec String u String

braces         = T.braces         lexer
brackets       = T.brackets       lexer
commaSep       = T.commaSep       lexer
float          = T.float          lexer
integer        = T.integer        lexer
naturalOrFloat = T.naturalOrFloat lexer
reserved       = T.symbol         lexer
stringLiteral  = T.stringLiteral  lexer
symbol         = T.symbol         lexer

-- | Basic JSON validator: parses a JSON and returns unit, so we're interested
--   only in validation (i.e. whether or not the input is correct).
type JSONValidator = Parsec String () ()

jsonValue :: JSONValidator
jsonValue = jsonArray               <|>
            jsonObject              <|>
            void (reserved "true" ) <|>
            void (reserved "false") <|>
            void (reserved "null" ) <|>
            void stringLiteral      <|>
            void naturalOrFloat

jsonObject :: JSONValidator
jsonObject =
    void $ braces $ commaSep pair
  where
    pair = stringLiteral >> symbol ":" >> jsonValue

jsonArray :: JSONValidator
jsonArray = void $ brackets $ commaSep jsonValue

-- | The Archetype Parser parses archetype JSON (the augmented JSON) and returns
--   a JSON validator
type ArchetypeParser = Parsec String () JSONValidator

archetype :: ArchetypeParser
archetype =
    do spaces
       o <- object <|> typeObject
       eof
       return $ jsonDocument o
  where
    jsonDocument o = void $ spaces >> o >> eof

value :: ArchetypeParser
value = object       <|>
        list         <|>
        string       <|>
        number       <|>
        word "true"  <|>
        word "false" <|>
        word "null"  <|>
        typeString   <|>
        typeNumber   <|>
        typeObject   <|>
        typeArray    <|>
        typeBoolean  <|>
        typeAny

typeString :: ArchetypeParser
typeString = try (reserved "string") >> return (void stringLiteral)

typeNumber :: ArchetypeParser
typeNumber = try (reserved "number") >> return (void naturalOrFloat)

typeBoolean :: ArchetypeParser
typeBoolean = try (reserved "boolean") >>
              return (void $ reserved "true" <|> reserved "false")

typeArray :: ArchetypeParser
typeArray = try (reserved "array") >> return jsonArray

typeObject :: ArchetypeParser
typeObject = try (reserved "object") >> return jsonObject

typeAny :: ArchetypeParser
typeAny = try (reserved "any") >> return jsonValue

word :: String -> ArchetypeParser
word w = try (reserved w) >> return (void $ reserved w)

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
    brackets (commaSep value) >>= return . jsonList
  where
    jsonList = void . brackets . sequence_ . intersperse (void $ symbol ",")

object :: ArchetypeParser
object =
    do pairs <- braces $ commaSep parsePair
       return (void $ symbol "{" >> jsonPairs (M.fromList pairs))
  where
    parsePair = do k <- stringLiteral
                   _ <- symbol ":"
                   v <- value
                   return (k, v)
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
        Just (archetypeFile, inputs) -> do contents <- readFile archetypeFile
                                           case parse archetype archetypeFile contents of
                                             Left e -> print e
                                             Right p -> validateFiles p inputs
  where
    validateFiles  parser [] = getContents >>= validateSource parser "input"
    validateFiles  parser inputs = forM_ inputs $ \f -> readFile f >>= validateSource parser f
    validateSource parser name contents = case parse parser name contents of
                                            Left e -> print e
                                            Right _ -> putStrLn $ name ++ ": success"


