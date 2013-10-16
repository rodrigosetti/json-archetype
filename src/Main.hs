module Main where

import Prelude hiding (null)
import Control.Monad (void, liftM, when)
import Data.Functor.Identity (Identity)
import Data.List (intersperse)
import Text.Parsec hiding (string)
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as T

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
    do vals <- braces (commaSep parsePair)
       return $ jsonObject vals
  where
    parsePair = do k <- stringLiteral
                   _ <- symbol ":"
                   v <- value
                   return $ do k'<- stringLiteral
                               if k' /= k then unexpected (show k) <?> show k'
                               else void (symbol ":" >> v)
    jsonObject = void . braces . sequence_ . intersperse (void (symbol ","))

main :: IO ()
main = do a <- readFile "archetype.json"
          case  parse json "archetype.json" a of
            Left e -> print e
            Right p -> do s <- getContents
                          parseTest p s

