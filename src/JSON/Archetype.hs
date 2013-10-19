module JSON.Archetype (archetype) where

{- | This module exports the "archetype" parser.
 -   It is a Parser that returns another parser (the JSON Validator).
 -}

import Control.Monad
import JSON.Validator
import Text.ExtraCombinators
import Text.Parsec hiding (string)
import Text.Regex.Posix
import qualified Data.Map as M
import qualified Text.RegexAssociationList as R

-- | Helper function to wrap a string into a "full-match" regular expression
wrapRe :: String -> String
wrapRe s = "^" ++ s ++ "$"

-- | The Archetype Parser parses archetype JSON (the augmented JSON).
--   The user state is a namespace, to keep track of defined  identifiers
--   and reuse structures.
type ArchetypeParser a = Parsec String Namespace a

-- | A namespace holds a mapping of identifier names and their JSON validators
type Namespace = M.Map String JSONValidator

-- The archetype JSON document parser: returns a JSON validator
archetype :: ArchetypeParser JSONValidator
archetype =
    do whiteSpace
       o <- many assignment >> (object <|> typeObject)
       eof
       return $ jsonDocument o
  where
    jsonDocument o = spaces >> o >> eof

assignment :: ArchetypeParser JSONValidator
assignment = do n <- identifier
                v <- symbol "=" >> value
                modifyState $ M.insert n v
                return $ return ()

value :: ArchetypeParser JSONValidator
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
        typeAny      <|>
        name

typeString :: ArchetypeParser JSONValidator
typeString = try (reserved "string") >> return (void stringLiteral)

typeNumber :: ArchetypeParser JSONValidator
typeNumber = try (reserved "number") >> return (void naturalOrFloat)

typeBoolean :: ArchetypeParser JSONValidator
typeBoolean = try (reserved "boolean") >>
              return (void $ reserved "true" <|> reserved "false")

typeArray :: ArchetypeParser JSONValidator
typeArray = try (reserved "array") >> return jsonArray

typeObject :: ArchetypeParser JSONValidator
typeObject = try (reserved "object") >> return jsonObject

typeAny :: ArchetypeParser JSONValidator
typeAny = try (reserved "any") >> return jsonValue

name :: ArchetypeParser JSONValidator
name = do n <- identifier
          namespace <- getState
          maybe (unexpected n) return $ M.lookup n namespace

word :: String -> ArchetypeParser JSONValidator
word w = try (reserved w) >> return (void $ reserved w)

number :: ArchetypeParser JSONValidator
number =
    liftM jsonNumber naturalOrFloat
  where
    jsonNumber (Left  i) = try $ do i' <- integer
                                    when (i /= i') (unexpected (show i') <?> show i)
    jsonNumber (Right f) = try $ do f' <- float
                                    when (f /= f') (unexpected (show f') <?> show f)

-- | Parses an archetype string, matching regular expression
string :: ArchetypeParser JSONValidator
string =
    liftM jsonString stringLiteral
  where
    jsonString s = try $ do s' <- stringLiteral
                            unless (s' =~ wrapRe s) (unexpected (show s') <?> show s)

-- | Parses an archetype list, handling the quantity qualifiers
list :: ArchetypeParser JSONValidator
list =
    liftM jsonList $ brackets $ commaSep $ quantified value
  where
    jsonList = void . brackets . quantifiedList (symbol ",")

-- | Parses a JSON archetype object, handling the fact that key/values can be
--   valid in any order, and the quantity qualifiers.
object :: ArchetypeParser JSONValidator
object =
    do pairs <- braces $ commaSep parsePair
       return (void $ symbol "{" >> jsonPairs pairs)
  where
    parsePair = do k <- stringLiteral
                   q <- quantifier
                   v <- symbol ":" >> value
                   return (wrapRe k, q v)
    jsonPairs pairs
        | null pairs = symbol "}" >> return []
        | otherwise = do k <- stringLiteral
                         case R.lookup k pairs of
                            Nothing -> unexpected k
                            Just q -> do (x, maybeQ) <- symbol ":" >> parseQuantifier q
                                         let pairs' = maybe (R.delete k pairs)
                                                            (\q'-> R.replace k q' pairs)
                                                            maybeQ
                                         let values = map snd pairs'
                                         xs <- continueIfSeparated (symbol ",") values $
                                                liftM (x:) $ jsonPairs pairs'
                                         when (null xs) $ void $ symbol "}"
                                         return xs

