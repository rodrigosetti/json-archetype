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
archetype = do whiteSpace
               o <- many assignment >> (object <|> typeObject)
               eof
               return (spaces >> o >> eof)

assignment :: ArchetypeParser JSONValidator
assignment = do n <- identifier
                v <- symbol "=" >> value
                modifyState $ M.insert n v
                return $ return ()

value :: ArchetypeParser JSONValidator
value = object       <|>
        array        <|>
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
  where
    typeAny     = try (reserved "any") >> return jsonValue
    typeBoolean = try (reserved "boolean") >>
                  return (void $ reserved "true" <|> reserved "false")
    typeArray   = try (reserved "array") >> return jsonArray
    typeNumber  = try (reserved "number") >> return jsonNumber
    typeString  = try (reserved "string") >> return (void stringLiteral)
    word w      = try (reserved w) >> return (void $ reserved w)

typeObject :: ArchetypeParser JSONValidator
typeObject = try (reserved "object") >> return jsonObject

name :: ArchetypeParser JSONValidator
name = do n <- identifier
          namespace <- getState
          maybe (unexpected n) return $ M.lookup n namespace

number :: ArchetypeParser JSONValidator
number =
    liftM jsonNumberValue integerOrFloat
  where
    integerOrFloat = do s <- sign
                        r <- naturalOrFloat
                        case r of
                            Left i  -> return $ Left  $ i * s
                            Right f -> return $ Right $ f * fromIntegral s
    jsonNumberValue (Left  i) = try $ do i' <- integer
                                         when (i /= i') (unexpected (show i') <?> show i)
    jsonNumberValue (Right f) = try $ do s <- option 1 (symbol "-" >> return (-1))
                                         f' <- float
                                         let signed = f' * s
                                         when (f /= signed) (unexpected (show signed) <?> show f)

-- | Parses an archetype string, matching regular expression
string :: ArchetypeParser JSONValidator
string =
    liftM jsonString stringLiteral
  where
    jsonString s = try $ do s' <- stringLiteral
                            unless (s' =~ wrapRe s) (unexpected (show s') <?> show s)

-- | Parses an archetype array, handling the quantity qualifiers
array :: ArchetypeParser JSONValidator
array =
    liftM quantifiedArray $ brackets $ commaSep $ quantified value
  where
    quantifiedArray = void . brackets . quantifiedList (symbol ",")

-- | Parses a JSON archetype object, handling the fact that key/values can be
--   valid in any order, and the quantity qualifiers.
object :: ArchetypeParser JSONValidator
object =
    liftM (void . braces . jsonPairs) $ braces $ commaSep pair
  where
    pair = do k <- stringLiteral
              q <- quantifier
              v <- symbol ":" >> value
              return (wrapRe k, q v)
    jsonPairs []    = return []
    jsonPairs pairs = do k <- stringLiteral
                         case R.lookup k pairs of
                            Nothing -> unexpected k
                            Just q -> do (x, maybeQ) <- symbol ":" >> parseQuantifier q
                                         let pairs' = maybe (R.delete k pairs)
                                                            (\q'-> R.replace k q' pairs)
                                                            maybeQ
                                             values = map snd pairs'
                                         continueIfSeparated (symbol ",") values $
                                                liftM (x:) $ jsonPairs pairs'

