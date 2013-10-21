module JSON.Validator (JSONValidator,
                       jsonValue,
                       sign,
                       jsonObject,
                       jsonArray,
                       jsonNumber) where

-- | Basic JSON validator: parses a JSON and returns unit, so we're interested
--   only in validation (i.e. whether or not the input is correct).

import Control.Monad (void)
import Text.ExtraCombinators
import Text.Parsec

type JSONValidator = Parsec String () ()

jsonValue :: JSONValidator
jsonValue = jsonArray               <|>
            jsonObject              <|>
            void (reserved "true" ) <|>
            void (reserved "false") <|>
            void (reserved "null" ) <|>
            void stringLiteral      <|>
            jsonNumber

sign:: Parsec String u Integer
sign = option 1 (symbol "-" >> return (-1))

jsonNumber :: JSONValidator
jsonNumber = sign >> void naturalOrFloat

jsonObject :: JSONValidator
jsonObject =
    void $ braces $ commaSep pair
  where
    pair = stringLiteral >> symbol ":" >> jsonValue

jsonArray :: JSONValidator
jsonArray = void $ brackets $ commaSep jsonValue

