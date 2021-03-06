module Text.ExtraCombinators where

{- | Extra parser combinators that are used by this project.
 -
 - * Several JSON style tokenizers (such as symbol, integer, etc.)
 - * Some functions to parse and handle quantifiers (zero-or-more, one-or-more,
 -   etc.)
 - * Some extra helper parser combinators.
 -}

import Control.Monad
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as T

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
identifier     :: Parsec String u String
integer        :: Parsec String u Integer
natural        :: Parsec String u Integer
naturalOrFloat :: Parsec String u (Either Integer Double)
reserved       :: String -> Parsec String u String
stringLiteral  :: Parsec String u String
symbol         :: String -> Parsec String u String
whiteSpace     :: Parsec String u ()

braces         = T.braces         lexer
brackets       = T.brackets       lexer
commaSep       = T.commaSep       lexer
float          = T.float          lexer
identifier     = T.identifier     lexer
integer        = T.integer        lexer
natural        = T.natural        lexer
naturalOrFloat = T.naturalOrFloat lexer
reserved       = T.symbol         lexer
stringLiteral  = T.stringLiteral  lexer
symbol         = T.symbol         lexer
whiteSpace     = T.whiteSpace     lexer

data Quantified a = Once a | Optional a | OneOrMore a | ZeroOrMore a | Range Int Int a deriving Show

-- | Parses a value with optionally a quantifier postfix and return the parsed
--   result encapsulated into a quantifier
quantified :: ParsecT String u Identity a -> ParsecT String u Identity (Quantified a)
quantified p =
    do r <- p
       q <- quantifier
       return $ q r

-- | Parses a quantifier constructor
quantifier :: ParsecT String u Identity (a -> Quantified a)
quantifier =
    option Once $ optionalQ <|> zeroOrMore <|> oneOrMore <|> range
  where
    optionalQ  = symbol "?" >> return Optional
    zeroOrMore = symbol "*" >> return ZeroOrMore
    oneOrMore  = symbol "+" >> return OneOrMore
    range = braces $ do n <- natural
                        let n' = fromIntegral n
                        maybeComma <- optionMaybe $ symbol ","
                        case maybeComma of
                         Nothing -> return $ Range n' n'
                         Just _ -> do m <- natural
                                      let m' = fromIntegral m
                                      when (n > m) (unexpected "n > m in {n,m} quantifier")
                                      return $ Range n' m'

-- | Parse the encapsulated parser in a quantifier, and return a transformed
--   quantifier or Nothing if the quantifier is consumed
parseQuantifier :: Monad m => Quantified (m a) -> m (a, Maybe (Quantified (m a)))
parseQuantifier   (Once       p) = p >>= \x -> return (x, Nothing)
parseQuantifier   (Optional   p) = p >>= \x -> return (x, Nothing)
parseQuantifier q@(ZeroOrMore p) = p >>= \x -> return (x, Just q)
parseQuantifier   (OneOrMore  p) = p >>= \x -> return (x, Just $ ZeroOrMore p)
parseQuantifier   (Range _ 1  p) = p >>= \x -> return (x, Nothing)
parseQuantifier   (Range 0 m  p) = p >>= \x -> return (x, Just $ Range 0     (m-1) p)
parseQuantifier   (Range n m  p) = p >>= \x -> return (x, Just $ Range (n-1) (m-1) p)

-- | Return true if empty string matches the list of quantifiers
matchesEmpty :: [Quantified a] -> Bool
matchesEmpty = all isOptional
  where isOptional (Optional   _) = True
        isOptional (ZeroOrMore _) = True
        isOptional (Range 0 _  _) = True
        isOptional _              = False

-- | Fails if a separator is expected to precede the rest
continueIfSeparated :: Stream s m t => ParsecT s u m b ->
                                       [Quantified (ParsecT s u m a)] ->
                                       ParsecT s u m [a] ->
                                       ParsecT s u m [a]
continueIfSeparated sep rest continuation =
    do maybeSep <- optionMaybe sep
       case maybeSep of
         Just _  -> do s  <- getParserState
                       xs <- continuation
                       when (null xs) $ setParserState s >> unexpected ","
                       return xs
         Nothing -> unless (matchesEmpty rest)
                           (unexpected "end") >>
                    return []

-- | Matches N occurrences of the parser separated by the separator
sepByN :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
sepByN n p sep
    | n <= 0    = return []
    | otherwise = do x <- p
                     xs <- count (n-1) (sep >> p)
                     return (x:xs)

-- | Matches a list of quantified parsers with a separator
quantifiedList :: Stream s m t => ParsecT s u m b ->
                                  [Quantified (ParsecT s u m a)] ->
                                  ParsecT s u m [a]
quantifiedList _   []                  = return []

quantifiedList sep (Once p:ps) =
    do x <- p
       liftM (x:) $ continueIfSeparated sep ps $ quantifiedList sep ps

quantifiedList sep (OneOrMore p:ps) =
    do x <- p
       continueIfSeparated sep ps $ liftM (x:) $ quantifiedList sep (ZeroOrMore p:ps)

quantifiedList sep (Optional p:ps) =
    do maybeR <- optionMaybe p
       case maybeR of
        Nothing -> quantifiedList sep ps
        Just x  -> liftM (x:) $ continueIfSeparated sep ps $ quantifiedList sep ps

quantifiedList sep (ZeroOrMore p:ps) =
    do maybeR <- optionMaybe p
       case maybeR of
        Nothing -> quantifiedList sep ps
        Just x  -> liftM (x:) $ continueIfSeparated sep ps $
                                quantifiedList sep (ZeroOrMore p:ps)

quantifiedList sep (Range 0 0 _:ps) = quantifiedList sep ps
quantifiedList sep (Range 0 m p:ps) = quantifiedList sep (Optional p:Range 0 (m-1) p:ps)
quantifiedList sep (Range n m p:ps)
    | n == m     = do xs <- sepByN n p sep
                      liftM (xs++) $ continueIfSeparated sep ps $ quantifiedList sep ps
    | otherwise  = do xs <- sepByN n p sep
                      liftM (xs++) $ continueIfSeparated sep ps $
                                     quantifiedList sep (Optional p:Range 0 (m-1) p:ps)

