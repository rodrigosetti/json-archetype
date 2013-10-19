module Text.RegexAssociationList (lookup, delete, replace) where

-- | Functions to manipulate association lists indexed by regular expressions

import Prelude hiding (lookup)
import Text.Regex.Posix

lookup :: String -> [(String, a)] -> Maybe a
lookup _ [] = Nothing
lookup x ((re, a):ys)
    | x =~ re   = Just a
    | otherwise = lookup x ys

delete :: String -> [(String, a)] -> [(String, a)]
delete _ [] = []
delete x ((y, a):ys)
    | x =~ y    = ys
    | otherwise = (y, a) : delete x ys

replace :: String -> a -> [(String, a)] -> [(String, a)]
replace _ _ [] = []
replace x b ((y, a):ys)
    | x =~ y    = (y, b) : ys
    | otherwise = (y, a) : replace x b ys

