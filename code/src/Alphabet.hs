{-
Serialization and deserialization of phonemes and phones to/from given alphabets.
-}
module Alphabet where

import Data.Maybe
import Data.List
import Showy


newtype Segment = Seg String -- the name of a phoneme
    deriving (Read, Show, Eq)
unSeg (Seg it) = it
instance Showy Segment where
    showy = unSeg


type AlphabetRead = [(String, Segment)] -- TODO segregate pairs into priorities
type AlphabetShow = [(Segment, String)] -- TODO segregate pairs into priorities -- FIXME ([Segment], String)

parses :: AlphabetRead -> String -> [[Segment]]
parses alphabet [] = pure []
parses alphabet input = do
    (pre_prefix, rest) <- prefixes
    let prefix = fromJust $ lookup pre_prefix alphabet
    suffix <- parses alphabet rest
    pure $ prefix : suffix
    where
    prefixes = catMaybes $ (\x -> splitPrefix x input) <$> (fst <$> alphabet)

-- FIXME except: this should be doing something less one-to-one, since affricates&c
renders :: AlphabetShow -> [Segment] -> String
renders alphabet segs = concatMap (\seg -> fromJust $ lookup seg alphabet) segs

splitPrefix :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
splitPrefix prefix str
    | prefix `isPrefixOf` str = Just (prefix, drop (length prefix) str)
    | otherwise = Nothing
