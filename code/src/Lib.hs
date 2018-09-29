{-# LANGUAGE ImplicitParams #-}
module Lib where

import Data.Maybe
import Control.Arrow

import Showy
import Util
import Alphabet
import Zipf

import qualified OldLang as Old


data Language = L
    { readers :: [(String, AlphabetRead)]
    , writers :: [(String, AlphabetShow)]
    }
    deriving (Read, Show)


loadOld :: FilePath -> IO Old.Language
loadOld = (read <$>) . readFile

upgrade :: Old.Language -> Language
upgrade (Old.L r w) = L
    { readers = second (second Seg <$>) <$> r
    , writers = second (first Seg <$>) <$> w
    }

save :: FilePath -> Language -> IO ()
save fname = writeFile fname . show

load :: FilePath -> IO Language
load = (read <$>) . readFile


fromAscii :: Language -> String -> IO [Segment]
fromAscii lang = chooseIo . trim . readAlphabet (fromJust $ lookup "ascii" $ readers lang)
    where
    trim xs =
        let len = minimum (length <$> xs)
        in filter (\x -> length x == len) xs

toIpa :: Language -> [Segment] -> String
toIpa _ = concat . (showy <$>)

toRoman :: Language -> [Segment] -> String
toRoman lang = showAlphabet (fromJust $ lookup "roman" $ writers lang)