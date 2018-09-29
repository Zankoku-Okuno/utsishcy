module Zipf where

zipf :: [a] -> [a]
zipf xs = concat $ zipWith replicate [len, len-1 .. 1] xs
    where len = length xs