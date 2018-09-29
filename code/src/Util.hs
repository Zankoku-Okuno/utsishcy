module Util where

import Data.List
import Showy


swap (a, b) = (b, a)

chooseIo :: Showy str => [str] -> IO str
chooseIo [] = error "Error: no options"
chooseIo [one] = pure one
chooseIo opts = do
    putStrLn "Multiple options, pick one:"
    putStrLn `mapM_` zipWith format [1..] opts
    choice <- loop (length opts)
    pure $ opts !! (choice - 1)
    where
    format num opt = concat [" ", show num, ") ", showy opt]
    loop :: Int -> IO Int
    loop max = do
        i <- readLn
        if 1 <= i && i <= max then pure i else putStrLn "Nope." >> loop max