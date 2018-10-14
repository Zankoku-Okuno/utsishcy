module Lib where

import Data.Maybe
import qualified Data.Map as Map
import Control.Arrow
import Control.Monad
import Control.Lens

import Showy
import Util
import Alphabet
import Zipf
import Language
import qualified OldLang as Old


import Data.IORef
import System.IO.Unsafe


look :: (Language -> a) -> IO a
look f = f <$> readIORef _the

smash :: (Language -> Language) -> IO ()
smash = modifyIORef _the


----------------------
-- The "Tools" Menu --
----------------------

fromAscii :: String -> IO [Segment]
fromAscii str = chooseIo =<< look opts
    where
    opts :: Language -> [[Segment]]
    opts = view $ readers.at "ascii"._Just.to (flip parses str >>> chooseSmallests)
    chooseSmallests xs =
        let len = minimum (length <$> xs)
        in filter (\x -> length x == len) xs

toIpa :: [Segment] -> String
toIpa segments = concat $ showy <$> segments

toRoman :: [Segment] -> IO String
toRoman x = look $ view $ writers.at "roman"._Just.to (flip renders x)


---------------------
-- The "File" Menu --
---------------------

{-# NOINLINE _theFilepath #-}
_theFilepath :: IORef FilePath
_theFilepath = unsafePerformIO $ newIORef (error "no lang loaded")
{-# NOINLINE _the #-}
_the :: IORef Language
_the = unsafePerformIO $ newIORef (error "no lang loaded")


open :: FilePath -> IO ()
open filepath = do
    writeIORef _theFilepath filepath
    writeIORef _the =<< read <$> readFile filepath

save :: IO ()
save = do
    fname <- readIORef _theFilepath
    l <- show <$> readIORef _the
    -- FIXME I definitely need to do an atomic write, not a lazy write
    writeFile fname l

saveAs :: FilePath -> IO ()
saveAs fname = writeIORef _theFilepath fname >> save

upgrade :: FilePath -> IO ()
upgrade filepath = do
    l' <- doUpgrade . read <$> readFile filepath
    writeIORef _theFilepath filepath
    writeIORef _the l'
    where
    doUpgrade (Old.L r w) = L
        { _readers = Map.fromList r
        , _writers = Map.fromList w
        }
