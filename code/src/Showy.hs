module Showy where

import Data.List


class Showy a where
    showy :: a -> String


instance {-# OVERLAPS #-} Showy String where
    showy = id

instance Showy a => Showy [a] where
    showy = intercalate " " . (showy <$>)

printy :: Showy a => a -> IO ()
printy = putStrLn . showy
