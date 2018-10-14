module Language where

import Control.Lens
import Data.Map (Map)

import Alphabet


data Language = L
    { _readers :: Map String AlphabetRead
    , _writers :: Map String AlphabetShow
    }
    deriving (Read, Show)
makeLenses ''Language