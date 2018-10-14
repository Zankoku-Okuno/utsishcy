module OldLang where

import Control.Lens

import Alphabet


data Language = L
    { _readers :: [(String, AlphabetRead)]
    , _writers :: [(String, AlphabetShow)]
    }
    deriving (Read, Show)
makeLenses ''Language
