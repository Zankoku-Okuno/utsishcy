module OldLang where

data Language = L
    { readers :: [(String, [(String, String)])]
    , writers :: [(String, [(String, String)])]
    }
    deriving (Read, Show)
