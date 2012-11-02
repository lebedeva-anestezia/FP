module Monstupar
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    , notok, char, oneOf, string
    , many, many1, optional
    --- ...
    ) where

import Monstupar.Core
import Monstupar.Derived
import Monstupar.Tests

main = do print $ runParser natList "10,20,0,1,111551"			
