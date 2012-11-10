module SpellChecker
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
import Data.List
import Data.Maybe

--Разбиваем строку по пробелам, активно используя Monstupar
tokenizer :: String -> [String]
tokenizer str = case runParser wordList str of
		Right (s, a) -> a
		Left e -> error "invalid input"	

levenshtein :: String -> String -> Int
levenshtein s1 s2 = last $ foldl (transform s1) [0..length s1] s2 where
	transform str xs@(x:xs') c = res where
		res = x + 1 : zipWith4 compute str xs xs' res
		compute c' x y z = minimum [y + 1, z + 1, x + (if c' == c then 0 else 1)]

neighbor :: String -> String
neighbor s = res where
	res = dict !! m
	m = fromJust (elemIndex (minimum levls) levls) 
	levls = zipWith levenshtein (replicate (length dict) s) dict		
	dict = ["opa","nya","word","pya"]

--главная функция, проверяет входную строку по словарю dict из функции neighbor

main :: String -> [String] 
main s = map neighbor (tokenizer s)