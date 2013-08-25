-- Ex 2. Write a parser that splits a string into a list of words using space characters as separators (use function isSpace). 

import Data.Char

type Word = String

sentence :: String -> [Word]
sentence = undefined

-- returns a word and the rest of input
word :: String -> (Word, String)
word = undefined
                          
main = print $ sentence "Ceci n'est pas une phrase"
