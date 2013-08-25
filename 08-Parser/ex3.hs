-- Ex 3. Generalize the sentence parser from (Ex 2) to take a pluggable parser. The new function is called several and takes as an argument a generic function String->(a, String), which is supposed to parse a string and return the result of type a together with the leftover string. Use it to split a string into a list of numbers.

import Data.Char

type Parser a = String -> (a, String)

several :: Parser a -> String -> [a]
several parser [] = []
several parser str =
  let trimmedStr = dropWhile isSpace str
      (n, str') = parser trimmedStr
  in n:several parser str'

num :: Parser Int
num str = let (numStr, str') = span isDigit str in (read numStr, dropWhile isSpace str')

main = print $ several num "   12 4 128  "
