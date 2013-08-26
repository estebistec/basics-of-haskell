-- Ex 3. The code below creates a frequency map of words in a given text. Fill in the implementation of indexWords, which counts the frequency of each word in a list of words; and splitWords, which splits a string into (lowercased) words, removing punctuation and newlines in the process. You might want to use the function findWithDefault from Data.Map and the function words from the Prelude. For bonus points try using map and foldl.

import qualified Data.Map as M
import Data.Char (toLower, isAlpha)
import Data.List (sortBy)

type Index = M.Map String Int

-- findWithDefault :: Ord k => a -> k -> Map k a -> a
-- findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'

indexWords ::  Index -> [String] -> Index
indexWords index []     = index
indexWords index (w:ws) = 
  let count = M.findWithDefault 0 w index
  in indexWords (M.insert w (count + 1) index) ws

splitWords :: String -> [String]
splitWords [] = []
splitWords str = 
  let (word, str') = span isAlpha (dropWhile (not . isAlpha) str)
  in (map toLower word):(splitWords (dropWhile (not . isAlpha) str'))

mostFrequent :: [String] -> [(String, Int)]
mostFrequent wrds =
    let index = indexWords M.empty wrds
    in take 9 (sortBy cmpFreq (M.toList index))
  where
    cmpFreq :: (String, Int) -> (String, Int) -> Ordering
    cmpFreq (w1, n1) (w2, n2) = compare n2 n1

main = do
    text <- readFile "moby.txt"
    print $ mostFrequent (splitWords text)
