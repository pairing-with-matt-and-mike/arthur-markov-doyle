module Main where

import Data.List
import qualified Data.Map as M
import Text.Regex (splitRegex, mkRegex, subRegex)
import System.Random (getStdRandom, randomR)


--                                                      ngram    choices
data MarkovModel = MarkovModel { markovNgrams :: M.Map [String] [String], markovN :: Int }

main = do
    originalText <- readFile "pg244.txt"
    let model = textToModel 4 originalText
    text <- generateTextFromModel 100 model
    putStrLn text

textToModel :: Int -> String -> MarkovModel
textToModel n = (ngramsToModel n) . (ngramize n) . splitTokens

splitTokens :: String -> [String]
splitTokens t = splitRegex (mkRegex "\\s+") nt where
  nt = normalizePunctuation t

normalizePunctuation :: String -> String
normalizePunctuation t = subRegex (mkRegex "[\".!?,:;]") t " \\0"

ngramize :: Int -> [String] -> [[String]]
ngramize n = (filter (((==) n) . length)) . transpose . (take n) . tails

ngramsToModel :: Int -> [[String]] -> MarkovModel
ngramsToModel n ngrams = MarkovModel (ngramsToModelMap n ngrams) n

ngramsToModelMap :: Int -> [[String]] -> M.Map [String] [String]
ngramsToModelMap n = (M.fromListWith (++)) . (fmap (splitAt (n - 1)))

generateTextFromModel :: Int -> MarkovModel -> IO String
generateTextFromModel size model = do
  start <- choice $ M.keys (markovNgrams model)
  ws <- addWords size model (reverse start)
  return $ joinWords (reverse ws)

addWords :: Int -> MarkovModel -> [String] -> IO [String]
addWords 0 _ ws = return ws
addWords n m ws = do
         word <- nextWord m ws
         addWords (n - 1) m (word:ws)

nextWord :: MarkovModel -> [String] -> IO String
nextWord (MarkovModel m n) ws = do
  let k = reverse $ take (n - 1) ws
  let xs = m M.! k
  choice xs

joinWords :: [String] -> String
joinWords words = concat $ intersperse " " words

choice :: [a] -> IO a
choice l = do
       i <- getStdRandom (randomR (0, (length l) - 1))
       return $ l !! i

-- The cat sat on the mat.
--
-- ^ The
-- The cat
-- cat sat
-- sat on
-- on the
-- the mat
-- mat .

-- I am now!
-- Are you following along?
