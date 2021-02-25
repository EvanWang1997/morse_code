-- CPSC 312 - 2021 - Binary Search Trees in Haskell
-- Copyright D. Poole 2021, released under the GPL.

module MorseBST where

import Data.Char

-- To run it, try:
-- ghci
-- :load MorseBST

-- a binary search tree 
data MorseBST v = Empty
                | Node v (MorseBST v) (MorseBST v)

morseTree :: MorseBST Char
morseTree = Node ' '
     (Node 'e'
          (Node 'i'
               (Node 's'
                    (Node 'h' (Node '5' Empty Empty) (Node '4' Empty Empty))
                    (Node 'v' Empty (Node '3' Empty Empty)))
               (Node 'u'
                    (Node 'f' Empty Empty)
                    (Node ' ' Empty (Node '2' Empty Empty))))
          (Node 'a'
               (Node 'r'
                    (Node 'l' Empty Empty)
                    (Node ' ' Empty Empty))
               (Node 'w'
                    (Node 'p' Empty Empty)
                    (Node 'j' Empty (Node '1' Empty Empty)))))
     (Node 't'
          (Node 'n'
               (Node 'd'
                    (Node 'b' (Node '6' Empty Empty) Empty)
                    (Node 'x' Empty Empty))
               (Node 'k'
                    (Node 'c' Empty Empty)
                    (Node 'y' Empty Empty)))
          (Node 'm'
               (Node 'g'
                    (Node 'z' (Node '7' Empty Empty) Empty)
                    (Node 'q' Empty Empty))
               (Node 'o'
                    (Node ' ' (Node '8' Empty Empty) Empty)
                    (Node ' ' (Node '9' Empty Empty) (Node '0' Empty Empty)))))


-- Function to covert a morse string into a alphanumeric character
morseToEng str (Node v dit dah)
     | str == [] = v
     | (head str) == '.' = morseToEng (tail str) dit
     | (head str) == '-' = morseToEng (tail str) dah

-- Helper Function to find if an alphanumeric character is in the left or right branch
inBranch char Empty  = False
inBranch char (Node v dit dah) =
     char == v || inBranch char dit || inBranch char dah 

-- Helper function for converting strings to lower case equivalents
lowercase :: String -> String
lowercase [] = []
lowercase s =
    map toLower s

-- Function to convert an english sentence into a morse message of this decoding format
engToMorse str =
     foldr (\x lst -> ((charToMorse [] x morseTree) ++ "/") ++ lst) [] (lowercase str)

-- Helper function to convert a single character to a morse character
charToMorse _ '.' _ = "|"
charToMorse _ ' ' _ = ""
charToMorse mstr char (Node v dit dah) 
     | inBranch char dit = charToMorse (mstr ++ ".") char dit
     | inBranch char dah = charToMorse (mstr ++ "-") char dah
     | otherwise = mstr

-- morseToEng "..." morseTree -> 's'

-- tolist tree  returns the list giving the inorder traversal of tree
tolist :: MorseBST v -> [v]
tolist Empty = []
tolist (Node val lt rt) =
     tolist lt ++ (val : tolist rt)
