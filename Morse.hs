
import Data.Text (Text)

-- Takes in the file specified from the user, outputs the original an array containing
import System.IO
textToString :: String -> IO String
textToString s =
    do 
        handle <- openFile s ReadMode
        contents <- hGetContents handle
        return contents

-- Function for converting morse string to Text String
-- TODO

-- Takes in english message of unknown repetition, and returns a single length of it !!

messageTrim :: String -> String
messageTrim s = findPattern s []

-- Helper function: takes in two strings, appends the first value of the first string
-- to the end of the second string
addFirst :: [char] -> [char] -> [char]
addFirst (h1:t1) lst2 = 
    lst2 ++ [h1]

-- Helper function: Checks if the first value in the original string equates
-- to the first value in the currently stored string
equalFirst :: String -> String -> Bool
equalFirst (h1:t1) (h2:t2) =
    h1 == h2

-- Helper function: Checks if the pattern repeats for the rest of the string
patternMatch :: String -> String -> Bool
patternMatch _ [] = False
patternMatch lst1 lst2
    | (length lst2) > (length lst1) = lst1 == take (length lst1) lst2
    | otherwise = (lst2 == take (length lst2) lst1) && (patternMatch (drop (length lst2) lst1) lst2)

-- Helper Function: Takes a string and pattern matches it against a larger string
findPattern :: String -> String -> String
findPattern str1 str2
    | patternMatch str1 str2 = str2
    | otherwise = findPattern (tail str1) (addFirst str1 str2)







    






    