
import Data.Text (Text)
import MorseBST()
import MorseBST(morseTree)
import MorseBST(morseToEng)


-- Takes in the file specified from the user, returns the contents of the file
import System.IO


-- Enter fileToEng along with the file name in quotations to return the english unparsed
-- message from the original morse code
-- Takes in a filepath, and returns the corresponding string in english
fileToEng :: String -> IO String
fileToEng s =
    do 
        morse <- textToString s
        let 
            eng = morseToMessage morse
            unparsed = reverse (messageTrim (reverse eng))
        return unparsed 
            

-- Takes in a file specified by the user, returns the english translation of the morse code
-- rawMessage :: String -> String
textToString :: String -> IO String
textToString s =
    do 
        handle <- openFile s ReadMode
        contents <- hGetContents handle
        return contents


-- Function for converting morse string to Text String
morseToMessage :: String -> String
morseToMessage "" = ""
morseToMessage (h:t)
    | h == '/' = morseToMessage t
    | h == '|' = ". " ++ morseToMessage t
    | otherwise = morseToEng (getMorseChar (h:t)) morseTree : morseToMessage((removeFirstMorse (h:t)))

-- Helper Function for converting getting first set of morse characters
getMorseChar :: [Char] -> [Char]
getMorseChar "" = ""
getMorseChar (h:t)
    | (elem h ".-") = h:getMorseChar t
    | otherwise = []

-- Helper Function for removing first morse character in a list
removeFirstMorse "" = ""
removeFirstMorse (h:t)
    | (elem h ".-") = removeFirstMorse t
    | otherwise = (h:t)


-- Takes in english message of unknown repetition, and returns a single length of it
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







    






    