import Data.Text (Text)
import MorseBST
import Trie


-- Takes in the file specified from the user, returns the contents of the file
import System.IO


-- Main function for executing translation of text file to parsed english messages
-- Enter fileToEng along with the file name in quotations to return the english unparsed
-- message from the original morse code
-- Takes in a filepath, and returns the corresponding string in english

fileToEng :: String -> IO [String]
fileToEng s =
    do 
        file <- readFile "ShortDictionary.txt"
        let ws = words file
        let dictionary = loadDict ws Trie.Empty

        morse <- textToString s
        let 
            eng = morseToMessage morse
            unparsed = reverse (messageTrim (reverse eng))
            separated = parseSent unparsed dictionary
        return (filter (`validSentence` dictionary) separated) 

textToEng :: String -> IO [String]
textToEng s =
    do
        file <- readFile "ShortDictionary.txt"
        let ws = words file
        let dictionary = loadDict ws Trie.Empty

        let 
            eng = morseToMessage s
            uparsed = reverse (messageTrim (reverse eng))
            separated = parseSent uparsed dictionary
        return (filter (`validSentence` dictionary) separated)
            

-- FUNCTIONS FOR CONVERTING MORSE STRING FILE TO AN ENGLISH MESSAGE

-- Takes in a file specified by the user, returns the english translation of the morse code
-- rawMessage :: String -> String
textToString :: String -> IO String
textToString s =
    do 
        handle <- openFile s ReadMode
        contents <- hGetContents handle
        return contents


-- Helper Function for converting morse string to Text String
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


-- FUNCTIONS FOR TRIMMING A REPEATING MESSAGE DOWN TO A SINGLE LENGTH OF IT


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



-- FUNCTIONS FOR PARSING A SENTENCE INTO RECOGNIZABLE WORDS, AND PRESENTS ALL POTENTIAL MESSAGES

-- Helper Function: Breaks down a sentence into potentially logical sentences, and returns a list of strings
parseSent :: String -> DicTrie Char Bool -> [String]
parseSent [] _ = [[]]
parseSent (h:t) dic = 
    foldr (\x lst -> (lst ++ (appendFirst x (parseSent (removeNextLetters x (h:t)) dic)))) [] (allFirstWords [] (h:t) dic)
              
-- TODO: ISSUE WITH RECOGNIZING PERIODS HAS BEEN INTRODUCED
-- Helper Function finds all potential words at the front of the current string
allFirstWords :: [Char] -> [Char] -> DicTrie Char Bool -> [[Char]]
allFirstWords ret (h:t) dic
    | ret == [] = (allFirstWords c (h:t) dic)
    | a && b = (allFirstWords c (h:t) dic) ++ [ret]
    | otherwise = [ret]
    where
        a = isWord ret dic
        b = canAddLetters ret (removeNextLetters ret (h:t)) dic
        c = nextWord ret (removeNextLetters ret (h:t)) dic

-- Helper Function: Given a word, and a list of different strings, appends the word along with a space in between to every string in a given list
appendFirst :: String -> [String] -> [String]
appendFirst [] _ = []
appendFirst _ [] = []
appendFirst s lst =
    map ((s ++ " ") ++) lst


-- Helper Function: Takes an original string (can be an empty array), a string, and returns the first matching word in that string
nextWord :: [Char] -> [Char] -> DicTrie Char Bool -> [Char]
nextWord ret [] dic = ret
nextWord ret (h:t) dic
    | h == '.' = (ret ++ ". ")
    | isWord (ret ++ [h]) dic = (ret ++ [h])
    | otherwise = nextWord (ret ++ [h]) t dic

-- Helper Function: Takes a word, the diciontary, and checks if additional letters can be added to make another word
canAddLetters :: [Char] -> [Char] -> DicTrie Char Bool -> Bool
canAddLetters _ [] _ = False
canAddLetters ret (h:t) dic
    | elem '.' ret = False
    | isWord (ret ++ [h]) dic = True
    | otherwise = canAddLetters (ret ++ [h]) t dic

-- Helper Function: takes a string, and another string, and removes a length of characters from the first string from the second string
removeNextLetters :: String -> String -> String
removeNextLetters s1 s2 = 
    drop (length s1) s2

-- Helper Function: Checks translated sentence for mistakes
validSentence :: [Char] -> DicTrie Char Bool -> Bool
validSentence [] dic = False
validSentence _ Trie.Empty = False
validSentence lst dic
    | last (words lst) == "." = True
    | last (words lst) `isWord` dic = True
    | otherwise = False


<<<<<<< HEAD
=======

test =
    do
        file <- readFile "Dictionary Adjusted.txt"
        let ws = words file
        let dictionary = loadDict ws Trie.Empty

        return (isWord "potato" dictionary)

    
>>>>>>> 677073433e75549ddce66c27d60ec183afa970dd
