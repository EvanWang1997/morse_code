-- This module contains functions for a more convenient UI for users to access the functions in this project, allowing for the following:

-- Converting from a morse file to a list of possible english messages
-- Converting an english message, even with repetition, into a morse message
-- Converting a morse message, into a list of possible english messages

import Morse
import MorseBST

-- Main function for starting the interface, and allows user to access options for morse translation
start :: IO [String]
start = 
    do
        putStrLn "Welcome to morse translator, please select from the from the following options:"
        putStrLn "A: convert english to morse"
        putStrLn "B: convert morse to english, and trim the message"
        putStrLn "C: convert a text file's morse into english"
        ans <- getLine

        if ans == "A"
        then askForMorse
        else if ans == "B"
        then askForEng
        else if ans == "C"
        then askForFile
        else return ["Come back when you have something you actually want translated"]

-- Helper function for converting an inputted string to its morse equivalents
askForMorse :: IO [String]
askForMorse = 
    do
        putStrLn "Please enter a string for conversion to morse, alphanumeric, spaces, and periods are accepted"
        ans <- getLine
        return [(engToMorse ans)]

-- Helper function for converting
askForEng :: IO [String]
askForEng = 
    do
        putStrLn "Please enter the morse string for conversion, it must following the convention of this decoder"
        ans <- getLine
        (textToEng ans)


askForFile :: IO [String]
askForFile =
    do
        putStrLn "Please enter a text file that you want translated from morse to english"
        ans <- getLine 
        fileToEng ans


