-- This module contains functions for a more convenient UI for users to access the functions in this project, allowing for the following:

-- Converting from a morse file to a list of possible english messages
-- Converting an english message, even with repetition, into a morse message
-- Converting a morse message, into a list of possible english messages

import Morse


start :: IO [String]
start = 
    do
        putStrLn "Welcome to morse translator, please select from the from the following options:"
        putStrLn "A: convert english to morse"
        putStrLn "B: convert morse to english, and trim the message"
        putStrLn "C: convert a text file's morse into english"
        ans <- getLine

        if ans == "A"
        then return ["hello"]
        else if ans == "B"
        then return ["goodbye"]
        else if ans == "C"
        then return ["working"]
        else return ["nothing"]








