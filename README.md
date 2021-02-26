# morse_code
Morse code for CPSC 312

**Under Morse.hs, you will find the following:**

**MAIN FUNCTIONS:**
- fileToEng: converts a text file in morse to an english method
- messageTrim: takes a repeating message, and returns a single length of it
- parseSent: breaks down an unparsed sentence into known words through a dictioary tree
- textToEng: takes a morse string, and converts to potential english messages

**HELPER FUNCTIONS:**
- textToString: returns contents of file in a IO string
- morseToMessage: converts a morse message into an unparsed english message
- getMorseChar: converts a morse string into a corresponding english char
- removeFirstMorse: removes a morse character from a string after it has been loaded
- addFirst: appends the first character of a string to the end of a second string
- equalFirst: checks if the first character of two strings equate
- patterMatch: matches a string against another string, and returns true if the first string is repeating in the second string
- findPattern: Takes a string and finds the minimal repeating pattern from a string
- appendFirst: takes a word, and appends it to the front of all sentences provided in a list of strings
- nextWord: finds the next english word in a string given an unparsed string and a dictionary
- canAddLetters: takes a word, and returns true if there is another word that can append letters in order to create another word in a dictionary
- removeNextLetters: removes a number of characters in a string, corresponding to the length in another string





