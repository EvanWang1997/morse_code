-- CPSC 312 - 2021 - Binary Search Trees in Haskell
-- Copyright D. Poole 2021, released under the GPL.

module Trie where

-- To run it, try:
-- ghci
-- :load DicTrie

-- a binary search tree 
data DicTrie v w = Empty
                | TrieNode v w (DicTrie v w) (DicTrie v w)
          deriving (Show, Read)


tesTrie = TrieNode '0' True Empty
     (TrieNode '1' True Empty
     (TrieNode '2' True Empty
     (TrieNode '3' True Empty
     (TrieNode '4' True Empty
     (TrieNode '5' True Empty
     (TrieNode '6' True Empty
     (TrieNode '7' True Empty
     (TrieNode '8' True Empty
     (TrieNode '9' True Empty
     (TrieNode 'a' True Empty
     (TrieNode 'b' False Empty
     (TrieNode 'c' False
          (TrieNode 'a' False
               (TrieNode 't' True Empty Empty) Empty)
     (TrieNode 'd' False Empty
     (TrieNode 'e' False Empty
     (TrieNode 'f' False Empty
     (TrieNode 'g' False Empty
     (TrieNode 'h' False Empty
     (TrieNode 'i' True  Empty
     (TrieNode 'j' False Empty
     (TrieNode 'k' False Empty
     (TrieNode 'l' False Empty
     (TrieNode 'm' False Empty
     (TrieNode 'n' False Empty
     (TrieNode 'o' False Empty
     (TrieNode 'p' False Empty
     (TrieNode 'q' False Empty
     (TrieNode 'r' False Empty
     (TrieNode 's' False Empty
     (TrieNode 't' False Empty
     (TrieNode 'u' False Empty
     (TrieNode 'v' False Empty
     (TrieNode 'w' False Empty
     (TrieNode 'x' False Empty
     (TrieNode 'y' False Empty
     (TrieNode 'z' False Empty Empty)))))))))))))))))))))))))))))))))))


-- tolist tree  returns the list giving the inorder traversal of tree
tolist :: DicTrie v w -> [(v, w)]
tolist Empty = []
tolist (TrieNode val word child sibling) =
     ((val, word) : tolist child) ++ tolist sibling

--- Example to try
-- tolist tesTrie

--- Defining Equality for Trees:
instance (Eq v,Eq w) => Eq (DicTrie v w) where
   t1 == t2 = tolist t1 == tolist t2

-- Insert a word into the Trie
insert :: Eq a => [a] -> DicTrie a Bool -> DicTrie a Bool
insert [] (TrieNode val word child sibling) = TrieNode val word child sibling
insert str Empty = TrieNode (head str) (null (tail str)) (if null (tail str) then Empty else insert (tail str) Empty) Empty
insert str (TrieNode val word child sibling)
     | null str = TrieNode val word child sibling
     | head str /= val = TrieNode val word child (insert str sibling)
     | head str == val = TrieNode val word (if null (tail str) then child else insert (tail str) child) sibling

-- given a list of words, insert them in the Trie
loadDict :: Eq a => [[a]] -> DicTrie a Bool -> DicTrie a Bool
loadDict [] d = d
loadDict lst d =
     insert (last lst) (loadDict (init lst) d)

-- returns True if the string is a word found in the Trie, False otherwise
isWord :: Eq a => [a] -> DicTrie a Bool -> Bool
isWord [] d = False
isWord _ Empty = False
isWord str (TrieNode val word child sibling)
     | head str /= val = isWord str sibling
     | head str == val = if null (tail str) then word else isWord (tail str) child