-- CPSC 312 - 2021 - Binary Search Trees in Haskell
-- Copyright D. Poole 2021, released under the GPL.

module MorseBST where

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


morseToEng str (Node v dit dah)
     | str == [] = v
     | (head str) == '.' = morseToEng (tail str) dit
     | (head str) == '-' = morseToEng (tail str) dah

-- morseToEng "..." morseTree -> 's'

-- tolist tree  returns the list giving the inorder traversal of tree
tolist :: MorseBST v -> [v]
tolist Empty = []
tolist (Node val lt rt) =
     tolist lt ++ (val : tolist rt)

--- Example to try
-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolist atree





-- tolist without append (++), using accumulators
tolista :: MorseBST v -> [v]
tolista lst =
    tolist2 lst []

-- tolist2 tree lst   returns the the list of elements of tree followed by the elements of lst
-- tolist2 :: BSTree k v -> [(k,v)] -> [(k,v)]
tolist2 Empty acc = acc
tolist2 (Node val lt rt) acc =
     tolist2 lt  (val : tolist2 rt acc)

--- Example to try
-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolista atree


-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolista (insert 6 "six" atree)

-- what if we wanted to return the old value of key as well as the tree?
-- what if there wasn't an old value; what should be returned? (It has to be of the correct type!)
