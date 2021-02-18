-- CPSC 312 - 2021 - Binary Search Trees in Haskell
-- Copyright D. Poole 2021, released under the GPL.

module DicTrie where

-- To run it, try:
-- ghci
-- :load DicTrie

-- a binary search tree 
data DicTrie v w = Empty
                | TrieNode v w
                    (DicTrie v w) -- 0
                    (DicTrie v w) -- 1
                    (DicTrie v w) -- 2
                    (DicTrie v w) -- 3
                    (DicTrie v w) -- 4
                    (DicTrie v w) -- 5
                    (DicTrie v w) -- 6
                    (DicTrie v w) -- 7
                    (DicTrie v w) -- 8
                    (DicTrie v w) -- 9
                    (DicTrie v w) -- a
                    (DicTrie v w) -- b
                    (DicTrie v w) -- c
                    (DicTrie v w) -- d
                    (DicTrie v w) -- e
                    (DicTrie v w) -- f
                    (DicTrie v w) -- g
                    (DicTrie v w) -- h
                    (DicTrie v w) -- i
                    (DicTrie v w) -- j
                    (DicTrie v w) -- k
                    (DicTrie v w) -- l
                    (DicTrie v w) -- m
                    (DicTrie v w) -- n
                    (DicTrie v w) -- o
                    (DicTrie v w) -- p
                    (DicTrie v w) -- q
                    (DicTrie v w) -- r
                    (DicTrie v w) -- s
                    (DicTrie v w) -- t
                    (DicTrie v w) -- u
                    (DicTrie v w) -- v
                    (DicTrie v w) -- w
                    (DicTrie v w) -- x
                    (DicTrie v w) -- y
                    (DicTrie v w) -- z
          deriving (Show, Read)

dict = TrieNode ' ' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty

tesTrie = TrieNode ' ' False
     (TrieNode '0' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '1' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '2' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '3' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '4' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '5' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '6' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '7' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '8' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode '9' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'a' True  Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'b' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'c' False (TrieNode 'a' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty
               (TrieNode 't' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
               Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
          Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'd' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'e' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'f' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'g' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'h' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'i' True  Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'j' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'k' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'l' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'm' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'n' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'o' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'p' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'q' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'r' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 's' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 't' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'u' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'v' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'w' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'x' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'y' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
     (TrieNode 'z' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)


-- tolist tree  returns the list giving the inorder traversal of tree
tolist :: DicTrie v w -> [(v, w)]
tolist Empty = []
tolist (TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z) =
     ((val, word) : tolist num0) ++ tolist num1 ++ tolist num2 ++ tolist num3 ++ tolist num4 ++ tolist num5 ++ tolist num6
          ++ tolist num7 ++ tolist num8 ++ tolist num9 ++ tolist a ++ tolist b ++ tolist c ++ tolist d ++ tolist e ++ tolist f
          ++ tolist g ++ tolist h ++ tolist i ++ tolist j ++ tolist k ++ tolist l ++ tolist m ++ tolist n ++ tolist o ++ tolist p
          ++ tolist q ++ tolist r ++ tolist s ++ tolist t ++ tolist u ++ tolist v ++ tolist w ++ tolist x ++ tolist y ++ tolist z

--- Example to try
-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolist atree

--- Defining Equality for Trees:
instance (Eq v,Eq w) => Eq (DicTrie v w) where
   t1 == t2 = tolist t1 == tolist t2

insert str (TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z)
     | str == [] = TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '0' =
          if num0 == Empty then TrieNode val word
               (insert (tail str) (TrieNode '0' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word
               (insert (tail str) num0)
               num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '1' =
          if num1 == Empty then TrieNode val word num0
               (insert (tail str) (TrieNode '1' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0
               (insert (tail str) num1)
               num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '2' =
          if num2 == Empty then TrieNode val word num0 num1
               (insert (tail str) (TrieNode '2' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1
               (insert (tail str) num2)
               num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '3' =
          if num3 == Empty then TrieNode val word num0 num1 num2
               (insert (tail str) (TrieNode '3' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2
               (insert (tail str) num3)
               num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '4' =
          if num4 == Empty then TrieNode val word num0 num1 num2 num3
               (insert (tail str) (TrieNode '4' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3
               (insert (tail str) num4)
               num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '5' =
          if num5 == Empty then TrieNode val word num0 num1 num2 num3 num4
               (insert (tail str) (TrieNode '5' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4
               (insert (tail str) num5)
               num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '6' =
          if num6 == Empty then TrieNode val word num0 num1 num2 num3 num4 num5
               (insert (tail str) (TrieNode '6' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5
               (insert (tail str) num6)
               num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '7' =
          if num7 == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6
               (insert (tail str) (TrieNode '7' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6
               (insert (tail str) num7)
               num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '8' =
          if num8 == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7
               (insert (tail str) (TrieNode '8' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7
               (insert (tail str) num8)
               num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == '9' =
          if num9 == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8
               (insert (tail str) (TrieNode '9' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               a b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8
               (insert (tail str) num9)
               a b c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == 'a' =
          if a == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9
               (insert (tail str) (TrieNode 'a' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               b c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9
               (insert (tail str) a)
               b c d e f g h i j k l m n o p q r s t u v w x y z



-- tolist without append (++), using accumulators
-- tolista :: DicTrie v w -> [(v, w)]
-- tolista lst =
--     tolist2 lst []

-- tolist2 tree lst   returns the the list of elements of tree followed by the elements of lst
-- tolist2 :: BSTree k v -> [(k,v)] -> [(k,v)]
-- tolist2 Empty acc = acc
-- tolist2 (Node val word lt rt) acc =
--      tolist2 lt  ((val, word) : tolist2 rt acc)

--- Example to try
-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolista atree


-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolista (insert 6 "six" atree)

-- what if we wanted to return the old value of key as well as the tree?
-- what if there wasn't an old value; what should be returned? (It has to be of the correct type!)
