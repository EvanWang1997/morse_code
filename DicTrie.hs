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
     (TrieNode 'c' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty
          (TrieNode 'a' False Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty
               (TrieNode 't' True Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
               Empty Empty Empty Empty Empty Empty)
          Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
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
     | null str = TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z
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
     | head str == 'b' =
          if b == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a
               (insert (tail str) (TrieNode 'b' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               c d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a
               (insert (tail str) b)
               c d e f g h i j k l m n o p q r s t u v w x y z
     | head str == 'c' =
          if c == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b
               (insert (tail str) (TrieNode 'c' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               d e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b
               (insert (tail str) c)
               d e f g h i j k l m n o p q r s t u v w x y z
     | head str == 'd' =
          if d == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c
               (insert (tail str) (TrieNode 'd' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               e f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c
               (insert (tail str) d)
               e f g h i j k l m n o p q r s t u v w x y z
     | head str == 'e' =
          if e == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d
               (insert (tail str) (TrieNode 'e' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               f g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d
               (insert (tail str) e)
               f g h i j k l m n o p q r s t u v w x y z
     | head str == 'f' =
          if f == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e
               (insert (tail str) (TrieNode 'f' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               g h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e
               (insert (tail str) f)
               g h i j k l m n o p q r s t u v w x y z
     | head str == 'g' =
          if g == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f
               (insert (tail str) (TrieNode 'g' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               h i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f
               (insert (tail str) g)
               h i j k l m n o p q r s t u v w x y z
     | head str == 'h' =
          if h == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g
               (insert (tail str) (TrieNode 'h' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               i j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g
               (insert (tail str) h)
               i j k l m n o p q r s t u v w x y z
     | head str == 'i' =
          if i == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h
               (insert (tail str) (TrieNode 'i' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               j k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h
               (insert (tail str) i)
               j k l m n o p q r s t u v w x y z
     | head str == 'j' =
          if j == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i
               (insert (tail str) (TrieNode 'j' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               k l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i
               (insert (tail str) j)
               k l m n o p q r s t u v w x y z
     | head str == 'k' =
          if k == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j
               (insert (tail str) (TrieNode 'k' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               l m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j
               (insert (tail str) k)
               l m n o p q r s t u v w x y z
     | head str == 'l' =
          if l == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k
               (insert (tail str) (TrieNode 'l' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               m n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k
               (insert (tail str) l)
               m n o p q r s t u v w x y z
     | head str == 'm' =
          if m == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l
               (insert (tail str) (TrieNode 'm' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               n o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l
               (insert (tail str) m)
               n o p q r s t u v w x y z
     | head str == 'n' =
          if n == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m
               (insert (tail str) (TrieNode 'n' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               o p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m
               (insert (tail str) n)
               o p q r s t u v w x y z
     | head str == 'o' =
          if o == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n
               (insert (tail str) (TrieNode 'o' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               p q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n
               (insert (tail str) o)
               p q r s t u v w x y z
     | head str == 'p' =
          if p == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o
               (insert (tail str) (TrieNode 'p' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               q r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o
               (insert (tail str) p)
               q r s t u v w x y z
     | head str == 'q' =
          if q == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p
               (insert (tail str) (TrieNode 'q' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               r s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p
               (insert (tail str) q)
               r s t u v w x y z
     | head str == 'r' =
          if r == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q
               (insert (tail str) (TrieNode 'r' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               s t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q
               (insert (tail str) r)
               s t u v w x y z
     | head str == 's' =
          if s == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r
               (insert (tail str) (TrieNode 's' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               t u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r
               (insert (tail str) s)
               t u v w x y z
     | head str == 't' =
          if t == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s
               (insert (tail str) (TrieNode 't' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               u v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s
               (insert (tail str) t)
               u v w x y z
     | head str == 'u' =
          if u == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t
               (insert (tail str) (TrieNode 'u' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               v w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t
               (insert (tail str) u)
               v w x y z
     | head str == 'v' =
          if v == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u
               (insert (tail str) (TrieNode 'v' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               w x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u
               (insert (tail str) v)
               w x y z
     | head str == 'w' =
          if w == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v
               (insert (tail str) (TrieNode 'w' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               x y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v
               (insert (tail str) w)
               x y z
     | head str == 'x' =
          if x == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w
               (insert (tail str) (TrieNode 'x' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               y z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w
               (insert (tail str) x)
               y z
     | head str == 'y' =
          if y == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x
               (insert (tail str) (TrieNode 'y' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
               z
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x
               (insert (tail str) y)
               z
     | head str == 'z' =
          if z == Empty then TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y
               (insert (tail str) (TrieNode 'z' (null (tail str)) Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty))
          else TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y
               (insert (tail str) z)

loadDict [] d = d
loadDict lst d =
     insert (last lst) (loadDict (init lst) d)

-- isWord [] d = False
isWord str (TrieNode val word num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 a b c d e f g h i j k l m n o p q r s t u v w x y z)
     | null str = word
     | head str == '0' = if num0 == Empty then False else isWord (tail str) num0
     | head str == '1' = if num1 == Empty then False else isWord (tail str) num1
     | head str == '2' = if num2 == Empty then False else isWord (tail str) num2
     | head str == '3' = if num3 == Empty then False else isWord (tail str) num3
     | head str == '4' = if num4 == Empty then False else isWord (tail str) num4
     | head str == '5' = if num5 == Empty then False else isWord (tail str) num5
     | head str == '6' = if num6 == Empty then False else isWord (tail str) num6
     | head str == '7' = if num7 == Empty then False else isWord (tail str) num7
     | head str == '8' = if num8 == Empty then False else isWord (tail str) num8
     | head str == '9' = if num9 == Empty then False else isWord (tail str) num9
     | head str == 'a' = if a == Empty then False else isWord (tail str) a
     | head str == 'b' = if b == Empty then False else isWord (tail str) b
     | head str == 'c' = if c == Empty then False else isWord (tail str) c
     | head str == 'd' = if d == Empty then False else isWord (tail str) d
     | head str == 'e' = if e == Empty then False else isWord (tail str) e
     | head str == 'f' = if f == Empty then False else isWord (tail str) f
     | head str == 'g' = if g == Empty then False else isWord (tail str) g
     | head str == 'h' = if h == Empty then False else isWord (tail str) h
     | head str == 'i' = if i == Empty then False else isWord (tail str) i
     | head str == 'j' = if j == Empty then False else isWord (tail str) j
     | head str == 'k' = if k == Empty then False else isWord (tail str) k
     | head str == 'l' = if l == Empty then False else isWord (tail str) l
     | head str == 'm' = if m == Empty then False else isWord (tail str) m
     | head str == 'n' = if n == Empty then False else isWord (tail str) n
     | head str == 'o' = if o == Empty then False else isWord (tail str) o
     | head str == 'p' = if p == Empty then False else isWord (tail str) p
     | head str == 'q' = if q == Empty then False else isWord (tail str) q
     | head str == 'r' = if r == Empty then False else isWord (tail str) r
     | head str == 's' = if s == Empty then False else isWord (tail str) s
     | head str == 't' = if t == Empty then False else isWord (tail str) t
     | head str == 'u' = if u == Empty then False else isWord (tail str) u
     | head str == 'v' = if v == Empty then False else isWord (tail str) v
     | head str == 'w' = if w == Empty then False else isWord (tail str) w
     | head str == 'x' = if x == Empty then False else isWord (tail str) x
     | head str == 'y' = if y == Empty then False else isWord (tail str) y
     | head str == 'z' = if z == Empty then False else isWord (tail str) z
     | otherwise = False


readcsv =
  do
    file <- readFile "ShortDictionary.txt"
    let ws = words file
    let dictionary = loadDict ws dict
    return (isWord "aer" dictionary)