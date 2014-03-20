module Space where

--------------------------------------------------------------------------------

type Seq a = (Integer, Integer -> a)

single :: a -> Seq a
single x = (1, \0 -> x)

combine :: [Seq a] -> Seq a
combine []           = (0, undefined)
combine [p]          = p
combine ((n1,h1):ps) = (n1+n2, \i -> if i < n1 then h1 i else h2 (i-n1))
 where
  (n2,h2) = combine ps

--------------------------------------------------------------------------------

type Space a = Int -> Seq a

empty :: Space a
empty = \_ -> combine []

unit :: a -> Space a
unit x = \s -> if s == 0 then single x else combine []

(+++) :: Space a -> Space a -> Space a
f +++ g = \s -> combine [ f s, g s ]

(***) :: Space a -> Space b -> Space (a,b)
f *** g = \s -> combine [ (n1*n2, \i -> (h1 (i `mod` n1), h2 (i `div` n1)))
                        | k <- [0..s]
                        , let (n1,h1) = f k
                              (n2,h2) = g (s-k)
                        ]

pay :: Space a -> Space a
pay f = \s -> if s == 0 then combine [] else f (s-1)

--------------------------------------------------------------------------------


