# Data.List.ZigZag

The feature of this module is ZigZag and its class instances. It is an
abstract data type and can be constructed / deconstructed by
fromList / toList or fromDiagonals / toDiagonals. See the associated
documentation for more information.

# diagonals :: [[a]] -> [[a]]

Finds the diagonals through a ragged list of lists.

For example, the diagonals of:

    [ [0,1,2]
    , []
    , [3,4]
    , [5,6,7]
    ]

Are:

    [ [0]
    , [1]
    , [3,2]
    , [5,4]
    , [6]
    , [7]
    ]
 
Which can be seen intuitively.

This algorithm works by storing a list of tails of rows already seen. To
find the next diagonal we take the head of the next row plus the head of
each stored tail. The tail remainders are stored plus the remainder of
the new row.

If there are no more rows but some remaining tails we then iteratively
form diagonals from the heads of each tail until there are no tails
remaining.

Applied to the example:

| Row       | Output   | Remaining   |
|-----------|----------|-------------|
| `[0,1,2]` | `[0]`    | `[[1,2]]`   |
| `[]`      | `[1]`    | `[[2]]`     |
| `[3,4]`   | `[3,2]`  | `[[4]]`     |
| `[5,6,7]` | `[5,4]`  | `[[6,7]]`   |
| x         | `[6]`    | `[[7]]`     |
| x         | `[7]`    | `[]`        |

# fromDiagonals :: [[a]] -> ZigZag a

Convert a list of diagonals to a ZigZag.

    fromDiagonals . toDiagonals = id
    toDiagonals . fromDiagonals = id

# fromList :: [a] -> ZigZag a

Convert a list to a ZigZag.

    fromList . toList = id
    toList . fromList = id

# toDiagonals :: ZigZag a -> [[a]]

Convert a ZigZag to a list of diagonals.

    fromDiagonals . toDiagonals = id
    toDiagonals . fromDiagonals = id

# toList :: ZigZag a -> [a]

Convert a ZigZag to a list.

    fromList . toList = id
    toList . fromList = id

# ZigZag

A list but with a balanced enumeration of Cartesian product such that

    fmap sum (sequence (replicate n (fromList [0..])))
 
is monotonically increasing.

Example:

    sequence [fromList [0,1], fromList [0,1,2]]
    = fromDiagonals
      [ [[0,0]]
      , [[1,0],[0,1]]
      , [[1,1],[0,2]]
      , [[1,2]]
      ]
 
This variation is useful in at least two ways. One, it is not stuck on
infinite factors. Two, if the factors are ordered then the product is
similarly ordered; this can lend to efficient searching of product
elements.

Note that this method fails for the infinitary product even if every
factor is known to be non-empty. The first element is known but
following it are infinite elements that each draw a second element from
one of the infinite factors. A product element drawing a third factor
element is never reached.
