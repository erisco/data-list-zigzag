{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The feature of this module is 'ZigZag' and its class instances. It
--  is an abstract data type and can be constructed \/ deconstructed
--  by 'fromList' \/ 'toList' or 'fromDiagonals' \/ 'toDiagonals'. See the
--  associated documentation for more information.
--
module Data.List.ZigZag
  ( diagonals
  , fromDiagonals
  , fromList
  , toDiagonals
  , toList
  , ZigZag()
  )
  where
--

import Control.Applicative
  ( Alternative(empty, (<|>))
  , Applicative(pure, (<*>))
  )
--

import Control.Monad
  ( ap
  , join
  , Monad(return, (>>=))
  , MonadPlus(mzero, mplus)
  )
--

import Data.Data
  ( Data
  )
--

import Data.Foldable
  ( concat
  , Foldable
  )
--

import Data.List
  ( transpose
  , unzip
  , (++)
  )
--

import Data.Maybe
  ( catMaybes
  , Maybe(Just, Nothing)
  )
--

import Data.Monoid
  ( Monoid(mappend, mempty)
  )
--

import Data.Semigroup
  ( Semigroup((<>))
  )
--

import Data.Traversable
  ( Traversable
  )
--

import Data.Typeable
  ( Typeable
  )
--

import GHC.Base
  ( Functor(fmap)
  , (.)
  )
--

import GHC.Generics
  ( Generic
  , Generic1
  )
--

import GHC.Read
  ( Read
  )
--

import GHC.Show
  ( Show(show)
  )
--

import Prelude
  ( Eq
  , Ord
  )
--

newtype Diagonal a =
  Diagonal
  { unDiagonal :: [a]
  }
  deriving
  ( Alternative
  , Applicative
  , Data
  , Eq
  , Foldable
  , Functor
  , Generic
  , Generic1
  , Monad
  , MonadPlus
  , Monoid
  , Ord
  , Read
  , Semigroup
  , Show
  , Traversable
  , Typeable
  )
--

-- NOTE: also defined in the "these" package but it has too many
-- irrelevant dependencies.
data These a b =
    This a
  | That b
  | Both a b
--

-- | A list but with a balanced enumeration of Cartesian product such
--  that
--
--  @
--    fmap sum (sequence (replicate n (fromList [0..])))
--  @
--
--  is monotonically increasing.
--
--  Example:
--
--  @
--    sequence [fromList [0,1], fromList [0,1,2]]
--    = fromDiagonals
--      [ [[0,0]]
--      , [[1,0],[0,1]]
--      , [[1,1],[0,2]]
--      , [[1,2]]
--      ]
--  @
--
--  This variation is useful in at least two ways. One, it is not stuck
--  on infinite factors. Two, if the factors are ordered then the
--  product is similarly ordered; this can lend to efficient searching
--  of product elements.
--
--  Note that this method fails for the infinitary product even if every
--  factor is known to be non-empty. The first element is known but
--  following it are infinite elements that each draw a second
--  element from one of the infinite factors. A product element drawing
--  a third factor element is never reached.
--
newtype ZigZag a =
  ZigZag
  { unZigZag :: [Diagonal a]
  }
  deriving
  ( Data
  , Eq
  , Foldable
  , Functor
  , Generic
  , Generic1
  , Ord
  , Read
  , Traversable
  , Typeable
  )
--

instance Alternative ZigZag where
  empty = ZigZag empty
  (<|>) (ZigZag xs) (ZigZag ys) = ZigZag (tie f xs ys)
    where
    f (Both x y) = x <|> y
    f (This x) = x
    f (That y) = y
--

instance Applicative ZigZag where
  pure = return
  (<*>) = ap
--

instance Monad ZigZag where
  return = ZigZag . return . return
  (>>=) (ZigZag xs) f =
    ZigZag (fmap (join . Diagonal) (diagonals (fmap inner xs)))
    where
    inner =
        fmap (Diagonal . concat)
      . transpose
      . unDiagonal
      . fmap (fmap unDiagonal . unZigZag . f)
--

instance MonadPlus ZigZag where
  mzero = empty
  mplus = (<|>)
--

instance Monoid (ZigZag a) where
  mempty = empty
  mappend = (<|>) 
--

instance Semigroup (ZigZag a) where
  (<>) = mappend
--

instance Show a => Show (ZigZag a) where
  show xs = "fromDiagonals " ++ show (toDiagonals xs)
--

-- | Finds the diagonals through a ragged list of lists.
--
--  For example, the diagonals of:
--
--  @
--    [ [0,1,2]
--    , []
--    , [3,4]
--    , [5,6,7]
--    ]
--  @
--
--  Are:
--
--  @
--    [ [0]
--    , [1]
--    , [3,2]
--    , [5,4]
--    , [6]
--    , [7]
--    ]
--  @
--
--  Which can be seen intuitively.
--
--  This algorithm works by storing a list of tails of rows already
--  seen. To find the next diagonal we take the head of the next row
--  plus the head of each stored tail. The tail remainders are stored
--  plus the remainder of the new row.
--
--  If there are no more rows but some remaining tails we then
--  iteratively form diagonals from the heads of each tail until there
--  are no tails remaining.
--
--  Applied to the example:
--
--  @
--    Row     | Output | Remaining
--    --------+--------+----------------
--    [0,1,2] | [0]    | [[1,2]]
--    []      | [1]    | [[2]]
--    [3,4]   | [3,2]  | [[4]]
--    [5,6,7] | [5,4]  | [[6,7]]
--    x       | [6]    | [[7]]
--    x       | [7]    | []
--  @
--
diagonals :: [[a]] -> [[a]]
diagonals = h []
  where
  h rem xxs =
    let (heads, tails) = peel rem
    in  case xxs of
          ((y:ys):xs) -> (y : heads) : h (ys : tails) xs
          ([]:xs) -> heads : h tails xs
          [] ->
            case heads of
              (_:_) -> heads : transpose tails
              [] -> transpose tails
  peel = unzip . catMaybes . fmap uncons
--

-- | Convert a list of diagonals to a ZigZag.
--
--  @
--    fromDiagonals . toDiagonals = id
--    toDiagonals . fromDiagonals = id
--  @
--
fromDiagonals :: [[a]] -> ZigZag a
fromDiagonals = ZigZag . fmap Diagonal

-- | Convert a list to a ZigZag.
--
--  @
--    fromList . toList = id
--    toList . fromList = id
--  @
-- 
fromList :: [a] -> ZigZag a
fromList = ZigZag . fmap (Diagonal . return)

-- | Zips up to the longest list rather than 'GHC.List.zip' which zips
--    up to the shortest list.
--
--  Example:
--
--  @
--    tie id [1] [3,4] = [Both 1 3, That 4]
--  @
--
tie :: (These a b -> c) -> [a] -> [b] -> [c]
tie f (x:xs) (y:ys) = f (Both x y) : tie f xs ys
tie f (x:xs) [] = f (This x) : tie f xs []
tie f [] (y:ys) = f (That y) : tie f [] ys
tie f [] [] = []

-- | Convert a ZigZag to a list of diagonals.
--
--  @
--    fromDiagonals . toDiagonals = id
--    toDiagonals . fromDiagonals = id
--  @
--
toDiagonals :: ZigZag a -> [[a]]
toDiagonals = fmap unDiagonal . unZigZag

-- | Convert a ZigZag to a list.
--
--  @
--    fromList . toList = id
--    toList . fromList = id
--  @
--
toList :: ZigZag a -> [a]
toList = concat . fmap unDiagonal . unZigZag

-- | Undo '(:)'.
--
uncons :: [a] -> Maybe (a, [a])
uncons (x:xs) = Just (x, xs)
uncons [] = Nothing
