{-# Language DeriveDataTypeable #-}
module Data.Octree.Operations
  ( Operation (..)
  , reduce
  , recreate ) where
 -- base
import Data.Typeable
import Control.Applicative
-- transformers
import Data.Functor.Identity
-- pockets
import Data.Octree.Internal
import Data.Octree.Lens

-- | An @'Octree' a@ can be reduced to a list of insertions.
data Operation a = Insert [Octant] a
  deriving (Eq, Show, Ord, Typeable)

-- | Reduce some @'Octree' a@ into some @['Operation' a]@.
reduce :: Node a -> [Operation a]
reduce (Leaf a) = [Insert [] a]
reduce (Branch (Octree n f)) = reduceHt Near n ++ reduceHt Far f where
  include :: Octant -> Operation a -> Operation a
  include q (Insert qs a) = Insert (q : qs) a
  reduceHt :: (Quadrant -> Octant) -> Halftree a -> [Operation a]
  reduceHt n (Halftree a b c d) =
       map (include $ n Northeast) (reduce a)
    ++ map (include $ n Northwest) (reduce b)
    ++ map (include $ n Southwest) (reduce c)
    ++ map (include $ n Southeast) (reduce d)
-- TODO: optimize this.

-- | Create an @'Octree' a@ from some @['Operation' a]@, given some
-- @a@ to begin with.
recreate :: Eq a => a -> [Operation a] -> Node a
recreate a ls = runIdentity
  $ foldl (>>=) (return $ Leaf a) (map perform ls) where
    perform :: (Eq a, Functor m, Monad m)
      => Operation a -> Node a -> m (Node a)
    perform (Insert ps v) = path ps (const $ return v)
