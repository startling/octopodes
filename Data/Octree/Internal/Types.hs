{-# Language DeriveDataTypeable #-}
module Data.Octree.Internal.Types where
-- base
import Data.Typeable

-- | An enumerated types representing four directions.
data Quadrant
  = Northeast
  | Northwest
  | Southeast
  | Southwest
  deriving (Eq, Show, Ord, Typeable)

-- | An enumerated type representing eight directions.
data Octant = Near Quadrant | Far Quadrant
  deriving (Eq, Show, Ord, Typeable)

-- | An @'Octree' a@ can be reduced to a list of insertions.
data Operation a = Insert [Octant] a
  deriving (Eq, Show, Ord, Typeable)

data Octree a 
  = Leaf a
  | Branch
  { _nne, _nnw, _nse, _nsw
  , _fne, _fnw, _fse, _fsw :: Octree a
  } deriving (Eq, Show, Ord, Typeable)
