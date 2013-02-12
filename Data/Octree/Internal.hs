{-# Language DeriveDataTypeable #-}
{-# Language TemplateHaskell #-}
module Data.Octree.Internal where
-- base
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.Typeable
import Control.Applicative

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

-- Octree
-- path
-- reduce
-- recreate
