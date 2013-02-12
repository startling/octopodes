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

data Octree a
  = Leaf a
  | Branch (Octree a) (Octree a) (Octree a) (Octree a)
    (Octree a) (Octree a) (Octree a) (Octree a)
  deriving (Eq, Show, Ord, Typeable)

-- | A bitraversal over the leaves and the self-similar children
-- of some @'Octree' a@.
nodes :: Applicative f => (a -> f b) ->
  (Octree a -> f (Octree b)) -> Octree a -> f (Octree b)
nodes fn _ (Leaf a) = Leaf <$> fn a
nodes _ fn (Branch a b c d e f g h) = Branch <$> fn a <*> fn b
  <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h

instance Traversable Octree where
  traverse = nodes <*> traverse

instance Foldable Octree where
  foldMap = foldMapDefault

instance Functor Octree where
  fmap = fmapDefault

-- path
-- reduce
-- recreate
