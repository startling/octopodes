{-# Language DeriveDataTypeable #-}
{-# Language TemplateHaskell #-}
module Data.Octree.Internal where
-- base
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.Typeable
import Control.Applicative

-- | A node of an @'Octree' a@; can be either a leaf node with
-- a value hanging on it or a child @'Octree' a@.
data Node a
  = Leaf a
  | Branch (Octree a)
  deriving (Eq, Ord, Show, Typeable)

-- | Traverse both kinds of @'Node' a@.
node :: Functor f
  => (s -> f t)
  -> (Octree s -> f (Octree t))
  -> Node s -> f (Node t)
node f _ (Leaf a) = Leaf <$> f a
node _ g (Branch a) = Branch <$> g a

instance Functor Node where
  fmap = fmapDefault

instance Foldable Node where
  foldMap = foldMapDefault

instance Traversable Node where
  traverse f = node f (traverse f)

-- | Half of an @'Octree' a@, with four nodes corresponding to
-- some four cardinal directions.
data Halftree a = Halftree
  { northeast
  , northwest
  , southeast
  , southwest :: Node a }
  deriving (Eq, Ord, Show, Typeable)

-- | Traverse the @'Node' a@ in a @'Halftree' a@.
nodes :: Applicative f
  => (Node a -> f (Node b))
  -> Halftree a -> f (Halftree b)
nodes f (Halftree a b c d) = Halftree <$> f a <*> f b <*> f c <*> f d

instance Functor Halftree where
  fmap = fmapDefault

instance Foldable Halftree where
  foldMap = foldMapDefault

instance Traversable Halftree where
  traverse = nodes . traverse

-- | A tree with eight nodes hanging on it.
data Octree a = Octree
  { near
  , far :: Halftree a }
  deriving (Eq, Ord, Show, Typeable )

-- | Traverse the @'Halftree' a@ of an @'Octree' a@.
halftrees :: Applicative f
  => (Halftree a -> f (Halftree b))
  -> Octree a -> f (Octree b)
halftrees f (Octree ne fa) = Octree <$> f ne <*> f fa

-- | Traverse the self-similar children of a @'Octree' a@, given
-- a traversal over the leaf nodes.
plateOctree :: Applicative f
  => (a -> f b)
  -> (Octree a -> f (Octree b))
  -> Octree a -> f (Octree b)
plateOctree f = halftrees . nodes . node f

instance Functor Octree where
  fmap = fmapDefault

instance Foldable Octree where
  foldMap = foldMapDefault

instance Traversable Octree where
  traverse = halftrees . traverse

-- | Simplify some @'Octree' a@ into a leaf when all of its
-- children are equal leaves.
shallowSimplify :: Eq t => Octree t -> Node t
shallowSimplify ot = let (n : ns) = oToList ot in
  if all isLeaf (n : ns) && all (== n) ns
    then n else Branch ot where
      isLeaf (Leaf _) = True; isLeaf (Branch _) = False;
      oToList (Octree a b) = hToList a ++ hToList b
      hToList (Halftree a b c d) = [a, b, c, d]

-- | Widen a 'Leaf' into a 'Branch' of equal leaves.
widen :: Node a -> Octree a
widen (Branch b) = b
widen t@(Leaf _) = Octree ht ht where ht = Halftree t t t t

-- | An index into the immediate children of a @'Halftree' a@.
data Quadrant
  = Northeast
  | Northwest
  | Southeast
  | Southwest
  deriving (Eq, Show, Ord, Typeable)

-- | An index into self-similar children of an @'Octree a'@.
data Octant = Near Quadrant | Far Quadrant
  deriving (Eq, Show, Ord, Typeable)
