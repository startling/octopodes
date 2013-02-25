{-# Language DeriveDataTypeable #-}
module Data.Octree.Internal where
-- base
import Data.Foldable (Foldable(..))
import Data.Typeable
import Control.Applicative
-- mtl
import Control.Monad.Writer

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
    { nearne, nearnw, nearse, nearsw
    , farne, farnw, farse, farsw :: Octree a }
  deriving (Eq, Show, Ord, Typeable)

-- | Narrow a branch into a leaf if all of its immediate children
-- are equal leaves.
narrow :: Eq a => Octree a -> Octree a
narrow br@(Branch a b c d e f g h) = if all (\x -> isLeaf x && a == x)
  [b, c, d, e, f, g, h] then a else br where
    -- Tell if an octree is a leaf.
    isLeaf (Leaf _) = True
    isLeaf _ = False

-- | A bitraversal over some @'Octree' a@: if it is just a leaf,
-- the first argument is used; otherwise, the second is used on its
-- immediate self-similar children.
--
-- This takes care to narrow the resulting octree; it may narrow some
-- previously-unnarrowed octrees.
nodes :: (Eq b, Applicative f) => (a -> f b) ->
  (Octree a -> f (Octree b)) -> Octree a -> f (Octree b)
nodes fn _ (Leaf a) = Leaf <$> fn a
nodes _ op (Branch a b c d e f g h) = Branch <$> fn a <*> fn b
  <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    where fn = fmap narrow . op

-- | Traverse the leaves of some @'Octree' a@.
--
-- This takes care to narrow the resulting octree; it may narrow some
-- previously-unnarrowed octrees.
leaves :: (Eq b, Applicative f) =>
  (a -> f b) -> Octree a -> f (Octree b)
leaves = nodes <*> leaves

-- | Create a lens on a child of some @'Octree' a@ out of a
-- list of 'Octant's.
--
-- This lens takes care to narrow the resulting octree; it may not be
-- a legal lens on some previously-unnarrowed octrees.
child :: (Eq a, Functor f) => [Octant] ->
  (Octree a -> f (Octree a)) -> Octree a -> f (Octree a)
child [] f t = f t
child (o : os) f b = fmap narrow . octant o (child os f) . widen $ b where
  -- Widen a leaf into a branch with itself as its children.
  widen a@(Leaf _) = Branch a a a a a a a a
  widen b = b
  -- Turn a single octant into a lens. N.B. this does not cover 'Leaf'
  -- and so is not a total function, but we only use it on something that
  -- has been widened beforehand.
  octant :: Functor f => 
    Octant -> (Octree a -> f (Octree a)) -> Octree a -> f (Octree a)
  octant (Near Northeast) f o = (\x -> o { nearne = x }) <$> f (nearne o)
  octant (Near Northwest) f o = (\x -> o { nearnw = x }) <$> f (nearnw o)
  octant (Near Southeast) f o = (\x -> o { nearse = x }) <$> f (nearse o)
  octant (Near Southwest) f o = (\x -> o { nearsw = x }) <$> f (nearsw o)
  octant  (Far Northeast) f o = (\x -> o { farne = x }) <$> f (farne o)
  octant  (Far Northwest) f o = (\x -> o { farnw = x }) <$> f (farnw o)
  octant  (Far Southeast) f o = (\x -> o { farse = x }) <$> f (farse o)
  octant  (Far Southwest) f o = (\x -> o { farsw = x }) <$> f (farsw o)
  -- TODO: factor this out

-- | From a list of 'Octant's create a lens examining all the leaves in
-- some @'Octree' a@ and replacing them with a single value.
path :: (Eq a, Functor f) => [Octant] ->
  ([a] -> f a) -> Octree a -> f (Octree a)
path os f = child os $ fmap Leaf . f . execWriter . leaves (tell . pure)

-- | From a list of 'Octant's, create a lens examining the leaf value
-- at that path and replacing it with a single value.
path' :: (Eq a, Functor f) => [Octant] ->
  (Maybe a -> f a) -> Octree a -> f (Octree a)
path' os f = path os (f . head') where
  head' [] = Nothing; head' (a : _) = Just a;

-- reduce
-- recreate
