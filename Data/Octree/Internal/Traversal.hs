module Data.Octree.Internal.Traversal where
-- base
import Data.Foldable (Foldable(..))
import Control.Applicative
-- mtl
import Control.Monad.Writer
-- octopodes
import Data.Octree.Internal.Types
import Data.Octree.Internal.Lens

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
child (o : os) f b = fmap narrow . octant o e (child os f) . widen $ b where
  -- Widen a leaf into a branch with itself as its children.
  widen a@(Leaf _) = Branch a a a a a a a a
  widen b = b
  -- Since we know that  'widen' never produces a 'Leaf', we can stick
  -- an illegal function into the first argument of the result of 'octant'.
  e :: Functor f => a -> f a
  e = error "Data.Octree.Internal.child broke an invariant."

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
