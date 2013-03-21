{-# Language DeriveDataTypeable #-}
module Data.Quadtree.Internal where
-- base
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

data Quadtree a
  = Leaf a
  | Branch { ne, nw, se, sw :: Quadtree a }
  deriving (Eq, Show, Ord, Typeable)

-- | Narrow a branch into a leaf if all of its immediate children
-- are equal leaves.
narrow :: Eq a => Quadtree a -> Quadtree a
narrow br@(Branch a b c d) = if all (\x -> isLeaf x && a == x)
  [b, c, d] then a else br where
    -- Tell if an octree is a leaf.
    isLeaf (Leaf _) = True
    isLeaf _ = False

-- | A bitraversal over some @'Quadtree' a@: if it is just a leaf,
-- the first argument is used; otherwise, the second is used on its
-- immediate self-similar children.
--
-- This takes care to narrow the resulting quadtree; it may narrow some
-- previously-unnarrowed quadtrees.
nodes :: (Eq b, Applicative f) => (a -> f b) ->
  (Quadtree a -> f (Quadtree b)) -> Quadtree a -> f (Quadtree b)
nodes fn _ (Leaf a) = Leaf <$> fn a
nodes _ op (Branch a b c d) = Branch <$> fn a <*> fn b
  <*> fn c <*> fn d
    where fn = fmap narrow . op

-- | Traverse the leaves of some @'Quadtree' a@.
--
-- This takes care to narrow the resulting octree; it may narrow some
-- previously-unnarrowed octrees.
leaves :: (Eq b, Applicative f) =>
  (a -> f b) -> Quadtree a -> f (Quadtree b)
leaves = nodes <*> leaves

-- | Create a lens on a child of some @'Quadtree' a@ out of a
-- list of 'Quadrant's.
--
-- This lens takes care to narrow the resulting octree; it may not be
-- a legal lens on some previously-unnarrowed octrees.
child :: (Eq a, Functor f) => [Quadrant] ->
  (Quadtree a -> f (Quadtree a)) -> Quadtree a -> f (Quadtree a)
child [] f t = f t
child (o : os) f b = fmap narrow . quadrant o (child os f) . widen $ b where
  -- Widen a leaf into a branch with itself as its children.
  widen a@(Leaf _) = Branch a a a a
  widen b = b
  -- Turn a single octant into a lens. N.B. this does not cover 'Leaf'
  -- and so is not a total function, but we only use it on something that
  -- has been widened beforehand.
  quadrant :: Functor f => 
    Quadrant -> (Quadtree a -> f (Quadtree a)) -> Quadtree a -> f (Quadtree a)
  quadrant Northeast f o = (\x -> o { ne = x }) <$> f (ne o)
  quadrant Northwest f o = (\x -> o { nw = x }) <$> f (nw o)
  quadrant Southeast f o = (\x -> o { se = x }) <$> f (se o)
  quadrant Southwest f o = (\x -> o { sw = x }) <$> f (sw o)
  -- TODO: factor this out

-- | From a list of 'Quadrant's create a lens examining all the leaves in
-- some @'Quadtree' a@ and replacing them with a single value.
path :: (Eq a, Functor f) => [Quadrant] ->
  ([a] -> f a) -> Quadtree a -> f (Quadtree a)
path os f = child os $ fmap Leaf . f . execWriter . leaves (tell . pure)

-- | From a list of 'Quadrant's, create a lens examining the leaf value
-- at that path and replacing it with a single value.
path' :: (Eq a, Functor f) => [Quadrant] ->
  (Maybe a -> f a) -> Quadtree a -> f (Quadtree a)
path' os f = path os (f . head') where
  head' [] = Nothing; head' (a : _) = Just a;
