module Data.Octree.Lens where
-- base
import Control.Applicative
-- pockets
import Data.Octree.Internal

-- | Turn an @'Octant'@ into a lens.
octant :: Functor f
  => Octant
  -> (Node a -> f (Node a)) -> Octree a -> f (Octree a)
octant d = case d of
  (Near n) -> _near.quadrant n;
  (Far f) -> _far.quadrant f; where 
    _near :: Functor f
       => (Halftree a -> f (Halftree a))
       -> Octree a -> f (Octree a)
    _near f (Octree ne fa) = flip Octree fa <$> f ne
    _far :: Functor f
      => (Halftree a -> f (Halftree a))
      -> Octree a -> f (Octree a)
    _far f (Octree ne fa) = Octree ne <$> f fa
    quadrant :: Functor f
      => Quadrant
      -> (Node a -> f (Node a))
      -> Halftree a -> f (Halftree a)
    quadrant x = case x of
      Northeast -> \f (Halftree a b c d) -> (\x -> Halftree x b c d) <$> f a
      Northwest -> \f (Halftree a b c d) -> (\x -> Halftree a x c d) <$> f b
      Southeast -> \f (Halftree a b c d) -> (\x -> Halftree a b x d) <$> f c
      Southwest -> \f (Halftree a b c d) -> (\x -> Halftree a b c x) <$> f d

-- | A lens onto the value at some 'Octant' path into some @'Octree' a@.
--
-- Note that this may not be a legal lens for unsimplified octrees.
path :: (Eq a, Functor f)
  => [Octant]
  -> (Maybe a -> f a) -> Node a -> f (Node a)
path (p : ps) f n = shallowSimplify <$> (octant p . path ps) f (widen n)
path [] f (Leaf a) = Leaf <$> f (Just a)
path [] f (Branch _) = Leaf <$> f Nothing

