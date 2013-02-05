{-# Language DeriveDataTypeable #-}
module Data.Octree.Operations
  ( Quadrant (..)
  , Octant (..)
  , shallowSimplify
  , widen
  , path
  , Operation (..)
  , reduce
  , recreate ) where
 -- base
import Data.Typeable
import Control.Applicative
-- transformers
import Data.Functor.Identity
-- pockets
import Data.Octree.Internal

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
