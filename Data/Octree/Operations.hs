{-# Language DeriveDataTypeable #-}
module Data.Octree.Operations
  ( Quadrant (..)
  , Octant (..)
  , shallowSimplify
  , widen
  , path ) where
 -- base
import Data.Typeable
import Control.Applicative
-- pockets
import Data.Octree
import Data.Octree.Lens
-- lens
import Control.Lens

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

-- | Turn an 'Octant' into a lens.
toLens :: Functor f
  => Octant
  -> (Node a -> f (Node a)) -> Octree a -> f (Octree a)
toLens d = case d of
  (Near n) -> _near.toLens' n;
  (Far f) -> _far.toLens' f; where 
    toLens' x = case x of
      Northeast -> _northeast; Northwest -> _northwest;
      Southeast -> _southeast; Southwest -> _southwest;

-- | Simplify some @'Octree' a@ into a leaf when all of its
-- children are equal leaves.
shallowSimplify :: Eq t => Octree t -> Node t
shallowSimplify ot = let (n : ns) = ot ^.. halftrees . nodes in
  if all isLeaf (n : ns) && all (== n) ns
    then n else Branch ot where
      isLeaf (Leaf _) = True; isLeaf (Branch _) = False;

-- | Widen a 'Leaf' into a 'Branch' of equal leaves.
widen :: Node a -> Octree a
widen (Branch b) = b
widen t@(Leaf _) = Octree ht ht where ht = Halftree t t t t

-- | A lens onto the value at some 'Octant' path into some @'Octree' a@.
--
-- Note that this may not be a legal lens for unsimplified octrees.
path :: (Eq a, Functor f)
  => [Octant]
  -> (Maybe a -> f a) -> Node a -> f (Node a)
path (p : ps) f n = shallowSimplify <$> (toLens p . path ps) f (widen n)
path [] f (Leaf a) = Leaf <$> f (Just a)
path [] f (Branch _) = Leaf <$> f Nothing
