{-# Language DeriveDataTypeable #-}
module Data.Octree.Operations
  ( Quadrant (..)
  , Octant (..)
  , seePath
  , setPath
  , shallowSimplify
  , widen ) where
 -- base
import Data.Typeable
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
toLens :: Octant -> Simple Lens (Octree a) (Node a)
toLens d = case d of
  (Near n) -> _near.toLens' n;
  (Far f) -> _far.toLens' f; where 
    toLens' x = case x of
      Northeast -> _northeast; Northwest -> _northwest;
      Southeast -> _southeast; Southwest -> _southwest;

-- | See the value at a given 'Octant' path.
seePath :: [Octant] -> Node a -> Either a (Octree a)
seePath (p:ps) (Branch s) = seePath ps $ s ^. toLens p
seePath [] (Branch x) = Right x
seePath _ (Leaf x) = Left x

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

-- | Set to some path of 'Octant's in a @'Node' a@.
setPath :: Eq a => [Octant] -> a -> Node a -> Node a
setPath [] v _ = Leaf v
setPath (p : ps) n o = shallowSimplify $ widen o & toLens p %~ setPath ps n
