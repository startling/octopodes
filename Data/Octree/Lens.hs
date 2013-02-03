module Data.Octree.Lens where
-- base
import Control.Applicative
-- pockets
import Data.Octree

-- | A traversal of the leaf part of some @'Node' a@.
_leaf :: Applicative f
  => (a -> f a)
  -> Node a -> f (Node a)
_leaf f = node f pure

-- | A traversal of the branch part of some @'Node' a@.
_branch :: Applicative f
  => (Octree a -> f (Octree a))
  -> Node a -> f (Node a)
_branch f = node pure f

-- | A lens on the nearer half of some @'Octree' a@.
_near :: Functor f
  => (Halftree a -> f (Halftree a))
  -> Octree a -> f (Octree a)
_near f (Octree ne fa) = flip Octree fa <$> f ne

-- | A lens on the farther half of some @'Octree' a@.
_far :: Functor f
  => (Halftree a -> f (Halftree a))
  -> Octree a -> f (Octree a)
_far f (Octree ne fa) = Octree ne <$> f fa

-- | A lens on the northeast quadrant of some @'Halftree' a@.
_northeast :: Functor f
  => (Node a -> f (Node a))
  -> Halftree a -> f (Halftree a)
_northeast f (Halftree a b c d) = (\x -> Halftree x b c d) <$> f a

-- | A lens on the northwest quadrant of some @'Halftree' a@.
_northwest :: Functor f
  => (Node a -> f (Node a))
  -> Halftree a -> f (Halftree a)
_northwest f (Halftree a b c d) = (\x -> Halftree a x c d) <$> f b

-- | A lens on the southwest quadrant of some @'Halftree' a@.
_southwest :: Functor f
  => (Node a -> f (Node a))
  -> Halftree a -> f (Halftree a)
_southwest f (Halftree a b c d) = (\x -> Halftree a b x d) <$> f c

-- | A lens on the southeast quadrant of some @'Halftree' a@.
_southeast :: Functor f
  => (Node a -> f (Node a))
  -> Halftree a -> f (Halftree a)
_southeast f (Halftree a b c d) = (\x -> Halftree a b c x) <$> f d

-- | Traverse the self-similar children of a @'Node' a@, given
-- a traversal over the leaf nodes.
plateNode :: Applicative f
  => (s -> f t)
  -> (Node s -> f (Node t))
  -> Node s -> f (Node t)
plateNode f = node f . halftrees . nodes

-- | Traverse the self-similar children of a @'Halftree' a@, given
-- a traversal over the leaf nodes.
plateHalftree :: Applicative f
  => (a -> f b)
  -> (Halftree a -> f (Halftree b))
  -> Halftree a -> f (Halftree b)
plateHalftree f = nodes . node f . halftrees

-- | Traverse the self-similar children of a @'Octree' a@, given
-- a traversal over the leaf nodes.
plateOctree :: Applicative f
  => (a -> f b)
  -> (Octree a -> f (Octree b))
  -> Octree a -> f (Octree b)
plateOctree f = halftrees . nodes . node f
