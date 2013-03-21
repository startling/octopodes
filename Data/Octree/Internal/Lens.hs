module Data.Octree.Internal.Lens where
-- base
import Control.Applicative
-- octopodes
import Data.Octree.Internal.Types

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its near northeast octant.
nne :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
nne f _ (Leaf a) = Leaf <$> f a
nne _ f o = (\x -> o { _nne = x}) <$> f (_nne o)

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its near northwest octant.
nnw :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
nnw f _ (Leaf a) = Leaf <$> f a
nnw _ f o = (\x -> o { _nnw = x}) <$> f (_nnw o)

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its near southeast octant.
nse :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
nse f _ (Leaf a) = Leaf <$> f a
nse _ f o = (\x -> o { _nse = x}) <$> f (_nse o)

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its near southwest octant.
nsw :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
nsw f _ (Leaf a) = Leaf <$> f a
nsw _ f o = (\x -> o { _nsw = x}) <$> f (_nsw o)

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its far northeast octant.
fne :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
fne f _ (Leaf a) = Leaf <$> f a
fne _ f o = (\x -> o { _fne = x}) <$> f (_fne o)

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its far northwest octant.
fnw :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
fnw f _ (Leaf a) = Leaf <$> f a
fnw _ f o = (\x -> o { _fnw = x}) <$> f (_fnw o)

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its far southeast octant.
fse :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
fse f _ (Leaf a) = Leaf <$> f a
fse _ f o = (\x -> o { _fse = x}) <$> f (_fse o)

-- | A bilens on some @'Octree a'. If it is a 'Leaf', the first argument
-- function is used on its contents; if it is a branch, the second is used on
-- its far southwest octant.
fsw :: Functor f =>
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
fsw f _ (Leaf a) = Leaf <$> f a
fsw _ f o = (\x -> o { _fsw = x}) <$> f (_fsw o)

-- | Dispatch to one of the above bilenses with an 'Octant'.
octant :: Functor f =>
  Octant ->
  (a -> f a) -> (Octree a -> f (Octree a)) ->
  Octree a -> f (Octree a)
octant o = case o of
  (Near Northeast) -> nne; (Far Northeast) -> fne;
  (Near Northwest) -> nnw; (Far Northwest) -> fnw;
  (Near Southeast) -> nse; (Far Southeast) -> fse;
  (Near Southwest) -> nsw; (Far Southwest) -> fsw;
