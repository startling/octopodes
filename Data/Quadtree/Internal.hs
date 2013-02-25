{-# Language DeriveDataTypeable #-}
module Data.Quadtree.Internal where
-- base
import Data.Typeable

-- | An enumerated types representing four directions.
data Quadrant
  = Northeast
  | Northwest
  | Southeast
  | Southwest
  deriving (Eq, Show, Ord, Typeable)

