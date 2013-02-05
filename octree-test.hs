module Main where
-- base
import Control.Applicative
-- quickcheck
import Test.QuickCheck
-- hspec
import Test.Hspec
-- pockets
import Data.Octree
import Data.Octree.Internal
-- lens
import Control.Lens hiding (elements)

instance Arbitrary a => Arbitrary (Node a) where
  arbitrary = sized $ \n -> frequency
    [(10,  Leaf <$> arbitrary )
    ,(n `div` 5, resize (n `div` 2) $ Branch <$> arbitrary ) ]

instance Arbitrary a => Arbitrary (Halftree a) where
  arbitrary = Halftree <$> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Octree a) where
  arbitrary = Octree <$> arbitrary <*> arbitrary

instance Arbitrary Quadrant where
  arbitrary = elements [Northeast, Northwest, Southwest, Southeast]

instance Arbitrary Octant where
  arbitrary = oneof [Near <$> arbitrary, Far <$> arbitrary]

instance Arbitrary a => Arbitrary (Operation a) where
  arbitrary = Insert <$> listOf arbitrary <*> arbitrary

main = hspec $ do
  describe "path" $ do
    it "creates a simplified tree when inserting" . property $
     \os v y -> case set (path os) (v :: Int) (Leaf y) of
        Leaf a -> True
        Branch b -> simplified b
    it "always gives a value for leaves" . property $
      \os v -> Leaf (v :: Int) ^. path os == Just v
  describe "reduce" $ do
    it "always produces a nonempty list" . property $
      \x -> not . null $ reduce (x :: Node Int)
  describe "recreate" $ do
    it "undoes `reduce`" . property $
      \x -> recreate (0 :: Int) (reduce x) == x
