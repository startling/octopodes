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

instance Arbitrary a => Arbitrary (Octree a) where
  arbitrary = sized $ \n -> frequency
    [ (10, Leaf <$> arbitrary)
    , (n `div` 5, resize (n `div` 2) branch) ] where
      branch = Branch <$> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Quadrant where
  arbitrary = elements [Northeast, Northwest, Southwest, Southeast]

instance Arbitrary Octant where
  arbitrary = oneof [Near <$> arbitrary, Far <$> arbitrary]

instance Arbitrary a => Arbitrary (Operation a) where
  arbitrary = Insert <$> listOf arbitrary <*> arbitrary

-- Tell whether an octree is simplified.
simplified :: Eq a => Octree a -> Bool
simplified (Leaf _) = True
simplified (Branch (Leaf a) b c d e f g h) = any (/= Leaf a) l
  && all simplified l where l = [b, c, d, e, f, g, h]
simplified (Branch a b c d e f g h) = all simplified l
  where l = [a, b, c, d, e, f, g, h]

main = hspec $ do
  describe "child" $ do
    it "always gives a value for leaves" . property $
      \os v -> Leaf (v :: Int) ^. child os == Leaf v
    it "follows the first lens law" . property $
      \os v t -> (t & child os .~ v)^.child os == (v :: Octree Int)
    it "follows the second lens law" . property $ do
      -- Only guaranteed for simplified trees.
      t <- arbitrary `suchThat` simplified :: Gen (Octree Int)
      os <- arbitrary
      let r = t ^. child os
      return $ (t & child os .~ r) == t
    it "follows the third lens law" . property $ do
      -- Only guaranteed for simplified trees.
       t <- arbitrary `suchThat` simplified :: Gen (Octree Int)
       (os, v1, v2) <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
       return $ (t & child os .~ v1 & child os .~ v2)
         == (t & child os .~ v2)
  describe "path" $ do
    it "creates a simplified tree when inserting" . property $
     \os v y -> simplified $ set (path os) (v :: Int) (Leaf y)
    it "always gives a value for leaves" . property $
      \os v -> Leaf (v :: Int) ^. path os == [v]
    it "follows the first lens law" . property $
      \os v t -> (t & path os .~ v)^.path os == [v :: Int]
    it "follows the second lens law" . property $ do
      -- Only guaranteed for simplified trees.
      t <- arbitrary `suchThat` simplified :: Gen (Octree Int)
      os <- arbitrary
      return $ case t ^. path os of
        v : [] -> (t & path os .~ v) == (t :: Octree Int)
        _ -> True
    it "follows the third lens law" . property $ do 
       -- Only guaranteed for simplified trees.
       t <- arbitrary `suchThat` simplified :: Gen (Octree Int)
       os <- arbitrary
       v1 <- arbitrary :: Gen Int
       v2 <- arbitrary :: Gen Int
       return $ (t & path os .~ v1 & path os .~ v2)
          == (t & path os .~ v2)
