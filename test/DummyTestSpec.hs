module DummyTestSpec (spec) where

import Test.Hspec
import Lib (mysum)

spec :: Spec
spec =
  describe "a dummy test" $ do
    it "dummy" $
      mysum 1 2 `shouldBe` (3 :: Integer)
