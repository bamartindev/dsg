module DSG.Domain.TaskPrioritySpec
  ( spec
  )
where

import Test.Hspec ( Spec, describe, it, shouldBe )

import DSG.Domain.TaskPriority (TaskPriority(..))

spec :: Spec
spec = do
  describe "TaskPriority Ordering" $ do
    it "TaskPriortyNone should be less than TaskPriorityLow" $
      TaskPriorityNone < TaskPriorityLow `shouldBe` True
    it "TaskPriorityLow should be less than TaskPriorityMedium" $
      TaskPriorityLow < TaskPriorityMedium `shouldBe` True
    it "TaskPriorityMedium should be less than TaskPriorityHigh" $
      TaskPriorityMedium < TaskPriorityHigh `shouldBe` True
    it "TaskPriorityHigh should be less than TaskPriorityUrgent" $
      TaskPriorityHigh < TaskPriorityUrgent `shouldBe` True