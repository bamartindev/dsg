module DSG.Domain.BacklogSpec 
  ( spec
  )
where

import Test.Hspec ( Spec, describe, it, shouldBe )

import DSG.Domain.Backlog (mkBacklog, BacklogStrategy (..), Backlog(..), updateBacklogWithStrategy)
import DSG.Domain.Task (mkTask)

spec :: Spec
spec = do
  describe "mkBacklog" $ do
    it "creates a new Backlog with the BacklogStrategyNewest strategy" $ do
      let expected = Backlog {backlogTasks = [], backlogStrategy = BacklogStrategyNewest}
      let backlog = mkBacklog BacklogStrategyNewest

      backlog `shouldBe` expected

    it "creates a new Backlog with the BacklogStrategyOldest strategy" $ do
      let expected = Backlog {backlogTasks = [], backlogStrategy = BacklogStrategyOldest }
      let backlog = mkBacklog BacklogStrategyOldest

      backlog `shouldBe` expected

  describe "updateBacklogWithStrategy" $ do
    it "sorts the backlog tasks from oldest to newest when the strategy is BacklogStrategyOldest" $ do
      first <- mkTask "first task"
      second <- mkTask "second task"
      third <- mkTask "third task"
      fourth <- mkTask "fourth task"

      let expected = Backlog {backlogTasks = [first, second, third, fourth], backlogStrategy = BacklogStrategyOldest }
      let backlog = mkBacklog BacklogStrategyOldest

      -- updating the backlog with second, fourth, third, then first
      let backlog' = updateBacklogWithStrategy first 
                      $ updateBacklogWithStrategy third 
                      $ updateBacklogWithStrategy fourth 
                      $ updateBacklogWithStrategy second backlog 

      backlog' `shouldBe` expected

    it "sorts the backlog tasks from newest to oldest when the strategy is BacklogStrategyNewes" $ do
      first <- mkTask "first task"
      second <- mkTask "second task"
      third <- mkTask "third task"
      fourth <- mkTask "fourth task"

      let expected = Backlog {backlogTasks = [fourth, third, second, first], backlogStrategy = BacklogStrategyNewest }
      let backlog = mkBacklog BacklogStrategyNewest

      -- updating the backlog with second, first, third, then fourth
      let backlog' = updateBacklogWithStrategy fourth 
                      $ updateBacklogWithStrategy third 
                      $ updateBacklogWithStrategy first 
                      $ updateBacklogWithStrategy second backlog 

      backlog' `shouldBe` expected
    
      

