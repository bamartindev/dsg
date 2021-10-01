module DSG.Domain.BacklogSpec 
  ( spec
  )
where

import Test.Hspec ( Spec, describe, it, shouldBe )

import DSG.Domain.Backlog 
  ( BacklogStrategy (..)
  , Backlog(..)
  , mkBacklog
  , addTaskToBacklog
  , addTaskToBacklogWithStrategy
  , setBacklogStrategy
  , setBacklogStrategyAndUpdate
  )
import DSG.Domain.Task (mkTask)
import DSG.Domain.TaskPriority (TaskPriority(..))

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

  describe "setBacklogStrategy" $ do
    it "should return a Backlog with the specified BacklogStrategy" $ do
      let backlog = mkBacklog BacklogStrategyHighestPriority
      let expected = Backlog {backlogTasks = [], backlogStrategy = BacklogStrategyLowestPriority }
      let result = setBacklogStrategy backlog BacklogStrategyLowestPriority

      result `shouldBe` expected

  describe "setBacklogStrategyAndUpdate" $ do
    it "should return a Backlog with the specified BacklogStrategy, and that was sorted based on that strategy" $ do
      first <- mkTask "first task" TaskPriorityHigh 
      second <- mkTask "second task" TaskPriorityLow 
      third <- mkTask "third task" TaskPriorityUrgent 
      fourth <- mkTask "fourth task" TaskPriorityNone

      let backlog = Backlog {backlogTasks = [first, second, third, fourth], backlogStrategy = BacklogStrategyOldest }
      let expected = Backlog {backlogTasks = [third, first, second, fourth], backlogStrategy = BacklogStrategyHighestPriority }
      let result = setBacklogStrategyAndUpdate backlog BacklogStrategyHighestPriority

      result `shouldBe` expected

  describe "addTaskToBacklog" $ do
    it "should return a Backlog with the task added to the front of the backlogTasks list" $ do
      first <- mkTask "first task" TaskPriorityHigh 
      second <- mkTask "second task" TaskPriorityLow 
      third <- mkTask "third task" TaskPriorityUrgent 

      let backlog = Backlog {backlogTasks = [first, second], backlogStrategy = BacklogStrategyOldest }
      let expected = Backlog {backlogTasks = [third, first, second], backlogStrategy = BacklogStrategyOldest }
      let result = addTaskToBacklog third backlog

      result `shouldBe` expected

  describe "addTaskToBacklogWithStrategy" $ do
    it "sorts the backlog tasks from oldest to newest when the strategy is BacklogStrategyOldest" $ do
      first <- mkTask "first task" TaskPriorityNone
      second <- mkTask "second task" TaskPriorityNone
      third <- mkTask "third task" TaskPriorityNone
      fourth <- mkTask "fourth task" TaskPriorityNone

      let expected = Backlog {backlogTasks = [first, second, third, fourth], backlogStrategy = BacklogStrategyOldest }
      let backlog = mkBacklog BacklogStrategyOldest

      -- updating the backlog with second, fourth, third, then first
      let backlog' = addTaskToBacklogWithStrategy first 
                      $ addTaskToBacklogWithStrategy third 
                      $ addTaskToBacklogWithStrategy fourth 
                      $ addTaskToBacklogWithStrategy second backlog 

      backlog' `shouldBe` expected

    it "sorts the backlog tasks from newest to oldest when the strategy is BacklogStrategyNewest" $ do
      first <- mkTask "first task" TaskPriorityNone
      second <- mkTask "second task" TaskPriorityNone
      third <- mkTask "third task" TaskPriorityNone
      fourth <- mkTask "fourth task" TaskPriorityNone

      let expected = Backlog {backlogTasks = [fourth, third, second, first], backlogStrategy = BacklogStrategyNewest }
      let backlog = mkBacklog BacklogStrategyNewest

      -- updating the backlog with second, first, third, then fourth
      let backlog' = addTaskToBacklogWithStrategy fourth 
                      $ addTaskToBacklogWithStrategy third 
                      $ addTaskToBacklogWithStrategy first 
                      $ addTaskToBacklogWithStrategy second backlog 

      backlog' `shouldBe` expected
    
    it "sorts the backlog tasks from highest to lowest priority when the strategy is BacklogStrategyHighestPriority" $ do
      first <- mkTask "first task" TaskPriorityUrgent 
      second <- mkTask "second task" TaskPriorityNone
      third <- mkTask "third task" TaskPriorityLow 
      fourth <- mkTask "fourth task" TaskPriorityMedium 
      fifth <- mkTask "fifth task" TaskPriorityHigh 

      let expected = Backlog {backlogTasks = [first, fifth, fourth, third, second], backlogStrategy = BacklogStrategyHighestPriority }
      let backlog = mkBacklog BacklogStrategyHighestPriority

      -- updating the backlog with second, first, third, then fourth
      let backlog' = addTaskToBacklogWithStrategy fourth 
                      $ addTaskToBacklogWithStrategy third
                      $ addTaskToBacklogWithStrategy fifth
                      $ addTaskToBacklogWithStrategy first 
                      $ addTaskToBacklogWithStrategy second backlog 

      backlog' `shouldBe` expected

    it "sorts the backlog tasks from lowest to highest priority when the strategy is BacklogStrategyLowestPriority" $ do
      first <- mkTask "first task" TaskPriorityUrgent 
      second <- mkTask "second task" TaskPriorityNone
      third <- mkTask "third task" TaskPriorityLow 
      fourth <- mkTask "fourth task" TaskPriorityMedium 
      fifth <- mkTask "fifth task" TaskPriorityHigh 

      let expected = Backlog {backlogTasks = [second, third, fourth, fifth, first], backlogStrategy = BacklogStrategyLowestPriority }
      let backlog = mkBacklog BacklogStrategyLowestPriority

      -- updating the backlog with second, first, third, then fourth
      let backlog' = addTaskToBacklogWithStrategy fourth 
                      $ addTaskToBacklogWithStrategy third
                      $ addTaskToBacklogWithStrategy fifth
                      $ addTaskToBacklogWithStrategy first 
                      $ addTaskToBacklogWithStrategy second backlog 

      backlog' `shouldBe` expected