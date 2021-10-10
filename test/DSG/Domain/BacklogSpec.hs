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
import DSG.Domain.WorkspaceId (WorkspaceId(..))
import Data.UUID as UUID
import Data.Maybe (fromJust)

dummyWorkspaceId :: WorkspaceId
dummyWorkspaceId = WorkspaceId $ fromJust $ UUID.fromString "36b435ee-ac09-42a9-acb6-3bcd4d9461f7"

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
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityHigh 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityLow 
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityUrgent 
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityNone

      let backlog = Backlog {backlogTasks = [first, second, third, fourth], backlogStrategy = BacklogStrategyOldest }
      let expected = Backlog {backlogTasks = [third, first, second, fourth], backlogStrategy = BacklogStrategyHighestPriority }
      let result = setBacklogStrategyAndUpdate backlog BacklogStrategyHighestPriority

      result `shouldBe` expected

  describe "addTaskToBacklog" $ do
    it "should return a Backlog with the task added to the front of the backlogTasks list" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityHigh 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityLow 
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityUrgent 

      let backlog = Backlog {backlogTasks = [first, second], backlogStrategy = BacklogStrategyOldest }
      let expected = Backlog {backlogTasks = [third, first, second], backlogStrategy = BacklogStrategyOldest }
      let result = addTaskToBacklog backlog third

      result `shouldBe` expected

  describe "addTaskToBacklogWithStrategy" $ do
    it "sorts the backlog tasks from oldest to newest when the strategy is BacklogStrategyOldest" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityNone
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityNone
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityNone
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityNone

      let expected = Backlog {backlogTasks = [first, second, third, fourth], backlogStrategy = BacklogStrategyOldest }
      let backlog = mkBacklog BacklogStrategyOldest

      -- updating the backlog with second, fourth, third, then first
      let tasks = [second, fourth, third, first]
      let backlog' = foldl addTaskToBacklogWithStrategy backlog tasks

      backlog' `shouldBe` expected

    it "sorts the backlog tasks from newest to oldest when the strategy is BacklogStrategyNewest" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityNone
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityNone
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityNone
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityNone

      let expected = Backlog {backlogTasks = [fourth, third, second, first], backlogStrategy = BacklogStrategyNewest }
      let backlog = mkBacklog BacklogStrategyNewest

      -- updating the backlog with second, first, third, then fourth
      let tasks = [second, first, third, fourth]
      let backlog' = foldl addTaskToBacklogWithStrategy backlog tasks

      backlog' `shouldBe` expected
    
    it "sorts the backlog tasks from highest to lowest priority when the strategy is BacklogStrategyHighestPriority" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityUrgent 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityNone
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityLow 
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityMedium 
      fifth <- mkTask dummyWorkspaceId "fifth task" TaskPriorityHigh 

      let expected = Backlog {backlogTasks = [first, fifth, fourth, third, second], backlogStrategy = BacklogStrategyHighestPriority }
      let backlog = mkBacklog BacklogStrategyHighestPriority

      -- updating the backlog with second, first, fifth, third, then fourth
      let tasks = [second, first, fifth, third, fourth]
      let backlog' = foldl addTaskToBacklogWithStrategy backlog tasks

      backlog' `shouldBe` expected

    it "sorts the backlog tasks from lowest to highest priority when the strategy is BacklogStrategyLowestPriority" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityUrgent 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityNone
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityLow 
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityMedium 
      fifth <- mkTask dummyWorkspaceId "fifth task" TaskPriorityHigh 

      let expected = Backlog {backlogTasks = [second, third, fourth, fifth, first], backlogStrategy = BacklogStrategyLowestPriority }
      let backlog = mkBacklog BacklogStrategyLowestPriority

      -- updating the backlog with second, first, fifth, third, then fourth
      let tasks = [second, first, fifth, third, fourth]
      let backlog' = foldl addTaskToBacklogWithStrategy backlog tasks

      backlog' `shouldBe` expected