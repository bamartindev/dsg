module DSG.Domain.TaskSpec
  ( spec
  )
where

import Test.Hspec ( Spec, describe, it, shouldBe )

import DSG.Domain.Task (Task(..), mkTask, sortTasksNewestFirst, sortTasksOldestFirst, sortTasksHighestPriorityFirst, sortTasksLowestPriorityFirst, filterTasksByPriority, filterTasksWithoutPriority)
import DSG.Domain.TaskPriority (TaskPriority(..))
import DSG.Domain.WorkspaceId (WorkspaceId(..))
import Data.UUID as UUID
import Data.Maybe (fromJust)

dummyWorkspaceId :: WorkspaceId
dummyWorkspaceId = WorkspaceId $ fromJust $ UUID.fromString "36b435ee-ac09-42a9-acb6-3bcd4d9461f7"

spec :: Spec
spec = do
  describe "mkTask" $ do
    it "creates a new Task with the provided description" $ do
      task <- mkTask dummyWorkspaceId "Test creating a new task" TaskPriorityNone

      taskDescription task `shouldBe` "Test creating a new task"

  describe "sortTasksNewestFirst" $ do
    it "takes an unsorted list of Tasks and sorts by createdAt, newest first" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityNone
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityNone
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityNone
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityNone

      let tasks = [second, fourth, first, third]
      let expected = [fourth, third, second, first]

      sortTasksNewestFirst tasks `shouldBe` expected

  describe "sortTasksOlderFirst" $ do
    it "takes an unsorted list of Tasks and sorts by createdAt, oldest first" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityNone
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityNone
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityNone
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityNone

      let tasks = [second, fourth, first, third]
      let expected = [first, second, third, fourth]

      sortTasksOldestFirst tasks `shouldBe` expected

  describe "sortTasksHighestPriorityFirst" $ do
    it "takes an unsorted list of Tasks and sorts by taskPriority, highest first" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityMedium 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityUrgent
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityLow
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityHigh

      let tasks = [first, second, third, fourth]
      let expected = [second, fourth, first, third]

      sortTasksHighestPriorityFirst tasks `shouldBe` expected

  describe "sortTasksLowestPriorityFirst" $ do
    it "takes an unsorted list of Tasks and sorts by taskPriority, lowest first" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityMedium 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityUrgent
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityLow
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityHigh

      let tasks = [first, second, third, fourth]
      let expected = [third, first, fourth, second]

      sortTasksLowestPriorityFirst tasks `shouldBe` expected

  describe "filterTasksByPriority" $ do
    it "takes an list of Tasks and filters by provided TaskPriority" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityMedium 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityUrgent
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityLow
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityHigh
      fifth <- mkTask dummyWorkspaceId "fifth task" TaskPriorityLow
      sixth <- mkTask dummyWorkspaceId "sixth task" TaskPriorityNone 

      let tasks = [fourth, sixth, second, fifth, third, first]

      filterTasksByPriority tasks TaskPriorityUrgent `shouldBe` [second]
      filterTasksByPriority tasks TaskPriorityHigh `shouldBe` [fourth]
      filterTasksByPriority tasks TaskPriorityMedium `shouldBe` [first]
      filterTasksByPriority tasks TaskPriorityLow `shouldBe` [fifth, third]
      filterTasksByPriority tasks TaskPriorityNone `shouldBe` [sixth]

  describe "filterTasksWithoutPriority" $ do
    it "takes an list of Tasks and filters out by provided TaskPriority" $ do
      first <- mkTask dummyWorkspaceId "first task" TaskPriorityMedium 
      second <- mkTask dummyWorkspaceId "second task" TaskPriorityUrgent
      third <- mkTask dummyWorkspaceId "third task" TaskPriorityLow
      fourth <- mkTask dummyWorkspaceId "fourth task" TaskPriorityHigh
      fifth <- mkTask dummyWorkspaceId "fifth task" TaskPriorityLow
      sixth <- mkTask dummyWorkspaceId "sixth task" TaskPriorityNone 

      let tasks = [fourth, sixth, second, fifth, third, first]

      filterTasksWithoutPriority tasks TaskPriorityUrgent `shouldBe` [fourth, sixth, fifth, third, first]
      filterTasksWithoutPriority tasks TaskPriorityHigh `shouldBe` [sixth, second, fifth, third, first]
      filterTasksWithoutPriority tasks TaskPriorityMedium `shouldBe` [fourth, sixth, second, fifth, third]
      filterTasksWithoutPriority tasks TaskPriorityLow `shouldBe` [fourth, sixth, second, first]
      filterTasksWithoutPriority tasks TaskPriorityNone `shouldBe` [fourth, second, fifth, third, first]