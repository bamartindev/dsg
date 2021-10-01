module DSG.Domain.TaskSpec
  ( spec
  )
where

import Test.Hspec ( Spec, describe, it, shouldBe )

import DSG.Domain.Task (Task(..), mkTask, sortTasksNewestFirst, sortTasksOldestFirst, sortTasksHighestPriorityFirst, sortTasksLowestPriorityFirst, filterTasksByPriority, filterTasksWithoutPriority)
import DSG.Domain.TaskPriority (TaskPriority(..))

spec :: Spec
spec = do
  describe "mkTask" $ do
    it "creates a new Task with the provided description" $ do
      task <- mkTask "Test creating a new task" TaskPriorityNone

      taskDescription task `shouldBe` "Test creating a new task"

  describe "sortTasksNewestFirst" $ do
    it "takes an unsorted list of Tasks and sorts by createdAt, newest first" $ do
      first <- mkTask "first task" TaskPriorityNone
      second <- mkTask "second task" TaskPriorityNone
      third <- mkTask "third task" TaskPriorityNone
      fourth <- mkTask "fourth task" TaskPriorityNone

      let tasks = [second, fourth, first, third]
      let expected = [fourth, third, second, first]

      sortTasksNewestFirst tasks `shouldBe` expected

  describe "sortTasksOlderFirst" $ do
    it "takes an unsorted list of Tasks and sorts by createdAt, oldest first" $ do
      first <- mkTask "first task" TaskPriorityNone
      second <- mkTask "second task" TaskPriorityNone
      third <- mkTask "third task" TaskPriorityNone
      fourth <- mkTask "fourth task" TaskPriorityNone

      let tasks = [second, fourth, first, third]
      let expected = [first, second, third, fourth]

      sortTasksOldestFirst tasks `shouldBe` expected

  describe "sortTasksHighestPriorityFirst" $ do
    it "takes an unsorted list of Tasks and sorts by taskPriority, highest first" $ do
      first <- mkTask "first task" TaskPriorityMedium 
      second <- mkTask "second task" TaskPriorityUrgent
      third <- mkTask "third task" TaskPriorityLow
      fourth <- mkTask "fourth task" TaskPriorityHigh

      let tasks = [first, second, third, fourth]
      let expected = [second, fourth, first, third]

      sortTasksHighestPriorityFirst tasks `shouldBe` expected

  describe "sortTasksLowestPriorityFirst" $ do
    it "takes an unsorted list of Tasks and sorts by taskPriority, lowest first" $ do
      first <- mkTask "first task" TaskPriorityMedium 
      second <- mkTask "second task" TaskPriorityUrgent
      third <- mkTask "third task" TaskPriorityLow
      fourth <- mkTask "fourth task" TaskPriorityHigh

      let tasks = [first, second, third, fourth]
      let expected = [third, first, fourth, second]

      sortTasksLowestPriorityFirst tasks `shouldBe` expected

  describe "filterTasksByPriority" $ do
    it "takes an list of Tasks and filters by provided TaskPriority" $ do
      first <- mkTask "first task" TaskPriorityMedium 
      second <- mkTask "second task" TaskPriorityUrgent
      third <- mkTask "third task" TaskPriorityLow
      fourth <- mkTask "fourth task" TaskPriorityHigh
      fifth <- mkTask "fifth task" TaskPriorityLow
      sixth <- mkTask "sixth task" TaskPriorityNone 

      let tasks = [fourth, sixth, second, fifth, third, first]

      filterTasksByPriority tasks TaskPriorityUrgent `shouldBe` [second]
      filterTasksByPriority tasks TaskPriorityHigh `shouldBe` [fourth]
      filterTasksByPriority tasks TaskPriorityMedium `shouldBe` [first]
      filterTasksByPriority tasks TaskPriorityLow `shouldBe` [fifth, third]
      filterTasksByPriority tasks TaskPriorityNone `shouldBe` [sixth]

  describe "filterTasksWithoutPriority" $ do
    it "takes an list of Tasks and filters out by provided TaskPriority" $ do
      first <- mkTask "first task" TaskPriorityMedium 
      second <- mkTask "second task" TaskPriorityUrgent
      third <- mkTask "third task" TaskPriorityLow
      fourth <- mkTask "fourth task" TaskPriorityHigh
      fifth <- mkTask "fifth task" TaskPriorityLow
      sixth <- mkTask "sixth task" TaskPriorityNone 

      let tasks = [fourth, sixth, second, fifth, third, first]

      filterTasksWithoutPriority tasks TaskPriorityUrgent `shouldBe` [fourth, sixth, fifth, third, first]
      filterTasksWithoutPriority tasks TaskPriorityHigh `shouldBe` [sixth, second, fifth, third, first]
      filterTasksWithoutPriority tasks TaskPriorityMedium `shouldBe` [fourth, sixth, second, fifth, third]
      filterTasksWithoutPriority tasks TaskPriorityLow `shouldBe` [fourth, sixth, second, first]
      filterTasksWithoutPriority tasks TaskPriorityNone `shouldBe` [fourth, second, fifth, third, first]