module DSG.Domain.TaskSpec
  ( spec
  )
where

import Test.Hspec ( Spec, describe, it, shouldBe )

import DSG.Domain.Task (Task(..), mkTask, sortTasksNewestFirst, sortTasksOldestFirst)

spec :: Spec
spec = do
  describe "mkTask" $ do
    it "creates a new Task with the provided description" $ do
      task <- mkTask "Test creating a new task"

      taskDescription task `shouldBe` "Test creating a new task"

  describe "sortTasksNewestFirst" $ do
    it "takes an unsorted list of Tasks and sorts by createdAt, newest first" $ do
      first <- mkTask "first task"
      second <- mkTask "second task"
      third <- mkTask "third task"
      fourth <- mkTask "fourth task"

      let tasks = [second, fourth, first, third]
      let expected = [fourth, third, second, first]

      sortTasksNewestFirst tasks `shouldBe` expected

  describe "sortTasksOlderFirst" $ do
    it "takes an unsorted list of Tasks and sorts by createdAt, oldest first" $ do
      first <- mkTask "first task"
      second <- mkTask "second task"
      third <- mkTask "third task"
      fourth <- mkTask "fourth task"

      let tasks = [second, fourth, first, third]
      let expected = [first, second, third, fourth]

      sortTasksOldestFirst tasks `shouldBe` expected
