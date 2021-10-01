module DSG.Domain.WorkspaceSpec 
  ( spec
  )
where

import Test.Hspec ( Spec, describe, it, shouldBe )

import DSG.Domain.Backlog (BacklogStrategy(BacklogStrategyOldest), Backlog(backlogTasks), mkBacklog)
import DSG.Domain.Task (mkTask)
import DSG.Domain.TaskPriority (TaskPriority(TaskPriorityNone))
import DSG.Domain.Workspace (Workspace(..), mkWorkspace, completeCurrentTask, swapCurrentTask, addTaskToWorkspaceWithStrategy)

spec :: Spec
spec = do
  describe "mkWorspace" $ do
    it "creates a new Workspace with provided name and defaults" $ do
      ws <- mkWorkspace "Testing creation of a new workspace"

      workspaceName ws `shouldBe` "Testing creation of a new workspace"

      -- Expected defaults for a new Workspace
      currentTask ws `shouldBe` Nothing
      completedTasks ws `shouldBe` []
      workspaceBacklog ws `shouldBe` mkBacklog BacklogStrategyOldest 

  describe "swapCurrentTask" $ do
    describe "without a current task" $ do
      it "should set the provided Task as the current task in the Workspace, and return Nothing for the previous task" $ do
        ws <- mkWorkspace "Test Workspace"
        task <- mkTask "new task" TaskPriorityNone

        let (task', ws') = swapCurrentTask task ws

        task' `shouldBe` Nothing
        currentTask ws' `shouldBe` Just task
    
    describe "with a current task" $ do
      it "should set the provided Task as the current task in the Workspace, and return Maybe Task for the previous task" $ do
        ws <- mkWorkspace "Test Workspace"
        task <- mkTask "new task" TaskPriorityNone
        anotherTask <- mkTask "another task" TaskPriorityNone

        let (Nothing, ws') = swapCurrentTask task ws
        let (task', ws'') = swapCurrentTask anotherTask ws'

        task' `shouldBe` Just task
        currentTask ws'' `shouldBe` Just anotherTask

  describe "completeCurrentTask" $ do
    describe "without a current task" $ do
      it "should be a no-op" $ do
        ws <- mkWorkspace "Test Workspace"
        let ws' = completeCurrentTask ws

        ws `shouldBe` ws'
    
    describe "with a current task" $ do
      it "should add the current task to completed tasks, and set the current task to Nothing" $ do
        ws <- mkWorkspace "Test Workspace"
        task <- mkTask "the task" TaskPriorityNone

        let (Nothing, ws') = swapCurrentTask task ws
        let result = completeCurrentTask ws'

        currentTask result `shouldBe` Nothing
        completedTasks result `shouldBe` [task]

  describe "addTaskToWorkspaceWithStrategy" $ do
    describe "working with default backlog setup" $ do
      it "should add tasks to the backlog" $ do
        ws <- mkWorkspace "Test Workspace"
        first <- mkTask "first task" TaskPriorityNone
        second <- mkTask "second task" TaskPriorityNone
        third <- mkTask "third task" TaskPriorityNone

        let expected = [first, second, third]
        -- adding in the order second, first, third to make sure the sorting is also working
        let result = addTaskToWorkspaceWithStrategy third 
                      $ addTaskToWorkspaceWithStrategy first 
                      $ addTaskToWorkspaceWithStrategy second ws
        
        (backlogTasks . workspaceBacklog) result `shouldBe` expected

