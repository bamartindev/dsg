module DSG.Domain.Workspace
  ( Workspace(..)
  , mkWorkspace
  , completeCurrentTask
  , swapCurrentTask
  , addTaskToBacklog
  )
where

import Data.Text (Text)
import qualified Data.UUID.V4 as V4UUID
import Data.UUID (UUID)

import DSG.Domain.Task (Task(taskId))
import DSG.Domain.Backlog (mkBacklog, updateBacklogWithStrategy, BacklogStrategy (BacklogStrategyOldest), Backlog)

data Workspace = Workspace
  { workspaceId :: UUID
  , workspaceName :: Text
  , currentTask :: Maybe Task
  , completedTasks :: [Task]
  , workspaceBacklog :: Backlog 
  }
  deriving (Show, Eq)

mkWorkspace :: Text -> IO Workspace
mkWorkspace name = do
  id <- V4UUID.nextRandom
  pure $ Workspace 
    { workspaceId = id
    , workspaceName = name
    , currentTask = Nothing
    , completedTasks = []
    , workspaceBacklog = mkBacklog BacklogStrategyOldest 
    }

-- |'completeCurrentTask' takes a 'Workspace' and adds the current task, if there is one,
-- to the list of completed tasks.  The current task is then set to Nothing.
-- If the current task is Nothing, this is a no-op
completeCurrentTask :: Workspace -> Workspace
completeCurrentTask ws = case currentTask ws of
  Nothing -> ws
  Just task -> ws {currentTask = Nothing, completedTasks = updatedCompletedTasks}
    where
      updatedCompletedTasks = task : completedTasks ws

-- |'swapCurrentTask' takes a 'Task' and 'Workspace', then returns a tuple with the previous
-- current task if one exists, and a new 'Workspace' with the supplie 'Task' as the current task.
swapCurrentTask :: Task -> Workspace -> (Maybe Task, Workspace)
swapCurrentTask t ws = case currentTask ws of
  Nothing -> (Nothing, ws {currentTask = pure t})
  Just task -> (pure task, ws {currentTask = pure t})

-- |`addTaskToBacklog` takes a 'Task' and 'Workspace' and adds it to the end of the task backlog
addTaskToBacklog :: Task -> Workspace -> Workspace
addTaskToBacklog t ws = ws {workspaceBacklog = updatedBacklog}
  where
    updatedBacklog = updateBacklogWithStrategy t (workspaceBacklog ws)

