module DSG.Domain.Workspace
  ( Workspace(..)
  , mkWorkspace
  , completeCurrentTask
  , swapCurrentTask
  , addTaskToWorkspaceWithStrategy
  )
where

import Data.Text (Text)
import qualified Data.UUID.V4 as V4UUID
import Data.UUID (UUID)
import Database.SQLite.Simple( FromRow(..), ToRow(..), field)

import DSG.Domain.WorkspaceId (WorkspaceId(..))
import DSG.Domain.Task (Task(taskId))
import DSG.Domain.Backlog (mkBacklog, addTaskToBacklogWithStrategy, BacklogStrategy (BacklogStrategyOldest), Backlog)

data Workspace = Workspace
  { workspaceId :: WorkspaceId
  , workspaceName :: Text
  , currentTask :: Maybe Task
  , completedTasks :: [Task]
  , workspaceBacklog :: Backlog 
  }
  deriving (Show, Eq)

-- instance FromRow Workspace where
--   fromRow = Workspace <$> field <*> field <*> field <*> field <*> field

-- instance ToRow Workspace where
--   toRow (Workspace id_ name current completed backlog) = toRow (id_, name, current, completed, backlog)

mkWorkspace :: Text -> IO Workspace
mkWorkspace name = do
  id <- V4UUID.nextRandom
  pure $ Workspace 
    { workspaceId = WorkspaceId id
    , workspaceName = name
    , currentTask = Nothing
    , completedTasks = []
    , workspaceBacklog = mkBacklog BacklogStrategyOldest 
    }

-- |Takes a 'Workspace' and adds the current task, if there is one,
-- to the list of completed tasks.  The current task is then set to Nothing.
-- If the current task is Nothing, this is a no-op
completeCurrentTask :: Workspace -> Workspace
completeCurrentTask ws = case currentTask ws of
  Nothing -> ws
  Just task -> ws {currentTask = Nothing, completedTasks = updatedCompletedTasks}
    where
      updatedCompletedTasks = task : completedTasks ws

-- |Takes a 'Task' and 'Workspace', then returns a tuple with the previous
-- current task if one exists, and a new 'Workspace' with the supplie 'Task' as the current task.
swapCurrentTask :: Workspace -> Task -> (Maybe Task, Workspace)
swapCurrentTask ws t = case currentTask ws of
  Nothing -> (Nothing, ws {currentTask = pure t})
  Just task -> (pure task, ws {currentTask = pure t})

-- |Takes a 'Task' and 'Workspace' and adds it to the end of the task backlog
addTaskToWorkspaceWithStrategy :: Workspace -> Task -> Workspace
addTaskToWorkspaceWithStrategy ws t = ws {workspaceBacklog = updatedBacklog}
  where
    updatedBacklog = addTaskToBacklogWithStrategy (workspaceBacklog ws) t

