module DSG.Persistence.DAO.Workspace
  ( WorkspaceDAO
  , workspaceIntoDAO
  )
where

import Data.Text (Text)
import Database.SQLite.Simple( FromRow(..), ToRow(..), field)

import DSG.Domain.Workspace (Workspace(..))
import DSG.Domain.WorkspaceId (WorkspaceId)
import DSG.Domain.Task (Task(taskId))
import DSG.Domain.TaskId (TaskId)

data WorkspaceDAO = WorkspaceDAO
  { workspaceDAOId :: WorkspaceId
  , workspaceDAOName :: Text
  , workspaceDAOCurrentTask :: Maybe TaskId
  }
  deriving (Show)

instance FromRow WorkspaceDAO where
  fromRow = WorkspaceDAO <$> field <*> field <*> field

instance ToRow WorkspaceDAO where
  toRow (WorkspaceDAO id_ name ct) = toRow (id_, name, ct)

workspaceIntoDAO :: Workspace -> WorkspaceDAO
workspaceIntoDAO ws = do
  let id = workspaceId ws
  let name = workspaceName ws
  let ct = taskId <$> currentTask ws
  WorkspaceDAO id name ct