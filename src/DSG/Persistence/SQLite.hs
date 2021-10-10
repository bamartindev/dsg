module DSG.Persistence.SQLite 
  ( createTables
  , insertNewTask
  , fetchAllTasks
  , insertNewWorkspace
  , fetchAllWorkspaces
  )
where

import Control.Applicative
import qualified Data.UUID as UUID
import qualified Data.Text as T
import Database.SQLite.Simple

import DSG.Domain.Workspace (Workspace(..))
import DSG.Domain.Task (Task(..))
import DSG.Persistence.DAO.Workspace

dbname :: String
dbname = "dsg.db"

createTables :: IO ()
createTables = do
  conn <- open dbname
  execute_ conn 
    "CREATE TABLE IF NOT EXISTS workspace (id TEXT PRIMARY KEY, name TEXT, current_task TEXT)"
  execute_ conn $
    "CREATE TABLE IF NOT EXISTS task" <>
    "(id TEXT PRIMARY KEY, workspace TEXT, description TEXT, priority TEXT, completed INTEGER, created_at DATETIME, " <>
    "FOREIGN KEY(workspace) REFERENCES workspace(id))"
  close conn

insertNewTask :: Task -> IO ()
insertNewTask task = do
  conn <- open dbname
  execute conn "INSERT INTO task (id, workspace, description, priority, completed, created_at) VALUES (?,?,?,?,?,?)" task
  close conn

fetchAllTasks :: IO [Task]
fetchAllTasks = do
  conn <- open dbname
  tasks <- query_ conn "SELECT * FROM task"
  close conn

  return tasks

insertNewWorkspace :: Workspace -> IO ()
insertNewWorkspace ws = do
  let wsdao = workspaceIntoDAO ws
  conn <- open dbname
  execute conn "INSERT INTO workspace (id, name, current_task) VALUES (?,?,?)" wsdao
  close conn

fetchAllWorkspaces :: IO [WorkspaceDAO]
fetchAllWorkspaces = do
  conn <- open dbname
  wss <- query_ conn "SELECT * FROM workspace"
  close conn

  return wss