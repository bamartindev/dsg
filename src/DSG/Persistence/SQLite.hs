module DSG.Persistence.SQLite 
  ( createTables
  , insertNewTask
  , fetchAllTasks
  )
where

import Control.Applicative
import qualified Data.UUID as UUID
import qualified Data.Text as T
import Database.SQLite.Simple

import DSG.Domain.Task (Task(..))


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