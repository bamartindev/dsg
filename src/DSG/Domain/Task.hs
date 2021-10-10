module DSG.Domain.Task
  ( Task(..)
  , mkTask
  , sortTasksNewestFirst
  , sortTasksOldestFirst
  , sortTasksHighestPriorityFirst
  , sortTasksLowestPriorityFirst
  , filterTasksByPriority
  , filterTasksWithoutPriority
  )
where

import qualified Data.List as List
import qualified Data.Time as Time
import Data.Time (UTCTime)
import Data.Text (Text)
import qualified Data.UUID.V4 as V4UUID
import Data.UUID (UUID)
import Database.SQLite.Simple( FromRow(..), ToRow(..), field)
import DSG.Domain.WorkspaceId (WorkspaceId)
import DSG.Domain.TaskId (TaskId(..))
import DSG.Domain.TaskPriority (TaskPriority)

data Task = Task
  { taskId :: TaskId
  , wsId :: WorkspaceId
  , taskDescription :: Text
  , taskPriority :: TaskPriority
  , taskCompleted :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show)

instance Eq Task where
  x == y = unwrapTaskId (taskId x) == unwrapTaskId (taskId y)
  x /= y = not (x == y)

instance Ord Task where
  x `compare` y = createdAt x `compare` createdAt y

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Task where
  toRow (Task id_ workspace desc prio completed createdAt) = toRow (id_, workspace, desc, prio, completed, createdAt)

mkTask :: WorkspaceId -> Text -> TaskPriority -> IO Task
mkTask wsId desc priority = do
  id <- V4UUID.nextRandom
  now <- Time.getCurrentTime 
  pure $ Task
    { taskId = TaskId id
    , wsId = wsId
    , taskDescription = desc
    , taskPriority = priority
    , taskCompleted = False
    , createdAt = now
    }

-- |Takes a list of 'Task' and returns a list of 
-- 'Task' that are sortest from most recent createdAt to oldest
sortTasksNewestFirst :: [Task] -> [Task]
sortTasksNewestFirst = sortTasks f
  where
    f t1 t2 = t2 `compare` t1

-- |Takes a list of 'Task' and returns a list of 
-- 'Task' that are sortest from the oldest createdAt to most recent
sortTasksOldestFirst :: [Task] -> [Task]
sortTasksOldestFirst = sortTasks f
  where
    f t1 t2 = t1 `compare` t2

-- |Takes a list of 'Task' and returns a list of 
-- 'Task' that are sortest from highest TaskPriority to the lowest.
sortTasksHighestPriorityFirst :: [Task] -> [Task]
sortTasksHighestPriorityFirst = sortTasks f
  where f t1 t2 = taskPriority t2 `compare` taskPriority t1

-- |Takes a list of 'Task' and returns a list of 
-- 'Task' that are sortest from lowest TaskPriority to the highest.
sortTasksLowestPriorityFirst :: [Task] -> [Task]
sortTasksLowestPriorityFirst =  sortTasks f
  where f t1 t2 = taskPriority t1 `compare` taskPriority t2

-- |A helper function that takes a predicate to sort
-- a list of 'Task'.  Should not be used directly.
sortTasks :: (Task -> Task-> Ordering) -> [Task] -> [Task]
sortTasks = List.sortBy

-- |Takes a list of 'Task' and a 'TaskPriority', then returns
-- a list of 'Task' of only that priority
filterTasksByPriority :: [Task] -> TaskPriority -> [Task]
filterTasksByPriority ts p = filter (\t -> taskPriority t == p) ts

-- |Takes a list of 'Task' and a 'TaskPriority', then returns
-- a list of 'Task' that does not contain that priority
filterTasksWithoutPriority :: [Task] -> TaskPriority -> [Task]
filterTasksWithoutPriority ts p = filter (\t -> taskPriority t /= p) ts