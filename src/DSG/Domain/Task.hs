module DSG.Domain.Task
  ( Task(..)
  , TaskId
  , mkTask
  , sortTasksNewestFirst
  , sortTasksOldestFirst
  )
where

import qualified Data.List as List
import qualified Data.Time as Time
import Data.Time (UTCTime)
import Data.Text (Text)
import qualified Data.UUID.V4 as V4UUID
import Data.UUID (UUID)

newtype TaskId = TaskId { unwrapTaskId :: UUID }
  deriving (Show)
data Task = Task
  { taskId :: TaskId
  , taskDescription :: Text
  , createdAt :: UTCTime
  }
  deriving (Show)

instance Eq Task where
  x == y = unwrapTaskId (taskId x) == unwrapTaskId (taskId y)
  x /= y = not (x == y)

instance Ord Task where
  x `compare` y = createdAt x `compare` createdAt y

mkTask :: Text -> IO Task
mkTask desc = do
  id <- V4UUID.nextRandom
  now <- Time.getCurrentTime 
  pure $ Task
    { taskId = TaskId id
    , taskDescription = desc
    , createdAt = now
    }

-- |'sortTasksNewestFirst' takes a list of 'Task' and returns a list of 
-- 'Task' that are sortest from most recent createdAt to oldest
sortTasksNewestFirst :: [Task] -> [Task]
sortTasksNewestFirst = sortTasks f
  where
    f t1 t2 = t2 `compare` t1

-- |'sortTasksOldestFirst' takes a list of 'Task' and returns a list of 
-- 'Task' that are sortest from the oldest createdAt to most recent
sortTasksOldestFirst :: [Task] -> [Task]
sortTasksOldestFirst = sortTasks f
  where
    f t1 t2 = t1 `compare` t2

sortTasks :: (Task -> Task-> Ordering) -> [Task] -> [Task]
sortTasks = List.sortBy