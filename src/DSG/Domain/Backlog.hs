module DSG.Domain.Backlog
  ( BacklogStrategy(..)
  , Backlog(..)
  , mkBacklog
  , updateBacklogWithStrategy
  )
where

import DSG.Domain.Task (Task, sortTasksNewestFirst, sortTasksOldestFirst)

data BacklogStrategy = BacklogStrategyNewest | BacklogStrategyOldest
  deriving (Show, Eq)

data Backlog = Backlog
  { backlogTasks :: [Task]
  , backlogStrategy :: BacklogStrategy
  }
  deriving (Show, Eq)

mkBacklog :: BacklogStrategy -> Backlog
mkBacklog strategy = Backlog
  { backlogTasks = []
  , backlogStrategy = strategy
  }

updateBacklogWithStrategy :: Task -> Backlog -> Backlog
updateBacklogWithStrategy t b = case backlogStrategy b of
  BacklogStrategyNewest -> b {backlogTasks = sortTasksNewestFirst preStrategyBacklog}
  BacklogStrategyOldest -> b {backlogTasks = sortTasksOldestFirst preStrategyBacklog}
  where
    preStrategyBacklog = t : backlogTasks b 