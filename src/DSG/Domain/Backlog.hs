module DSG.Domain.Backlog
  ( BacklogStrategy(..)
  , Backlog(..)
  , mkBacklog
  , addTaskToBacklog
  , setBacklogStrategy
  , setBacklogStrategyAndUpdate
  , addTaskToBacklogWithStrategy
  , applyBacklogStrategy
  )
where

import DSG.Domain.Task (Task, sortTasksNewestFirst, sortTasksOldestFirst, sortTasksHighestPriorityFirst, sortTasksLowestPriorityFirst)

data BacklogStrategy = BacklogStrategyNewest 
                     | BacklogStrategyOldest 
                     | BacklogStrategyHighestPriority 
                     | BacklogStrategyLowestPriority
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


-- |Takes a 'Backlog' and a 'BacklogStrategy', then returns the backlog with the
-- new strategy set.
setBacklogStrategy :: Backlog -> BacklogStrategy -> Backlog
setBacklogStrategy b strategy = b {backlogStrategy = strategy}

-- |Takes a 'Backlog' and a 'BacklogStrategy', sets the new backlog strategy,
-- then applies it and returns a backlog.
setBacklogStrategyAndUpdate :: Backlog -> BacklogStrategy -> Backlog
setBacklogStrategyAndUpdate b strategy = applyBacklogStrategy (setBacklogStrategy b strategy)

-- |Takes a 'Task' and a 'Backlog' and adds the 'Task' to the front of the list of
-- backlog tasks.
addTaskToBacklog :: Backlog -> Task -> Backlog
addTaskToBacklog b t= b {backlogTasks = t : backlogTasks b}

-- |Takes a 'Task' and a 'Backlog' and adds the 'Task' to the list of backlog tasks,
-- then applies the backlog strategy for ordering the tasks.
addTaskToBacklogWithStrategy :: Backlog -> Task -> Backlog
addTaskToBacklogWithStrategy b t = applyBacklogStrategy b {backlogTasks = preStrategyBacklog}
  where
    preStrategyBacklog = t : backlogTasks b

-- |Takes a 'Backlog' and applies its 'BacklogStrategy' to order the tasks.
applyBacklogStrategy :: Backlog -> Backlog
applyBacklogStrategy b = case backlogStrategy b of
  BacklogStrategyNewest -> b {backlogTasks = sortTasksNewestFirst preStrategyBacklog}
  BacklogStrategyOldest -> b {backlogTasks = sortTasksOldestFirst preStrategyBacklog}
  BacklogStrategyHighestPriority -> b {backlogTasks = sortTasksHighestPriorityFirst preStrategyBacklog}
  BacklogStrategyLowestPriority -> b {backlogTasks = sortTasksLowestPriorityFirst preStrategyBacklog}
  where
    preStrategyBacklog = backlogTasks b