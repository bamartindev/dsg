module DSG.Domain.TaskPriority
  ( TaskPriority(..)
  )
where

data TaskPriority = TaskPriorityNone
                  | TaskPriorityLow 
                  | TaskPriorityMedium 
                  | TaskPriorityHigh 
                  | TaskPriorityUrgent
  deriving (Eq, Ord)

instance Show TaskPriority where
  show TaskPriorityNone = "None"
  show TaskPriorityLow = "Low"
  show TaskPriorityMedium = "Medium"
  show TaskPriorityHigh = "High"
  show TaskPriorityUrgent = "Urgent"