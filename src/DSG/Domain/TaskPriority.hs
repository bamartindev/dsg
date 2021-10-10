module DSG.Domain.TaskPriority
  ( TaskPriority(..)
  )
where

import qualified Data.Text as T
import Database.SQLite.Simple
    ( SQLData(SQLText), ResultError(ConversionFailed) )
import Database.SQLite.Simple.FromField
    ( ResultError(ConversionFailed), returnError, FromField(..) )
import Database.SQLite.Simple.ToField ( ToField(..) )
import Database.SQLite.Simple.Internal ( Field(Field) )
import Database.SQLite.Simple.Ok ( Ok(Ok) )

data TaskPriority = TaskPriorityNone
                  | TaskPriorityLow 
                  | TaskPriorityMedium 
                  | TaskPriorityHigh 
                  | TaskPriorityUrgent
  deriving (Eq, Ord, Read)

instance Show TaskPriority where
  show TaskPriorityNone = "None"
  show TaskPriorityLow = "Low"
  show TaskPriorityMedium = "Medium"
  show TaskPriorityHigh = "High"
  show TaskPriorityUrgent = "Urgent"

instance ToField TaskPriority where
  toField = SQLText . T.pack . show

instance FromField TaskPriority where
  fromField (Field (SQLText "None") _)   = Ok TaskPriorityNone 
  fromField (Field (SQLText "Low") _)    = Ok TaskPriorityLow 
  fromField (Field (SQLText "Medium") _) = Ok TaskPriorityMedium 
  fromField (Field (SQLText "High") _)   = Ok TaskPriorityHigh 
  fromField (Field (SQLText "Urgent") _) = Ok TaskPriorityUrgent
  fromField f = returnError ConversionFailed f "need 'None', 'Low', 'Medium', 'High', or 'Urgent'"