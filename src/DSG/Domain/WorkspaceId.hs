module DSG.Domain.WorkspaceId 
  ( WorkspaceId(..)
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
import qualified Data.UUID as UUID
import Data.UUID (UUID)

newtype WorkspaceId = WorkspaceId { unwrapWorkspaceId :: UUID }
  deriving (Eq)

instance Show WorkspaceId where
  show t = show $ unwrapWorkspaceId t

instance ToField WorkspaceId where
  toField = SQLText . T.pack . show

instance FromField WorkspaceId where
  fromField f@(Field (SQLText a) _) = case UUID.fromString (T.unpack a)of
    Just id -> Ok (WorkspaceId id)
    Nothing -> returnError ConversionFailed f "failed to convert the stored value into a UUID"
  fromField f = returnError ConversionFailed f "need text to convert to UUID"