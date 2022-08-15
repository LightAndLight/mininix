module Store (MonadStore (..)) where

import Key (Key)

class Monad m => MonadStore key value m | m -> key value where
  keyPath :: Key -> m FilePath

  get :: key -> m (Maybe value)
  put :: value -> m key
  delete :: key -> m Bool

  getMemo :: Key -> m (Maybe Key)
  putMemo :: Key -> Key -> m ()