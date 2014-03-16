-- |
-- Module      : System.Linux.CGroup.Types
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
module System.Linux.CGroup.Types
    (CGroup(..)
    ) where

import qualified Filesystem.Path.CurrentOS as FS (FilePath)

data CGroup = CGroup
    { cgroupPath :: FS.FilePath
    , subCGroups :: [CGroup]
    } deriving (Show, Eq, Ord)
