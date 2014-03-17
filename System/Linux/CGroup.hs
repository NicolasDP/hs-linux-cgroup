{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : System.Linux.CGroup
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
module System.Linux.CGroup
    ( CGroup(..)
    , getRootCGroup
    , findCGroup
    , listTasks
    ) where

import System.Linux.CGroup.Types

import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem                as FS
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.UTF8      as BS (toString)
import Data.Maybe
import System.Posix.Types(ProcessID)

-- | retrieve the mount point of the CGroup file system
getRootCGroup :: IO (Maybe CGroup)
getRootCGroup = do
    mountedContent <- readFile "/proc/mounts"
    let list = mapMaybe decodeLine $ lines mountedContent
    case list of
        []     -> return Nothing
        [a]    -> buildCGroupTree a
        (a:_)  -> buildCGroupTree a -- TODO: manage that case?
    where
      decodeLine l = case words l of
          ("cgroup":b:_) -> Just $ FS.decodeString b
          _              -> Nothing

      buildCGroupTree :: FS.FilePath -> IO (Maybe CGroup)
      buildCGroupTree path = do
          testIsDir <- FS.isDirectory path
          if not testIsDir then return Nothing
                           else do
              listContents <- FS.listDirectory path
              subCGroups <- mapM buildCGroupTree listContents
              return $ Just $ CGroup path $ mapMaybe id subCGroups

-- | find a CGroup in the given CGroup Tree.
findCGroup :: CGroup       -- ^ CGroup Tree ("/" is the main CGROUP)
           -> FS.FilePath  -- ^ an absolute file path
           -> Either String CGroup -- ^ maybe the finded CGroup
findCGroup group path
    | (FS.absolute path) =
        let splitedDir = FS.splitDirectories path
        in  case splitedDir of
                [_]   -> Right group
                (a:b) -> findSubCGroup (subCGroups group) b
                _     -> Left $ "unable to find path: " ++ (FS.encodeString path)
    | otherwise = Left $ "filepath not supported: " ++ (FS.encodeString path)
    where
      findSubCGroup :: [CGroup] -> [FS.FilePath] -> Either String CGroup
      findSubCGroup [] l = Left $ "not subCGroup named: " ++ (FS.encodeString $ FS.concat l)
      findSubCGroup _ [] = Left "CGroup not found"
      findSubCGroup (cg:xs) [name] =
          if (FS.filename $ cgroupPath cg) == name then Right cg else findSubCGroup xs [name]
      findSubCGroup (cg:xs) (a:b) =
          if (FS.filename $ cgroupPath cg) == a then findSubCGroup (subCGroups cg) b
                                                else findSubCGroup xs (a:b)

listTasks :: CGroup -> IO [ProcessID]
listTasks cg = do
    content <- readFile $ FS.encodeString $ (cgroupPath cg) FS.</> "tasks"
    return $ map (read) (lines content)
