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
    , getCGroup
    ) where

import System.Linux.CGroup.Types

import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem                as FS
import qualified Data.ByteString.Char8     as BS
import Data.Maybe

getRootCGroup :: IO (Maybe CGroup)
getRootCGroup = do
    mountedContent <- readFile "/proc/mounts"
    let list = mapMaybe decodeLine $ lines mountedContent
    putStrLn $ show list
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

getCGroup :: CGroup -> FS.FilePath -> Maybe CGroup
getCGroup group path
    | cgroupPath group == path                 = Just group
    | (FS.filename $ cgroupPath group) == path = Just group
    | otherwise =
        let splitedDir = FS.splitDirectories path
        in  findCGroup [group] splitedDir
    where
      findCGroup :: [CGroup] -> [FS.FilePath] -> Maybe CGroup
      findCGroup [] _ = Nothing
      findCGroup _ [] = Nothing
      findCGroup (cg:xs) [name] =
          if (FS.filename $ cgroupPath cg) == name then Just cg else findCGroup xs [name]
      findCGroup (cg:xs) (a:b) =
          if (FS.filename $ cgroupPath cg) == a then findCGroup (subCGroups cg) b
                                                else findCGroup xs (a:b)
