module XMonad.Prompt.Common (
  makeCompletionFunc,
  filterDirRecursive
  ) where

import XMonad.Prompt
import Control.Monad ( forM )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath ( (</>) )
import Data.List
import Data.Char

-- Real World Haskell
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topPath = do
  names <- getDirectoryContents topPath
  let
    properNames =
      filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

filterDirRecursive :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
filterDirRecursive pred dir  = do
  files <- getRecursiveContents dir
  return $ filter pred files

makeCompletionFunc :: IO [String] -> ComplFunction
makeCompletionFunc list input = do
  xs <- list
  return $ complFunction xs input

complFunction :: [String] -> String -> [String]
complFunction xs input = filter (\name -> and (fmap (`isInfixOf` lowerCase name)
                                               (lowerCase <$> words input))) xs

lowerCase :: String -> String
lowerCase = fmap toLower
