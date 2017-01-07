module Main where

import Control.Monad (sequence_)
import qualified Data.Text as Text
import Git (Branch (..), GitError (..))
import qualified Git

confirmDeleteBranches :: IO ()
confirmDeleteBranches = do
  bs <- Git.listBranches
  putStrLn (show (length bs) ++ " branches found.")
  sequence_ $ map askToDelete bs

askToDelete :: Branch -> IO ()
askToDelete branch = do
  putStrLn ("Are you sure you want to delete " ++ Text.unpack (getBranchName branch) ++ "? (y/n)")
  answer <- getLine
  case answer of
    "y" -> deleteBranch branch
    _ -> return ()

askToForceDelete :: Branch -> IO ()
askToForceDelete branch = do
  putStrLn ("Would you like to force delete " ++ Text.unpack (getBranchName branch) ++ "? (y/n)")
  answer <- getLine
  case answer of
    "y" -> forceDeleteBranch branch
    _ -> return ()

deleteBranch :: Branch -> IO ()
deleteBranch branch = do
  result <- Git.deleteBranch branch
  case result of
    Right success -> putStrLn $ Text.unpack success
    Left (NotFullyMergedError msg) -> putStrLn (Text.unpack msg ++ "\n") >> askToForceDelete branch
    Left (UnhandledError msg) -> putStrLn $ Text.unpack msg

forceDeleteBranch :: Branch -> IO ()
forceDeleteBranch branch = do
  result <- Git.forceDeleteBranch branch
  case result of
    Right success -> putStrLn $ Text.unpack success
    Left gitError -> putStrLn $ show gitError

main :: IO ()
main = confirmDeleteBranches
