module Main where

import Control.Monad (sequence_)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Git (Branch (..), GitError (..))
import Output
import Paths_branch_deleter
import qualified Git

confirmDeleteBranches :: IO ()
confirmDeleteBranches = do
  bs <- Git.listBranches
  printInfoBold (Text.pack $ show (length bs) ++ " branches found.\n\n")
  sequence_ $ map askToDelete bs

askToDelete :: Branch -> IO ()
askToDelete branch = do
  printNorm "Delete " >> printInfoBold (getBranchName branch) >> printNorm "? (y/n)\n"
  answer <- getLine
  case answer of
    "y" -> deleteBranch branch
    _ -> return ()

askToForceDelete :: Branch -> IO ()
askToForceDelete branch = do
  printWarn "Force delete " >> printInfoBold (getBranchName branch) >> printWarn "? (y/n)\n"
  answer <- getLine
  case answer of
    "y" -> forceDeleteBranch branch
    _ -> return ()

deleteBranch :: Branch -> IO ()
deleteBranch branch = do
  result <- Git.deleteBranch branch
  case result of
    Right success -> printInfo success
    Left (NotFullyMergedError msg) -> printWarn (msg <> "\n") >> askToForceDelete branch
    Left (UnhandledError msg) -> printError msg

forceDeleteBranch :: Branch -> IO ()
forceDeleteBranch branch = do
  result <- Git.forceDeleteBranch branch
  case result of
    Right success -> printInfo success
    Left gitError -> printError (Git.getMsg gitError)

printWelcome :: IO ()
printWelcome = do
  welcomeFile <- getDataFileName "data/welcome-screen.txt"
  readFile welcomeFile >>= putStrLn

main :: IO ()
main = printWelcome >> confirmDeleteBranches
