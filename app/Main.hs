module Main where

import Control.Monad (join, sequence_)
import Data.Text (Text)
import qualified Data.Text as Text
import Lib
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

data Branch = Current Text
            | Local Text
            | Remote Text
  deriving (Show)

data GitError = NotFullyMergedError { getMsg :: Text }
              | UnhandledError { getMsg :: Text }
  deriving (Show)

getBranchName :: Branch -> Text
getBranchName (Current b) = b
getBranchName (Local b) = b
getBranchName (Remote b) = b

confirmDeleteBranches :: IO ()
confirmDeleteBranches = do
  bs <- branches
  sequence_ $ map askToDelete bs

askToDelete :: Branch -> IO ()
askToDelete branch = do
  putStrLn $ "Are you sure you want to delete " ++ (Text.unpack $ getBranchName branch) ++ "? (y/n)"
  answer <- getLine
  case answer of
    "y" -> deleteBranch branch
    "n" -> return ()

askToForceDelete :: Branch -> IO ()
askToForceDelete branch = do
  putStrLn $ "Would you like to force delete " ++ (Text.unpack $ getBranchName branch) ++ "? (y/n)"
  answer <- getLine
  case answer of
    "y" -> forceDeleteBranch branch
    "n" -> return ()

deleteBranch :: Branch -> IO ()
deleteBranch (Current _) = putStrLn "Cannot delete current branch. Skipping..."
deleteBranch (Remote _) = putStrLn "Deleting remote branches not yet supported. Skipping..."
deleteBranch (Local branch) = do
  result <- gitCommand ["branch", "-d", branch]
  case parseResult result of
    Right success -> putStrLn $ Text.unpack success
    Left (NotFullyMergedError msg) -> putStrLn (Text.unpack msg ++ "\n") >> askToForceDelete (Local branch)
    Left (UnhandledError msg) -> putStrLn $ Text.unpack msg

forceDeleteBranch :: Branch -> IO ()
forceDeleteBranch (Current _) = putStrLn "Cannot force delete current branch. Skipping..."
forceDeleteBranch (Remote _) = putStrLn "Force deleting remote branches not yet supported. Skipping..."
forceDeleteBranch (Local branch) = do
  result <- gitCommand ["branch", "-D", branch]
  case parseResult result of
    Right success -> putStrLn $ Text.unpack success
    Left gitError -> putStrLn $ show gitError

branches :: IO [Branch]
branches = do
  result <- gitCommand ["branch"]
  case parseResult result of
    Right success -> return $ map mkBranch (map Text.strip (Text.lines success))
    Left gitError -> putStrLn (Text.unpack (getMsg gitError)) >> return []

mkBranch :: Text -> Branch
mkBranch branchName
  | Text.isPrefixOf "*" branchName      = Current branchName
  | Text.isPrefixOf "remote" branchName = Remote branchName
  | otherwise                           = Local branchName

gitCommand :: [Text] -> IO (ExitCode, String, String)
gitCommand args = readProcessWithExitCode "git" (map Text.unpack args) ""

parseResult :: (ExitCode, String, String) -> Either GitError Text
parseResult (ExitSuccess, stdout, stderr) = Right(Text.pack $ stderr ++ stdout)
parseResult (ExitFailure code, stdout, stderr)
  | Text.isInfixOf "not fully merged" (Text.pack stderr) = Left $ NotFullyMergedError "This branch is not fully merged."
  | otherwise = Left $ UnhandledError (Text.pack $ (show code) ++ " - " ++ stderr ++ stdout)

main :: IO ()
main = confirmDeleteBranches
