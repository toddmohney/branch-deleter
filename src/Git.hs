module Git (
  Branch (..)
, GitError (..)
, deleteBranch
, forceDeleteBranch
, listBranches
) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

data Branch = Current { getBranchName :: Text }
            | Local { getBranchName :: Text }
            | Remote { getBranchName :: Text }
  deriving (Show)

data GitError = NotFullyMergedError { getMsg :: Text }
              | UnhandledError { getMsg :: Text }
  deriving (Show)

listBranches :: IO [Branch]
listBranches = do
  result <- fmap parseResult $ gitCommand ["branch", "-a"]
  case result of
    Right success -> return $ map mkBranch (map Text.strip (Text.lines success))
    Left gitError -> putStrLn (Text.unpack (getMsg gitError)) >> return []

deleteBranch :: Branch -> IO (Either GitError Text)
deleteBranch branch = deleteBranch' branch False

forceDeleteBranch :: Branch -> IO (Either GitError Text)
forceDeleteBranch branch = deleteBranch' branch True

deleteBranch' :: Branch -> Bool -> IO (Either GitError Text)
deleteBranch' (Current _) _ = return $ Left (UnhandledError "Politely refusing to delete the current branch. Sorry.")
deleteBranch' b@(Local _) force = fmap parseResult $ gitCommand (deleteBranchCommand b force)
deleteBranch' (Remote branch) _ = fmap parseResult $ gitCommand ["push", "--delete", remoteName, branchName]
  where
    remoteName = fst $ tokenizeBranchName branch
    branchName = snd $ tokenizeBranchName branch

deleteBranchCommand :: Branch -> Bool -> [Text]
deleteBranchCommand branch True = ["branch", "-D", getBranchName branch]
deleteBranchCommand branch False = ["branch", "-d", getBranchName branch]

mkBranch :: Text -> Branch
mkBranch branchName
  | Text.isPrefixOf "*" branchName      = Current branchName
  | Text.isPrefixOf "remotes" branchName = Remote branchName
  | otherwise                           = Local branchName

tokenizeBranchName :: Text -> (Text, Text)
tokenizeBranchName name = case Text.splitOn "/" name of
  ("remotes":remoteName:branchName) -> (remoteName, (Text.unwords branchName))
  (branchName) -> ("", (Text.unwords branchName))

gitCommand :: [Text] -> IO (ExitCode, String, String)
gitCommand args =readProcessWithExitCode "git" (map Text.unpack args) ""

parseResult :: (ExitCode, String, String) -> Either GitError Text
parseResult (ExitSuccess, stdout, stderr) = Right(Text.pack $ stderr ++ stdout)
parseResult (ExitFailure code, stdout, stderr)
  | Text.isInfixOf "not fully merged" (Text.pack stderr) = Left $ NotFullyMergedError "This branch is not fully merged."
  | otherwise = Left $ UnhandledError (Text.pack $ (show code) ++ " - " ++ stderr ++ stdout)
