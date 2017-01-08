module Main where

import Control.Monad (foldM)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Git
import Git (Branch (..), GitError (..))
import Output
import Paths_branch_deleter

main :: IO ()
main = do
  printWelcome

  bs <- Git.listBranches
  printInfoBold (Text.pack $ show (length bs) ++ " branches found.\n\n")

  foldM (\coll b -> askToDelete coll b) [] bs >>= confirmPlan

printWelcome :: IO ()
printWelcome = do
  welcomeFile <- getDataFileName "data/welcome-screen.txt"
  readFile welcomeFile >>= putStrLn

confirmPlan :: [(Bool, Branch)] -> IO ()
confirmPlan plans = showPlan plans >> confirmPlan'
  where
    confirmPlan' = do
      printNorm "Do you approve of this plan? (y/n)\n"
      answer <- getLine
      case answer of
        "y" -> mapM_ (deleteBranch . snd) (filter fst plans)
        _   -> printNorm "Exiting...\n"

showPlan :: [(Bool, Branch)] -> IO ()
showPlan = mapM_ drawRow
  where
    drawRow (True, branch)  = printInfo ("[x] " <> getBranchName branch <> "\n")
    drawRow (False, branch) = printInfo ("[ ] " <> getBranchName branch <> "\n")

askToDelete :: [(Bool, Branch)] -> Branch -> IO [(Bool, Branch)]
askToDelete collection branch = do
  printNorm "Delete " >> printInfoBold (getBranchName branch) >> printNorm "? (y/n)\n"
  answer <- getLine
  case answer of
    "y" -> return $ collection ++ [(True, branch)]
    _   -> return $ collection ++ [(False, branch)]

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
