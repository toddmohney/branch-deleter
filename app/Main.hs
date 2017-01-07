module Main where

import Control.Monad (sequence_)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Git (Branch (..), GitError (..))
import qualified Git
import System.Console.ANSI as Term

confirmDeleteBranches :: IO ()
confirmDeleteBranches = do
  bs <- Git.listBranches
  printInfo (Text.pack $ show (length bs) ++ " branches found.")
  sequence_ $ map askToDelete bs

askToDelete :: Branch -> IO ()
askToDelete branch = do
  printInfo ("Delete " <> getBranchName branch <> "? (y/n)")
  answer <- getLine
  case answer of
    "y" -> deleteBranch branch
    _ -> return ()

askToForceDelete :: Branch -> IO ()
askToForceDelete branch = do
  printWarn ("Force delete " <> getBranchName branch <> "? (y/n)")
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

printInfo :: Text -> IO ()
printInfo txt = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Vivid Green
         ]
  putStrLn (Text.unpack txt)
  setSGR []

printWarn :: Text -> IO ()
printWarn txt = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Vivid Yellow
         ]
  putStrLn (Text.unpack txt)
  setSGR []

printError :: Text -> IO ()
printError txt = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Vivid Red
         ]
  putStrLn (Text.unpack txt)
  setSGR []

main :: IO ()
main = confirmDeleteBranches
