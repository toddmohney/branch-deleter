module Output where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.ANSI

printNorm :: Text -> IO ()
printNorm = putStr . Text.unpack

printInfo :: Text -> IO ()
printInfo txt = do
  setSGR [ SetColor Foreground Dull Green ]
  putStr (Text.unpack txt)
  setSGR [Reset]

printInfoBold :: Text -> IO ()
printInfoBold txt = do
  setSGR [ SetColor Foreground Dull Magenta ]
  putStr (Text.unpack txt)
  setSGR [Reset]

printWarn :: Text -> IO ()
printWarn txt = do
  setSGR [ SetColor Foreground Dull Yellow ]
  putStr (Text.unpack txt)
  setSGR [Reset]

printError :: Text -> IO ()
printError txt = do
  setSGR [ SetColor Foreground Dull Red ]
  putStr (Text.unpack txt)
  setSGR [Reset]
