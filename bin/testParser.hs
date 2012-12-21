module Main where

import Language.Lojban.Parser
import System.Environment

main = do
	[str] <- getArgs
	print $ parse str
