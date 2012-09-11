module Main where

import TestPappy
import System.Environment
import Preprocess
import Parse
import Data.Maybe

main = do
	[str] <- getArgs
	let p = fromMaybe (error "can't lex") $ preprocess str
	case testPappytest $ testPappyParse "test" p of
		Parsed v _ _ -> print v
		NoParse e -> error $ show e
