module Preprocess (preprocess) where

import Pos
import Parse
import System.Environment

import LexerPappy

preprocess :: String -> Maybe String
preprocess src = do
	(is, ws) <- lexwords src
	return $ is ++ wordsToString ws

wordsToString :: [(String, String)] -> String
wordsToString [] = ""
-- wordsToString (("zoi", _) :
wordsToString ((w, s) : rest) = w ++ s ++ wordsToString rest

lexwords :: String -> Maybe (String, [(String, String)])
lexwords src = case lexerPappytest_words $ lexerPappyParse "test_words" src of
	Parsed v _ _ -> Just v
	NoParse e -> Nothing

main = do
	[str] <- getArgs
	print $ preprocess str
