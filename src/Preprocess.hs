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
wordsToString (("zoi", s1) : (q, s2) : rest) =
	"zoi" ++ s1 ++ " \NUL" ++ s2 ++ quoteInside q rest
wordsToString ((w, s) : rest) = w ++ s ++ wordsToString rest

quoteInside :: String -> [(String, String)] -> String
quoteInside q0 ((q1, s) : rest)
	| q0 == q1 = "\NUL" ++ s ++ wordsToString rest
	| otherwise = q1 ++ s ++ quoteInside q0 rest

lexwords :: String -> Maybe (String, [(String, String)])
lexwords src = case lexerPappytest_words $ lexerPappyParse "test_words" src of
	Parsed v _ _ -> Just v
	NoParse e -> Nothing

main = do
	[str] <- getArgs
	print $ preprocess str
