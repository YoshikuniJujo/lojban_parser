{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Parser(
) where

import Lexer
import Text.Peggy

import Data.Maybe

[peggy|

-- ****************
-- Free
-- ****************

-- ****************
-- ZEI and BU
-- ****************

-- pre_zei_bu :: 

-- ****************
-- Spaces, Si and UI
-- ****************
--	Spaces including '.y'
--	be eaten *after* a word.

indicators :: Indicators = _UI		{ Indicators }

post_clause :: [Indicators] = indicators*

-- _NAI_clause_ind

-- ****************
-- BAhE
-- ****************
--	be eaten *before* a word.

-- ****************
-- SELMAHO Clause
-- ****************

|]

main :: IO ()
main = do
	putStrLn "yet"

data Indicators
	= Indicators
	deriving Show
