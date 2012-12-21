module Language.Lojban.Parser (
	parse,
	ParseError,
	Text,
	Clause,

	Sentence(..),
	Sumti(..),
	TermsGikTerms(..),
	SumtiTail(..),
	Selbri(..),
	RelativeClause(..),
	Linkargs(..),
	Links(..),
	MexOperator(..),
	Operand(..),
	RPExpression(..),
	RPExpressionTail(..),
	Free(..),
	JoikJek(..),
	Gek(..),
	IntervalProperty(..),
	Tag(..),
	SpaceInterval(..),
	SpaceOffset(..),

	Indicators(..),
	Term(..),
	Gik(..),
	Gihek(..),
	Statement(..),
	Ek(..),
	Quantifier(..),
	Paragraph(..),
	Paragraphs(..),
	Pre(..),
	LerfuString(..),
	RelativeClauses(..),
	Mex(..),
	JoikEk(..),
	NumberLerfuString(..),
	Guhek(..),
	JoikJekSelbri5(..),
	Joik(..),
	Number(..),
	Tense(..)
) where

import TestPappy
import Parse
import Preprocess
import Data.Maybe

parse :: String -> Either ParseError Text
parse src = case testPappytest $ testPappyParse "test" p of
	Parsed v _ _ -> Right v
	NoParse e -> Left e
	where
	p = fromMaybe (error "can't lex") $ preprocess src
