{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy
import Data.Maybe

[peggy|

statement :: ([Prenex], Sentence)
	= sentence		{ ([], $1) }
	/ prenex statement	{ ($1 : fst $2, snd $2) }

sentence :: Sentence
	= terms? bridi_tail { Sentence (fst $2) $ fromMaybe [] $1 ++ snd $2 }

bridi_tail :: (Selbri, [Sumti])
	= selbri tail_terms

tail_terms :: [Sumti]
	= terms?	{ fromMaybe [] $1 }

selbri :: Selbri
	= tanru_unit

tanru_unit :: Selbri
	= brivla brivla*	{ Selbri $ $1 : $2 }

terms :: [Sumti]
	= sumti+

sumti :: Sumti
	= koha	{ SKOhA $1 }
	/ la cmene+	{ SCmene $1 $2 }
	/ la sumti_tail { SLA $1 $2 }
	/ le sumti_tail { SLE $1 $2 }

sumti_tail :: Selbri
	= selbri

prenex :: Prenex
	= terms zohu	{ Prenex $1 }

brivla ::: Brivla
	= (noDoubleConsonant anyLetter)* doubleConsonant endWithVowel
		{ Brivla $ map snd $1 ++ $2 ++ $3 }

cmene ::: Cmene
	= consonant_final "."	{ Cmene $1 }

i ::: I
	= ".i" { I }

koha ::: KOhA
	= "mi"	{ MI }
	/ "do"	{ DO }

la ::: LA
	= "la"	{ LA }

le ::: LE
	= "le"	{ LE }
	/ "lei"	{ LEI }

zohu :: ZOhU
	= "zo\'u" { ZOhU }

endWithVowel :: String
	= anyLetter endWithVowel	{ $1 : $2 }
	/ vowel				{ [$1] }

consonant_final :: String
	= anyLetter consonant_final	{ $1 : $2 }
	/ consonant			{ [$1] }

noDoubleConsonant :: ()
	= !(consonant consonant)	{ () }

doubleConsonant :: String
	= consonant consonant	{ $1 : [$2] }

anyLetter :: Char
	= consonant / vowel / symbol

notConsonant :: Char
	= vowel / symbol

consonant :: Char
	= [mpbfvszcjtdkgrlnxMPBFVSZCJTDKGRLNX]

vowel :: Char
	= [aeiouyAEIOU]

symbol :: Char
	= [',.]

|]

data Statement = Statement [Prenex] Sentence
data Sentence = Sentence Selbri [Sumti] deriving Show

data Selbri = Selbri [Brivla] deriving Show
data Sumti
	= SKOhA KOhA
	| SCmene LA [Cmene]
	| SLA LA Selbri
	| SLE LE Selbri
	deriving Show
data Prenex = Prenex [Sumti] deriving Show

data Brivla = Brivla String deriving Show
data Cmene = Cmene String deriving Show
data I = I deriving Show
data KOhA = MI | DO deriving Show
data LA = LA deriving Show
data LE = LE | LEI deriving Show
data ZOhU = ZOhU deriving Show

main :: IO ()
main = interact $ show . parseString statement "<stdin>"
