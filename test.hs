{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy
import Data.Maybe

[peggy|

text :: Paragraphs
	= paragraphs eof	{ $1 }

paragraphs :: Paragraphs
	= paragraph (niho paragraphs)?
		{ maybe (ParagraphsS $1) (ParagraphsM $1) $2 }

paragraph :: Paragraph
--	(([Prenex], Sentence), [((I, [[(UI, Maybe NAI)]]), ([Prenex], Sentence))])
	= statement (i_clause statement)*	{ Paragraph $1 $2 }

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
	= na? tanru_unit		{ Selbri $1 $2 }

tanru_unit :: TanruUnit -- [(Brivla, [[(UI, Maybe NAI)]])]
	= brivla_clause brivla_clause*	{ TUBrivla $ $1 : $2 }
	/ goha				{ TUGOhA $1 }

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

post_clause :: [[(UI, Maybe NAI)]]
	= indicators*

indicators :: [(UI, Maybe NAI)]
	= indicator+

indicator :: (UI, Maybe NAI)
	= ui nai?

terms_ :: [(FAhA, Sumti)]
	= term+

term :: (FAhA, Sumti)
	= tag sumti

tag :: FAhA
	= space_

space_ :: FAhA
	= faha

brivla_clause :: (Brivla, [[(UI, Maybe NAI)]])
	= brivla post_clause

i_clause :: (I, [[(UI, Maybe NAI)]])
	= i post_clause

brivla ::: Brivla
	= (noDoubleConsonant anyLetter)* doubleConsonant endWithVowel
		{ Brivla $ map snd $1 ++ $2 ++ $3 }

cmene ::: Cmene
	= consonant_final "."	{ Cmene $1 }

faha :: FAhA
	= "pa\`o"	{ PAhO }

goha :: GOhA
	= "co\'e"	{ COhE }

i ::: I
	= ".i" 		{ I }

koha ::: KOhA
	= "mi"		{ MI }
	/ "do"		{ DO }

la ::: LA
	= "la"		{ LA }

le ::: LE
	= "le"		{ LE }
	/ "lei"		{ LEI }

na :: NA
	= "na"		{ NA }

nai :: NAI
	= "nai"		{ NAI }

niho :: NIhO
	= "ni\'o"	{ NIhO }

ui ::: UI
	= "i\'a"	{ IhA }
	/ "o\'e"	{ OhE }
	/ "ku\'i"	{ KUhI }

zohu ::: ZOhU
	= "zo\'u"	{ ZOhU }

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

anything :: Char
	= [^ ] / [ ]

eof :: ()
	= [\n]? !anything	{ () }

|]

data Paragraphs
	= ParagraphsS Paragraph
	| ParagraphsM Paragraph (NIhO, Paragraphs) deriving Show
data Paragraph = Paragraph
	([Prenex], Sentence) [((I, [[(UI, Maybe NAI)]]), ([Prenex], Sentence))]
	deriving Show
data Statement = Statement [Prenex] Sentence
data Sentence = Sentence Selbri [Sumti] deriving Show

data TanruUnit
	= TUBrivla [(Brivla, [[(UI, Maybe NAI)]])]
	| TUGOhA GOhA
	deriving Show
data Selbri = Selbri (Maybe NA) TanruUnit deriving Show -- [( Brivla, [[(UI, Maybe NAI)]])] deriving Show
data Sumti
	= SKOhA KOhA
	| SCmene LA [Cmene]
	| SLA LA Selbri
	| SLE LE Selbri
	deriving Show
data Prenex = Prenex [Sumti] deriving Show

data Brivla = Brivla String deriving Show
data Cmene = Cmene String deriving Show

data FAhA = PAhO deriving Show
data GOhA = COhE deriving Show
data I = I deriving Show
data KOhA = MI | DO deriving Show
data LA = LA deriving Show
data LE = LE | LEI deriving Show
data NA = NA deriving Show
data NAI = NAI deriving Show
data NIhO = NIhO deriving Show
data UI = OhE | IhA | KUhI deriving Show
data ZOhU = ZOhU deriving Show

main :: IO ()
main = interact $ (++ "\n") . show . parseString text "<stdin>"
