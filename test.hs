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

bridi_tail :: (Selbri, [Term])
	= selbri tail_terms

tail_terms :: [Term]
	= terms?	{ fromMaybe [] $1 }

selbri :: Selbri
	= selbri_1
	/ tag selbri_1		{ TagSelbri $1 $2 }

selbri_1 :: Selbri
	= selbri_2
	/ na selbri		{ NotSelbri $2 }

selbri_2 :: Selbri
	= selbri_6

selbri_6 :: Selbri
	= tanru_unit			{ Selbri $1 }

tanru_unit :: TanruUnit -- [(Brivla, [[(UI, Maybe NAI)]])]
	= brivla_clause brivla_clause*	{ TUBrivla $ $1 : $2 }
	/ goha				{ TUGOhA $1 }

terms :: [Term]
	= term+

term :: Term
	= sumti		{ TSumti $1 }
	/ tag sumti	{ TTense $1 $2 }

sumti :: Sumti
	= sumti_2

sumti_2 :: Sumti
	= sumti_3 (joik_ek sumti_3)*	{ SLConnect $1 $2 }

sumti_3 :: Sumti
	= sumti_5

sumti_5 :: Sumti
	= sumti_6 relative_clause	{ SRelative $1 (fst $2) (snd $2) }
	/ sumti_6			{ $1 }

sumti_6 :: Sumti
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

tag :: Tense
	= tense_modal

tense_modal :: Tense
	= space_	{ TFAhA $1 }
	/ time		{ TPU $1 }

time :: PU
	= pu

space_ :: FAhA
	= faha

joik_ek :: A
	= ek

ek :: A
	= a

relative_clause :: (GOI, Term)
	= goi term

brivla_clause :: (Brivla, [[(UI, Maybe NAI)]])
	= brivla post_clause

i_clause :: (I, [[(UI, Maybe NAI)]])
	= i post_clause

brivla ::: Brivla
	= (noDoubleConsonant anyLetter)* doubleConsonant endWithVowel
		{ Brivla $ map snd $1 ++ $2 ++ $3 }

cmene ::: Cmene
	= consonant_final "."	{ Cmene $1 }

a ::: A
	= ".e"		{ E }

faha :: FAhA
	= "pa\'o"	{ PAhO }
	/ "to\'o"	{ TOhO }

goha :: GOhA
	= "co\'e"	{ COhE }

goi :: GOI
	= "pe"		{ PE }

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

pu :: PU
	= "ca"		{ CA }

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
data Sentence = Sentence Selbri [Term] deriving Show

data TanruUnit
	= TUBrivla [(Brivla, [[(UI, Maybe NAI)]])]
	| TUGOhA GOhA
	deriving Show
data Selbri
	= Selbri TanruUnit
	| NotSelbri Selbri
	| TagSelbri Tense Selbri
	deriving Show
data Term
	= TSumti Sumti
	| TTense Tense Sumti
	deriving Show
data Sumti
	= SKOhA KOhA
	| SCmene LA [Cmene]
	| SLA LA Selbri
	| SLE LE Selbri
	| SLConnect Sumti [(A, Sumti)]
	| SRelative Sumti GOI Term
	deriving Show
data Tense
	= TFAhA FAhA
	| TPU PU
	deriving Show
data Prenex = Prenex [Term] deriving Show

data Brivla = Brivla String deriving Show
data Cmene = Cmene String deriving Show

data A = E deriving Show
data FAhA = PAhO | TOhO deriving Show
data GOhA = COhE deriving Show
data GOI = PE deriving Show
data I = I deriving Show
data KOhA = MI | DO deriving Show
data LA = LA deriving Show
data LE = LE | LEI deriving Show
data NA = NA deriving Show
data NAI = NAI deriving Show
data NIhO = NIhO deriving Show
data PU = CA deriving Show
data UI = OhE | IhA | KUhI deriving Show
data ZOhU = ZOhU deriving Show

main :: IO ()
main = interact $ (++ "\n") . show . parseString text "<stdin>"
