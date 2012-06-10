{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy
import Data.Maybe

[peggy|

text :: Text
	= text_1 eof		{ $1 }

text_1 :: Text
	= paragraphs		{ TParagraphs $1 }
--	/ i free* text_1?	{ Text1 $1 $2 }

paragraphs :: Paragraphs
	= paragraph (niho paragraphs)?
		{ maybe (ParagraphsS $1) (ParagraphsM $1) $2 }

paragraph :: Paragraph
	= statement (i_clause free* statement?)*	{ Paragraph $1 $2 }

statement :: ([Prenex], Sentence)
	= statement_3

statement_3 :: ([Prenex], Sentence)
	= sentence		{ ([], $1) }
	/ prenex statement	{ ($1 : fst $2, snd $2) }

sentence :: Sentence
	= terms? cu? bridi_tail	{ Sentence (fst $3) $ fromMaybe [] $1 ++ snd $3 }

subsentence :: Subsentence
	= sentence		{ SSentence $1 }
	/ prenex subsentence	{ SSubsentence $1 $2 }

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

tanru_unit :: TanruUnit
	= tanru_unit_2

tanru_unit_2 :: TanruUnit
	= brivla_clause brivla_clause*	{ TUBrivla $ $1 : $2 }
	/ goha				{ TUGOhA $1 }
	/ se tanru_unit_2		{ TSE $1 $2 }
	/ nu subsentence kei?		{ TNU $1 $2 }

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
	= sumti_6 relative_clause
				{ SRelative $1 (fst $2) (snd $2) }
	/ sumti_6		{ $1 }
	/ quantifier sumti_6 relative_clause
				{ SQuantifier $1 $ SRelative $2 (fst $3) (snd $3) }
	/ quantifier sumti_6	{ SQuantifier $1 $2 }
	/ quantifier selbri	{ SQuantifier $1 $ SLE LO $2 }

sumti_6 :: Sumti
	= koha	{ SKOhA $1 }
	/ la cmene+	{ SCmene $1 $2 }
	/ la sumti_tail { let s = SLA $1 $ snd $2 in maybe s (SRelative s PE) $ fst $2 }
	/ le sumti_tail { let s = SLE $1 $ snd $2 in maybe s (SRelative s PE) $ fst $2 }
	/ li__clause	{ SMex $ snd $1 }

sumti_tail :: (Maybe Term, Selbri)
	= sumti_6? sumti_tail_1		{ (fmap TSumti $1, $2) }

sumti_tail_1 :: Selbri
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
	/ bai		{ TBAI $1 }

time :: PU
	= pu

space_ :: FAhA
	= faha

joik_ek :: A
	= ek

ek :: A
	= a

li__clause :: (LI, PA)
	= li mex

mex :: PA
	= quantifier

quantifier :: PA
	= pa

free :: Free
	= vocative relative_clause? selbri
			{ FVocativeSelbri $1 $2 $3 }

vocative :: DOI
	= doi

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

bai ::: BAI
	= "la\'u"	{ LAhU }

cu ::: CU
	= "cu"		{ CU }

doi :: DOI
	= "doi"		{ DOI }

faha :: FAhA
	= "pa\'o"	{ PAhO }
	/ "to\'o"	{ TOhO }

goha :: GOhA
	= "co\'e"	{ COhE }

goi :: GOI
	= "pe"		{ PE }

i ::: I
	= ".i" 		{ I }

kei :: KEI
	= "kei"		{ KEI }

koha ::: KOhA
	= "mi"		{ MI }
	/ "do"		{ DO }

la ::: LA
	= "la"		{ LA }

le ::: LE
	= "le"		{ LE }
	/ "lo"		{ LO }
	/ "lei"		{ LEI }

li ::: LI
	= "li"		{ LI }

na :: NA
	= "na"		{ NA }

nai :: NAI
	= "nai"		{ NAI }

niho :: NIhO
	= "ni\'o"	{ NIhO }

nu :: NU
	= 'nu'		{ NU }
	/ 'ni'		{ NI }
	/ "du\'u"	{ DUhU }

pa :: PA
	= "so\'i"	{ SOhI }
	/ "so\'o"	{ SOhO }

pu :: PU
	= "ca"		{ CA }
	/ "ba"		{ BA }
	/ "pu"		{ PU }

se :: SE
	= "te"		{ TE }

ui ::: UI
	= ".i\'a"	{ IhA }
	/ ".o\'e"	{ OhE }
	/ "ku\'i"	{ KUhI }
	/ ".oi"		{ OI }

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

data Text
	= TParagraphs Paragraphs
	| Text1 I [Free] Text
	deriving Show

data Paragraphs
	= ParagraphsS Paragraph
	| ParagraphsM Paragraph (NIhO, Paragraphs) deriving Show
data Paragraph = Paragraph
	([Prenex], Sentence) [(
		(I, [[(UI, Maybe NAI)]]),
		[Free],
		Maybe ([Prenex], Sentence))]
	deriving Show
data Statement = Statement [Prenex] Sentence
data Sentence = Sentence Selbri [Term] deriving Show
data Subsentence
	= SSentence Sentence
	| SSubsentence Prenex Subsentence
	deriving Show

data TanruUnit
	= TUBrivla [(Brivla, [[(UI, Maybe NAI)]])]
	| TUGOhA GOhA
	| TNU NU Subsentence
	| TSE SE TanruUnit
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
	| SQuantifier PA Sumti
	| SMex PA
	deriving Show
data Tense
	= TFAhA FAhA
	| TPU PU
	| TBAI BAI
	deriving Show
data Prenex = Prenex [Term] deriving Show
data Free
	= FVocativeSelbri DOI (Maybe (GOI, Term)) Selbri
	deriving Show

data Brivla = Brivla String deriving Show
data Cmene = Cmene String deriving Show

data A = E deriving Show
data BAI = LAhU deriving Show
data CU = CU deriving Show
data DOI = DOI deriving Show
data FAhA = PAhO | TOhO deriving Show
data GOhA = COhE deriving Show
data GOI = PE deriving Show
data I = I deriving Show
data KEI = KEI deriving Show
data KOhA = MI | DO deriving Show
data LA = LA deriving Show
data LE = LE | LO | LEI deriving Show
data LI = LI deriving Show
data NA = NA deriving Show
data NAI = NAI deriving Show
data NIhO = NIhO deriving Show
data NU = NU | NI | DUhU deriving Show
data PA = SOhI | SOhO deriving Show
data PU = PU | CA | BA deriving Show
data SE = TE deriving Show
data UI = IhA | KUhI | OhE | OI deriving Show
data ZOhU = ZOhU deriving Show

main :: IO ()
main = interact $ (++ "\n") . show . parseString text "<stdin>"
