{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy
import Data.Maybe
import Data.Char
import Data.List

[peggy|

whole :: Text
	= text eof			{ $1 }

text :: Text
	= text_part_2 text_1 eof?	{ TIndicator $1 $2 }

text_part_2 :: Maybe [Indicator]
	= indicators?

text_1 :: Text
	= i text_1?			{ Text1 $2 }
	/ niho+ free* paragraphs?	{ TNIhO $2 $3 }
	/ paragraphs			{ TParagraphs $1 }

paragraphs :: Paragraphs
	= paragraph (niho+ free* paragraphs)?
		{ maybe (ParagraphsS $1) (ParagraphsM $1) $2 }

paragraph :: Paragraph
	= statement (i_clause free* statement?)*	{ Paragraph $1 $2 }

statement :: Statement
	= statement_2

statement_2 :: Statement
	= statement_3 (i_clause stag? bo statement_2)?	{ SBO $1 $2 }

statement_3 :: Statement
	= sentence		{ Statement [] $1 }
	/ prenex statement	{ let Statement p b = $2 in
					Statement ($1 : p) b }

sentence :: Sentence
	= terms? cu? bridi_tail	{ Sentence (fromMaybe [] $1) $3 }

subsentence :: Subsentence
	= sentence		{ SSentence $1 }
	/ prenex subsentence	{ SSubsentence $1 $2 }

bridi_tail :: BridiTail
	= bridi_tail_1

bridi_tail_1 :: BridiTail
	= bridi_tail_2

bridi_tail_2 :: BridiTail
	= bridi_tail_3 (gihek bridi_tail_2)? { BTBridiTail $1 $2 }

bridi_tail_3 :: BridiTail
	= selbri tail_terms	{ BTSelbri $1 $2 }

gihek :: (GIhA, Maybe NAI)
	= giha nai?

tail_terms :: Term
	= terms? free*	{ TFree (fromMaybe [] $1) $2 }

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
	= tanru_unit_1 tanru_unit_1*	{ TUTanruUnit $ $1 : $2 }

tanru_unit_1 :: TanruUnit
	= tanru_unit_2 linkargs		{ TULinkargs $1 $2 }
	/ tanru_unit_2			{ $1 }

tanru_unit_2 :: TanruUnit
	= brivla_clause free*		{ TUBrivla $1 $2 }
	/ goha				{ TUGOhA $1 }
	/ me sumti			{ TME $2 }
	/ number moi			{ TMOI $1 $2 }
	/ se tanru_unit_2		{ TSE $1 $2 }
	/ jai tag? tanru_unit_2		{ TJAI $1 $2 $3 }
	/ nu_clause free* subsentence kei?	{ TNU $1 $2 $3 }

terms :: [Term]
	= term+

term :: Term
	= term_1

term_1 :: Term
	= sumti		{ TSumti $1 }
	/ tag sumti	{ TTense $1 $ Just $2 }
	/ tag ku?	{ TTense $1 Nothing }
	/ fa sumti	{ TFA $1 $2 }

sumti :: Sumti
	= sumti_2

sumti_2 :: Sumti
	= sumti_3 (joik_ek sumti_3)*	{ SLConnect $1 $2 }

sumti_3 :: Sumti
	= sumti_5

sumti_5 :: Sumti
	= sumti_6 relative_clause
				{ SRelative $1 $2 }
	/ sumti_6		{ $1 }
	/ quantifier sumti_6 relative_clause
				{ SQuantifier $1 $ SRelative $2 $3 }
	/ quantifier sumti_6	{ SQuantifier $1 $2 }
	/ quantifier selbri	{ SQuantifier $1 $ SLE LO [] $2 }

sumti_6 :: Sumti
	= zoi_clause	{ $1 }
	/ lerfu_string	{ SLerfu $1 }
	/ lu text lihu	{ SLU $1 $2 }
	/ koha		{ SKOhA $1 }
	/ la cmene+	{ SCmene $1 $2 }
	/ la sumti_tail
		{ case fst $2 of
			Nothing -> SLA $1 $ snd $2
			Just t -> SRelative (SLA $1 $ snd $2) $ RCGOI PE t }
	/ le_clause sumti_tail
		{ case fst $2 of
			Nothing -> SLE (fst $1) (snd $1) $ snd $2
			Just t -> SRelative (SLE (fst $1) (snd $1) $ snd $2) $ RCGOI PE t }
	/ li__clause	{ SMex $ snd $1 }

sumti_tail :: (Maybe Term, Selbri)
	= sumti_6? sumti_tail_1		{ (fmap TSumti $1, $2) }

sumti_tail_1 :: Selbri
	= selbri

prenex :: Prenex
	= terms zohu	{ Prenex $1 }

post_clause :: [[Indicator]]
	= indicators*

indicators :: [Indicator]
	= indicator+

indicator :: Indicator
	= ui nai?	{ IUI $1 $2 }
	/ cai nai?	{ ICAI $1 $2 }

tag :: Tense
	= tense_modal

stag :: Tense
	= simple_tense_modal

tense_modal :: Tense
	= simple_tense_modal

simple_tense_modal :: Tense
	= space_	{ TFAhA $1 }
	/ time		{ TTime $1 }
	/ se? bai	{ TBAI $1 $2 }

time :: Time
	= zi time_offset*
			{ TZI $1 $2 }
	/ time_offset+	{ TTimeOffset $1 }
	/ pu		{ TPU $1 }
	/ zeha		{ TZEhA $1 }
	/ interval_property
			{ TIntervalProperty $1 }

time_offset :: TimeOffset
	= pu zi?	{ TimeOffset $1 $2 }

space_ :: FAhA
	= faha

joik_ek :: (A, [[Indicator]])
	= ek

ek :: (A, [[Indicator]])
	= a_clause

linkargs :: Term
	= linkargs_1

linkargs_1 :: Term
	= be term beho?	{ $2 }

li__clause :: (LI, Quantifier)
	= li mex

mex :: Quantifier
	= quantifier

quantifier :: Quantifier
	= number boi?	{ QBOI $1 $2 }

interval_property :: IntervalProperty
	= zaho		{ IPZAhO $1 }
	/ number roi	{ IPROI $1 $2 }

number :: PA
	= pa

free :: Free
	= vocative relative_clause? selbri
			{ FVocativeSelbri $1 $2 $3 }
	/ number mai	{ FMAI $1 $2 }
	/ to text toi?	{ FTO $2 }

vocative :: DOI
	= doi

relative_clause :: RelativeClause
	= goi term		{ RCGOI $1 $2 }
	/ noi subsentence	{ RCNOI $1 $2 }

lerfu_string :: [BY]
	= lerfu_word+

lerfu_word :: BY
	= by

a_clause :: (A, [[Indicator]])
	= a post_clause

brivla_clause :: (Brivla, [[Indicator]])
	= brivla post_clause

i_clause :: (I, [[Indicator]])
	= i post_clause

le_clause :: (LE, [[Indicator]])
	= le post_clause

zoi_clause :: Sumti
	= zoi nullt notNull* nullt	{ SZOI $1 $3 }

nu_clause :: NU
	= nu indicators?		{ $1 }

brivla ::: Brivla
	= (noDoubleConsonant anyLetter)* doubleConsonant endWithVowel
		{ Brivla $ map snd $1 ++ $2 ++ $3 }

cmene ::: Cmene
	= consonant_final "."	{ Cmene $1 }

a ::: A
	= ".e"		{ E }

bai ::: BAI
	= "la\'u"	{ LAhU }
	/ "gau"		{ GAU }
	/ "tai"		{ TAI }
	/ "va\'o"	{ VAhO }
	/ "fau"		{ FAU }
	/ "mu\'i"	{ MUhI }
	/ "pi\'o"	{ PIhO }
	/ "ki\'u"	{ KIhU }
	/ "zu\'e"	{ ZUhE }

be ::: BE
	= "be"		{ BE }

beho :: BEhO
	= "be\'o"	{ BEhO }

bo ::: BO
	= "bo"		{ BO }

boi ::: BOI
	= "boi"		{ BOI }

by ::: BY
	= "py"		{ PY }
	/ "ly"		{ LY }
	/ "ky"		{ KY }
	/ "sy"		{ SY }

cai :: CAI
	= "sai"		{ SAI }

cu ::: CU
	= "cu"		{ CU }

doi ::: DOI
	= "doi"		{ DOI }

fa ::: FA
	= "fa"		{ FA }
	/ "fe"		{ FE }
	/ "fi"		{ FI }
	/ "fo"		{ FO }
	/ "fai"		{ FAI }

faha ::: FAhA
	= "pa\'o"	{ PAhO }
	/ "to\'o"	{ TOhO }
	/ "bu\'u"	{ BUhU }

faho :: FAhO
	= "fa\'o"	{ FAhO }

giha ::: GIhA
	= "gi\'e"	{ GIhE }

goha ::: GOhA
	= "co\'e"	{ COhE }

goi ::: GOI
	= "pe"		{ PE }

i ::: I
	= ".i" 		{ I }

jai ::: JAI
	= "jai"		{ JAI }

kei ::: KEI
	= "kei"		{ KEI }

koha ::: KOhA
	= "mi"		{ MI }
	/ "do"		{ DO }
	/ "ko"		{ KO }
	/ "dei"		{ DEI }
	/ "da"		{ DA }
	/ "ke\'a"	{ KEhA }
	/ "di\'e"	{ DIhE }
	/ "ma"		{ MA }

ku :: KU
	= "ku"		{ KU }

la ::: LA
	= "la"		{ LA }

le ::: LE
	= "le"		{ LE }
	/ "lo"		{ LO }
	/ "lei"		{ LEI }

li ::: LI
	= "li"		{ LI }

lihu :: LIhU
	= "li\'u"	{ LIhU }

lu :: LU
	= "lu"		{ LU }

mai :: MAI
	= "mo\'o"	{ MOhO }

me :: ME
	= "me"		{ ME }

moi :: MOI
	= "moi"		{ MOI }

na ::: NA
	= "na"		{ NA }
	/ "ja\'a"	{ JAhA }

nai ::: NAI
	= "nai"		{ NAI }

niho ::: NIhO
	= "ni\'o"	{ NIhO }

noi ::: NOI
	= "noi"		{ NOI }
	/ "poi"		{ POI }

nu ::: NU
	= 'nu'		{ NU }
	/ 'ni'		{ NI }
	/ "du\'u"	{ DUhU }

pa ::: PA
	= "pa"		{ PA }
	/ "re"		{ RE }
	/ "ci"		{ CI }
	/ "xa"		{ XA }
	/ "no"		{ NO }
	/ "so\'i"	{ SOhI }
	/ "so\'o"	{ SOhO }
	/ "ro"		{ RO }
	/ "so\'u"	{ SOhU }

pu ::: PU
	= "ca"		{ CA }
	/ "ba" !"\'"	{ BA }
	/ "pu"		{ PU }

roi ::: ROI
	= "roi"		{ ROI }

se ::: SE
	= "se"		{ SE }
	/ "te"		{ TE }
	/ "ve"		{ VE }

to ::: TO
	= "to"		{ TO }

toi ::: TOI
	= "toi"		{ TOI }

ui ::: UI
	= ".i\'a"	{ IhA }
	/ ".o\'e"	{ OhE }
	/ "ku\'i"	{ KUhI }
	/ ".oi"		{ OI }
	/ ".e\'u"	{ EhU }
	/ "bi\'u"	{ BIhU }
	/ "ji\'a"	{ JIhA }
	/ "ja\'o"	{ JAhO }
	/ "xu"		{ XU }
	/ "kau"		{ KAU }
	/ ".ue"		{ UE }

zoi :: ZOI
	= "zoi"		{ ZOI }
	/ "la\'o"	{ LAhO }

zaho ::: ZAhO
	= "ba\'o"	{ BAhO }
	/ "de\'a"	{ DEhA }

zeha ::: ZEhA
	= "ze\'a"	{ ZEhA }

zi :: ZI
	= "za"		{ ZA }

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

nullt ::: ()
	= nullc

nullc :: ()
	= "\x00"

notNull :: Char
	= !"\x00" .

anything :: Char
	= [^ ] / [ ]

eof :: ()
	= [\n]? !anything	{ () }

|]

data Text
	= TParagraphs Paragraphs
	| Text1 (Maybe Text)
	| TNIhO [Free] (Maybe Paragraphs)
	| TIndicator (Maybe [Indicator]) Text
	deriving Show

data Paragraphs
	= ParagraphsS Paragraph
	| ParagraphsM Paragraph ([NIhO], [Free], Paragraphs) deriving Show
data Paragraph = Paragraph
	Statement [(
		(I, [[Indicator]]),
		[Free],
		Maybe Statement)]
	deriving Show
data Statement
	= Statement [Prenex] Sentence
	| SBO Statement (Maybe ((I, [[Indicator]]), Maybe Tense, BO, Statement))
	deriving Show
data Sentence = Sentence [Term] BridiTail deriving Show
data Subsentence
	= SSentence Sentence
	| SSubsentence Prenex Subsentence
	deriving Show

data TanruUnit
	= TUBrivla (Brivla, [[Indicator]]) [Free]
	| TUGOhA GOhA
	| TNU NU [Free] Subsentence
	| TMOI PA MOI
	| TSE SE TanruUnit
	| TUTanruUnit [TanruUnit]
	| TULinkargs TanruUnit Term
	| TME Sumti
	| TJAI JAI (Maybe Tense) TanruUnit
	deriving Show
data BridiTail
	= BTSelbri Selbri Term
	| BTBridiTail BridiTail (Maybe ((GIhA, Maybe NAI), BridiTail))
	deriving Show
data Selbri
	= Selbri TanruUnit
	| NotSelbri Selbri
	| TagSelbri Tense Selbri
	deriving Show
data Term
	= TSumti Sumti
	| TTense Tense (Maybe Sumti)
	| TFA FA Sumti
	| TFree [Term] [Free]
	deriving Show
data Sumti
	= SKOhA KOhA
	| SCmene LA [Cmene]
	| SLA LA Selbri
	| SLE LE [[Indicator]] Selbri
	| SLConnect Sumti [((A, [[Indicator]]), Sumti)]
	| SRelative Sumti RelativeClause
	| SQuantifier Quantifier Sumti
	| SMex Quantifier
	| SZOI ZOI String
	| SLU LU Text
	| SLerfu [BY]
	deriving Show
data Tense
	= TFAhA FAhA
	| TTime Time
	| TBAI (Maybe SE) BAI
	deriving Show
data Time
	= TZI ZI [TimeOffset]
	| TPU PU
	| TZAhO ZAhO
	| TZEhA ZEhA
	| TIntervalProperty IntervalProperty
	| TTimeOffset [TimeOffset]
	deriving Show
data TimeOffset
	= TimeOffset PU (Maybe ZI)
	deriving Show
data IntervalProperty
	= IPZAhO ZAhO
	| IPROI PA ROI
	deriving Show
data Prenex = Prenex [Term] deriving Show
data Free
	= FVocativeSelbri DOI (Maybe RelativeClause) Selbri
	| FMAI PA MAI
	| FTO Text
	deriving Show
data Indicator
	= IUI UI (Maybe NAI)
	| ICAI CAI (Maybe NAI)
	deriving Show
data RelativeClause
	= RCGOI GOI Term
	| RCNOI NOI Subsentence
	deriving Show
data Quantifier
	= QBOI PA (Maybe BOI)
	deriving Show

data Brivla = Brivla String deriving Show
data Cmene = Cmene String deriving Show

data A = E deriving Show
data BAI
	= LAhU
	| GAU
	| TAI
	| VAhO
	| FAU
	| MUhI
	| PIhO
	| KIhU
	| ZUhE
	deriving Show
data BE = BE deriving Show
data BEhO = BEhO deriving Show
data BO = BO deriving Show
data BOI = BOI deriving Show
data BY = KY | LY | PY | SY
	deriving Show
data CAI = SAI deriving Show
data CU = CU deriving Show
data DOI = DOI deriving Show
data FA = FA | FE | FI | FO | FAI deriving Show
data FAhA = PAhO | TOhO | BUhU deriving Show
data FAhO = FAhO deriving Show
data GIhA = GIhE deriving Show
data GOhA = COhE deriving Show
data GOI = PE deriving Show
data I = I deriving Show
data JAI = JAI deriving Show
data KEI = KEI deriving Show
data KOhA = MI | DO | KO | DEI | DA | KEhA | DIhE | MA
	deriving Show
data KU = KU deriving Show
data LA = LA deriving Show
data LE = LE | LO | LEI deriving Show
data LI = LI deriving Show
data LIhU = LIhU deriving Show
data LU = LU deriving Show
data MAI = MOhO deriving Show
data ME = ME deriving Show
data MOI = MOI deriving Show
data NA = NA | JAhA deriving Show
data NAI = NAI deriving Show
data NIhO = NIhO deriving Show
data NOI = NOI | POI deriving Show
data NU = NU | NI | DUhU deriving Show
data PA	= PA
	| RE
	| CI
	| XA
	| NO
	| SOhI
	| SOhO
	| RO
	| SOhU
	deriving Show
data PU = PU | CA | BA deriving Show
data ROI = ROI deriving Show
data SE = SE | TE | VE deriving Show
data TO = TO deriving Show
data TOI = TOI deriving Show
data UI =
	BIhU | IhA | KUhI | OhE | OI | EhU | JIhA | JAhO | XU | KAU | UE
	deriving Show
data ZAhO = BAhO | DEhA
	deriving Show
data ZEhA = ZEhA
	deriving Show
data ZI = ZA
	deriving Show
data ZOI = ZOI | LAhO deriving Show
data ZOhU = ZOhU deriving Show

main :: IO ()
main = interact $ (++ "\n") . show . parseString whole "<stdin>" . preprocess

preprocess :: String -> String
preprocess "" = ""
preprocess ('l' : 'a' : '\'' : 'o' : rest_) =
	"la'o \0 " ++ q ++ " \0 " ++ preprocess rest''
	where
	rest = dropWhile isSpace rest_
	(d, '.' : rest') = span (/= '.') rest
	(q, rest'') = spanTo ('.' : d) rest'
preprocess ('z' : 'o' : 'i' : rest_) =
	"zoi \0 " ++ q ++ " \0 " ++ preprocess rest''
	where
	rest = dropWhile isSpace rest_
	(d, '.' : rest') = span (/= '.') rest
	(q, rest'') = spanTo ('.' : d) rest'
preprocess (x : xs) = x : preprocess xs

spanTo :: Eq a => [a] -> [a] -> ([a], [a])
spanTo d [] = ([], [])
spanTo d lst@(x : xs)
	| isPrefixOf d lst = ([], drop (length d) lst)
	| otherwise = let (pre, post) = spanTo d xs in (x : pre, post)
