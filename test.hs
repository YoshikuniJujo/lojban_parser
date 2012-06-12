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
	= statement_1

statement_1 :: Statement
	= statement_2 (i_clause joik_jek statement_2?)*	{ SJoikJek $1 $2 }

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

gihek :: ((GIhA, [[Indicator]]), Maybe NAI)
	= giha_clause nai?

tail_terms :: Term
	= terms? free*	{ TFree (fromMaybe [] $1) $2 }

selbri :: Selbri
	= selbri_1
	/ tag selbri_1		{ TagSelbri $1 $2 }

selbri_1 :: Selbri
	= selbri_2
	/ na_clause selbri	{ NotSelbri $1 $2 }

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
	= sumti_4

sumti_4 :: Sumti
	= sumti_5 / gek sumti gik sumti_4	{ SGek $1 $2 $3 $4 }

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
	/ lahe sumti	{ SLAhE $2 }
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
	= se? bai	{ TBAI $1 $2 }
	/ space_	{ TSpace $1 }
	/ time		{ TTime $1 }
	/ caha		{ TCAhA $1 }

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

space_ :: Space
	= space_offset		{ SFAhA $1 }
	/ space_interval	{ SSpaceInterval $1 }

space_offset :: FAhA
	= faha

space_interval :: SpaceInterval
	= veha			{ SIVEhA $1 }
	/ space_int_props	{ SIFEhE $1 }

space_int_props :: [(FEhE, IntervalProperty)]
	= (fehe interval_property)+

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
	= number !moi boi?	{ QBOI $1 $2 }

interval_property :: IntervalProperty
	= number roi	{ IPROI $1 $2 }
	/ tahe nai?	{ IPTAhE $1 $2 }
	/ zaho		{ IPZAhO $1 }

number :: Number
	= pa_clause+	{ Number $1 }
--	= pa pa*	{ $1 : $2 }

free :: Free
	= vocative relative_clause? selbri
			{ FVocativeSelbri $1 $2 $3 }
	/ number mai	{ FMAI $1 $2 }
	/ to text toi?	{ FTO $2 }

vocative :: DOI
	= doi

relative_clause :: RelativeClause
	= goi term		{ RCGOI $1 $2 }
	/ noi subsentence kuho?	{ RCNOI $1 $2 }

lerfu_string :: [LerfWord]
	= lerfu_word+

joik_jek :: JA
	= jek

gek :: (GA, Maybe NAI)
	= ga nai?

gik :: (GI, Maybe NAI)
	= gi nai?

jek :: JA
	= ja

lerfu_word :: LerfWord
	= by_clause	{ LBY (fst $1) (snd $1) }

brivla_clause :: (Brivla, [[Indicator]])
	= brivla post_clause

a_clause :: (A, [[Indicator]])
	= a post_clause

by_clause :: (BY, [[Indicator]])
	= by post_clause

giha_clause :: (GIhA, [[Indicator]])
	= giha post_clause

i_clause :: (I, [[Indicator]])
	= i post_clause

le_clause :: (LE, [[Indicator]])
	= le post_clause

na_clause :: (NA, [[Indicator]])
	= na post_clause

nai_clause :: (NAI, [[Indicator]])
	= nai post_clause

pa_clause :: (PA, [[Indicator]])
	= pa post_clause

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
	/ "ji"		{ JI }

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
	/ "cau"		{ CAU }

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

caha :: CAhA
	= "ca\'a"	{ CAhA }
	/ "ka\'e"	{ KAhE }

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
	/ "ne\'i"	{ NEhI }

faho :: FAhO
	= "fa\'o"	{ FAhO }

fehe :: FEhE
	= "fe\'e"	{ FEhE }

ga :: GA
	= "ge"		{ GE }

gi :: GI
	= "gi"		{ GI }

giha ::: GIhA
	= "gi\'a"	{ GIhA }
	/ "gi\'e"	{ GIhE }
	/ "gi\'i"	{ GIhI }

goha ::: GOhA
	= "co\'e"	{ COhE }
	/ "du" !"\'"	{ DU }

goi ::: GOI
	= "pe"		{ PE }
	/ "po\'u"	{ POhU }

i ::: I
	= ".i" 		{ I }

ja ::: JA
	= "je"		{ JE }

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
	/ "di\'u"	{ DIhU }
	/ "ta"		{ TA }
	/ "zo\'e"	{ ZOhE }

ku :: KU
	= "ku"		{ KU }

kuho :: KUhO
	= "ku\'o"	{ KUhO }

la ::: LA
	= "la"		{ LA }

lahe ::: LAhE
	= "la\'e"	{ LAhE }
	/ "tu\'a"	{ TUhA }

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
	/ "ka" !"\'"	{ KA }

pa ::: PA
	= "pa"		{ PA }
	/ "re" !"\'"	{ RE }
	/ "ci"		{ CI }
	/ "xa"		{ XA }
	/ "bi" !"\'"	{ BI }
	/ "no"		{ NO }
	/ "so\'i"	{ SOhI }
	/ "so\'o"	{ SOhO }
	/ "ro"		{ RO }
	/ "so\'u"	{ SOhU }
	/ "su\'e"	{ SUhE }

pu ::: PU
	= "ca" !"\'"	{ CA }
	/ "ba" !"\'"	{ BA }
	/ "pu" !"\'"	{ PU }

roi ::: ROI
	= "roi"		{ ROI }
	/ "re\'u"	{ REhU }

se ::: SE
	= "se"		{ SE }
	/ "te"		{ TE }
	/ "ve"		{ VE }

tahe ::: TAhE
	= "ru\'i"	{ RUhI }

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
	/ "po\'o"	{ POhO }
	/ "sa\'e"	{ SAhE }

veha ::: VEhA
	= "ve\'a"	{ VEhA }

zoi ::: ZOI
	= "zoi"		{ ZOI }
	/ "la\'o"	{ LAhO }

zaho ::: ZAhO
	= "ba\'o"	{ BAhO }
	/ "de\'a"	{ DEhA }
	/ "co\'a"	{ COhA }
	/ "pu\'o"	{ PUhO }
	/ "co\'u"	{ COhU }
	/ "za\'o"	{ ZAhO }

zeha ::: ZEhA
	= "ze\'a"	{ ZEhA }
	/ "ze\'i"	{ ZEhI }
	/ "ze\'u"	{ ZEhU }

zi ::: ZI
	= "za" !"\'"	{ ZA }

zohu ::: ZOhU
	= "zo\'u"	{ ZOhU }

endWithVowel :: String
	= anyLetter endWithVowel	{ $1 : $2 }
	/ vowel				{ [$1] }

consonant_final :: String
	= anyLetter consonant_final	{ $1 : $2 }
	/ consonant			{ [$1] }

noDoubleConsonant :: ()
	= !doubleConsonant

doubleConsonant :: String
	= consonant consonant		{ $1 : [$2] }
	/ consonant [y] consonant	{ $1 : 'y' : [$2] }

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
	| SJoikJek Statement [((I, [[Indicator]]), JA, Maybe Statement)]
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
	| TMOI Number MOI
	| TSE SE TanruUnit
	| TUTanruUnit [TanruUnit]
	| TULinkargs TanruUnit Term
	| TME Sumti
	| TJAI JAI (Maybe Tense) TanruUnit
	deriving Show
data BridiTail
	= BTSelbri Selbri Term
	| BTBridiTail BridiTail (Maybe (((GIhA, [[Indicator]]), Maybe NAI), BridiTail))
	deriving Show
data Selbri
	= Selbri TanruUnit
	| NotSelbri (NA, [[Indicator]]) Selbri
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
	| SGek (GA, Maybe NAI) Sumti (GI, Maybe NAI) Sumti
	| SRelative Sumti RelativeClause
	| SQuantifier Quantifier Sumti
	| SMex Quantifier
	| SZOI ZOI String
	| SLU LU Text
	| SLerfu [LerfWord]
	| SLAhE Sumti
	deriving Show
data Tense
	= TTime Time
	| TSpace Space
	| TBAI (Maybe SE) BAI
	| TCAhA CAhA
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
	= IPROI Number ROI
	| IPTAhE TAhE (Maybe NAI)
	| IPZAhO ZAhO
	deriving Show
data Space
	= SFAhA FAhA
	| SSpaceInterval SpaceInterval
	deriving Show
data SpaceInterval
	= SIVEhA VEhA
	| SIFEhE [(FEhE, IntervalProperty)]
	deriving Show
data Prenex = Prenex [Term] deriving Show
data Free
	= FVocativeSelbri DOI (Maybe RelativeClause) Selbri
	| FMAI Number MAI
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
	= QBOI Number (Maybe BOI)
	deriving Show
data LerfWord
	= LBY BY [[Indicator]]
	deriving Show
data Number
	= Number [(PA, [[Indicator]])]
	deriving Show

data Brivla = Brivla String deriving Show
data Cmene = Cmene String deriving Show

data A = E | JI deriving Show
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
	| CAU
	deriving Show
data BE = BE deriving Show
data BEhO = BEhO deriving Show
data BO = BO deriving Show
data BOI = BOI deriving Show
data BY = KY | LY | PY | SY
	deriving Show
data CAI = SAI deriving Show
data CAhA = CAhA | KAhE deriving Show
data CU = CU deriving Show
data DOI = DOI deriving Show
data FA = FA | FE | FI | FO | FAI deriving Show
data FAhA = PAhO | TOhO | BUhU | NEhI deriving Show
data FAhO = FAhO deriving Show
data FEhE = FEhE deriving Show
data GA = GE deriving Show
data GI = GI deriving Show
data GIhA = GIhA | GIhE | GIhI deriving Show
data GOhA = COhE | DU deriving Show
data GOI = PE | POhU deriving Show
data I = I deriving Show
data JA = JE deriving Show
data JAI = JAI deriving Show
data KEI = KEI deriving Show
data KOhA = MI | DO | KO | DEI | DA | KEhA | DIhE | MA | DIhU | TA | ZOhE
	deriving Show
data KU = KU deriving Show
data KUhO = KUhO deriving Show
data LA = LA deriving Show
data LAhE = LAhE | TUhA deriving Show
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
data NU = NU | NI | DUhU | KA deriving Show
data PA	= PA
	| RE
	| CI
	| XA
	| BI
	| NO
	| SOhI
	| SOhO
	| RO
	| SOhU
	| SUhE
	deriving Show
data PU = PU | CA | BA deriving Show
data ROI = ROI | REhU deriving Show
data SE = SE | TE | VE deriving Show
data TAhE = RUhI deriving Show
data TO = TO deriving Show
data TOI = TOI deriving Show
data UI	= BIhU | IhA | KUhI | OhE | OI | EhU | JIhA | JAhO | XU | KAU | UE | POhO
	| SAhE
	deriving Show
data VEhA = VEhA deriving Show
data ZAhO = BAhO | DEhA | COhA | PUhO | COhU | ZAhO
	deriving Show
data ZEhA = ZEhA | ZEhI | ZEhU
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
