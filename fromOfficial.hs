{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy

[peggy|

-------------------------------------------------------------------- 1396

-- cmene :: String = !h &consonant_final coda? (any_syllable / digit)* &pause
consonant_final :: () = (non_space &non_space)* consonant &pause	{ () }

-------------------------------------------------------------------- 1408

-- cmavo :: String = !cmene !cvcy_lujvo cmavo_form &post_word
-- cvcy_lujvo :: String = cvd_rafsi y h? initial_rafsi* brivla_core

-------------------------------------------------------------------- 1418

-- lojban_word :: String = cmene / cmavo / brivla

-------------------------------------------------------------------- 1428

-------------------------------------------------------------------- 1454

-- cmene :: String = !h &consonant_final coda? (any_syllable / digit)* &pause

-------------------------------------------------------------------- 1490

{-
any_syllable :: ()
	= onset nucleus coda?
	/ consonantal_syllable

consonantal_syllable :: ()
	= consonant syllabic &(consonantal_syllable / onset) (consonant &spaces)?

coda :: (Maybe Char, Maybe Char)
	= !any_syllable consonant &any_syllable	{ (Just $1, Nothing) }
	/ syllabic? consonant? &pause		{ ($1, $2) }
-}

onset :: ()
	= h				{ () }
	/ consonant? glide		{ () }
	/ initial			{ () }

nucleus :: (Char, Maybe Char)
	= vowel				{ ($1, Nothing) }
	/ diphthong			{ (fst $1, Just $ snd $1) }
	/ y !nucleus			{ ($1, Nothing) }

-------------------------------------------------------------------- 1523

glide :: Char = (i / u) &nucleus !glide

diphthong :: (Char, Char) = (a i / a u / e i / o i) !nucleus !glide

vowel :: Char = (a / e / i / o / u) !nucleus

a :: Char = comma* [aA]			{ $2 }
e :: Char = comma* [eE]			{ $2 }
i :: Char = comma* [iI]			{ $2 }
o :: Char = comma* [oO]			{ $2 }
u :: Char = comma* [uU]			{ $2 }
y :: Char = comma* [yY]			{ $2 }

-------------------------------------------------------------------- 1543

cluster :: String = consonant consonant+	{ $1 : $2 }

initial_pair :: (Char, Char) = &initial consonant consonant !consonant
initial :: (Maybe Char, Maybe Char, Maybe Char)
	= (affricate	{ (Just $ fst $1, Just $ snd $1, Nothing ) }
		/ sibilant? other? liquid?) !consonant !glide

affricate :: (Char, Char) = t c / t s / d j / d z

liquid :: Char = l / r
other :: Char = p / t !l / k / f / x / b / d !l / g / v / m / n !liquid
sibilant :: Char = c / s !x / (j / z) !n !liquid

consonant :: Char = voiced / unvoiced / syllabic

syllabic :: Char = l / m / n / r
voiced :: Char = b / d / g / j / v / z
unvoiced :: Char = c / f / k / p / s / t / x

l :: Char = comma* [lL] !h !l			{ $2 }
m :: Char = comma* [mM] !h !m !z		{ $2 }
n :: Char = comma* [nN] !h !n !affricate	{ $2 }
r :: Char = comma* [rR] !h !r			{ $2 }
b :: Char = comma* [bB] !h !b !unvoiced		{ $2 }
d :: Char = comma* [dD] !h !d !unvoiced		{ $2 }
g :: Char = comma* [gG] !h !g !unvoiced		{ $2 }
v :: Char = comma* [vV] !h !v !unvoiced		{ $2 }
j :: Char = comma* [jJ] !h !j !z !unvoiced	{ $2 }
z :: Char = comma* [zZ] !h !z !j !unvoiced	{ $2 }
s :: Char = comma* [sS] !h !s !c !voiced	{ $2 }
c :: Char = comma* [cC] !h !c !s !x !voiced	{ $2 }
x :: Char = comma* [xX] !h !x !c !k !voiced	{ $2 }
k :: Char = comma* [kK] !h !k !x !voiced	{ $2 }
f :: Char = comma* [fF] !h !f !voiced		{ $2 }
p :: Char = comma* [pP] !h !p !voiced		{ $2 }
t :: Char = comma* [tT] !h !t !voiced		{ $2 }
h :: Char = comma* ['h] &nucleus		{ $2 }

-------------------------------------------------------------------- 1603

digit :: Char = comma* [0123456789] !h !nucleus
					{ $2 }
-- post_word :: () = pause / !nucleus lojban_word

pause :: ()
	= comma* space_char		{ () }
	/ eof

eof :: () = comma* !.			{ () }

comma :: () = [,]			{ () }

-- non_lojban_word :: String = !lojban_word non_space+

non_space :: Char = !space_char .

space_char :: ()
	= [.?! ]			{ () }
	/ space_char1
	/ space_char2
space_char1 :: () = '\t'		{ () }
space_char2 :: () = '\r'		{ () }

-------------------------------------------------------------------- 1626

spaces :: () = !y initial_spaces				{ () }
initial_spaces :: ()
	= (comma* space_char { () } / !ybu y { () })+ eof?	{ () }
	/ eof
ybu :: Lerfu = y space_char* bu					{ Lerfu }
-- lujvo :: String = !gismu !fuhivla brivla

-------------------------------------------------------------------- 1636

-- bu :: () = &cmavo (b u) &post_word
bu :: () = (b u)			{ () }

|]

data Lerfu = Lerfu
