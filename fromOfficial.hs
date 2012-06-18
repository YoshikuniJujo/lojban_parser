{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Prelude hiding (words)

import Text.Peggy
import Data.Maybe

[peggy|

-------------------------------------------------------------------- 1388

words :: [String] = pause? (word pause?)*		{ map fst $2 }
word :: String = lojban_word / non_lojban_word
lojban_word :: String = cmene / cmavo / brivla

-------------------------------------------------------------------- 1396

cmene :: String
	= !h &consonant_final coda? (any_syllable / digit { [$1] })* &pause
	{ maybe (concat $2) (++ concat $2) $1 }
consonant_final :: () = (non_space &non_space)* consonant &pause	{ () }

-------------------------------------------------------------------- 1408

cmavo :: String = !cmene !cvcy_lujvo cmavo_form &post_word
cvcy_lujvo :: ()
	= cvc_rafsi y h? initial_rafsi* brivla_core	{ () }
	/ stressed_cvc_rafsi y short_final_rafsi	{ () }
cmavo_form :: String
	= !h !cluster onset (nucleus h)* (!stressed nucleus / nucleus !cluster)
		{ $1 ++ concatMap (\(n1, h1) -> n1 ++ [h1]) $2 ++ $3 }
	/ y+
	/ digit	{ [$1] }

-------------------------------------------------------------------- 1420

brivla :: String = !cmavo initial_rafsi* brivla_core	{ concat $1 ++ $2 }

brivla_core :: String
	= fuhivla / gismu / cvv_final_rafsi
	/ stressed_initial_rafsi short_final_rafsi	{ $1 ++ $2 }

stressed_initial_rafsi :: String
	= stressed_extended_rafsi / stressed_y_rafsi / stressed_y_less_rafsi

initial_rafsi :: String
	= extended_rafsi / y_rafsi / !any_extended_rafsi y_less_rafsi

-------------------------------------------------------------------- 1433

any_extended_rafsi :: String = fuhivla / extended_rafsi / stressed_extended_rafsi

fuhivla :: String
	= fuhivla_head stressed_syllable consonantal_syllable* final_syllable
	{ $1 ++ $2 ++ concat $3 ++ $4 }

stressed_extended_rafsi	:: String = stressed_brivla_rafsi / stressed_fuhivla_rafsi
extended_rafsi :: String = brivla_rafsi / fuhivla_rafsi

stressed_brivla_rafsi :: String
	= &unstressed_syllable brivla_head stressed_syllable h y
	{ $1 ++ $2 ++ [$3] ++ [$4] }
brivla_rafsi :: String
	= &(syllable consonantal_syllable* syllable) brivla_head h y h?
	{ let init1 = $1 ++ [$2] ++ [$3] in maybe init1 ((init1 ++) . (: "")) $4 }

stressed_fuhivla_rafsi :: String
	= fuhivla_head stressed_syllable &consonant onset y
	{ $1 ++ $2 ++ $3 ++ [$4] }
fuhivla_rafsi :: String
	= &unstressed_syllable fuhivla_head &consonant onset y h?
	{ let init1 = $1 ++ $2 ++ [$3] in maybe init1 ((init1 ++) . (: "")) $4 }

fuhivla_head :: String = !rafsi_string brivla_head
brivla_head :: String
	= !cmavo !slinkuhi !h &onset unstressed_syllable*	{ concat $1 }
slinkuhi :: String = consonant rafsi_string			{ $1 : $2 }
rafsi_string :: String = y_less_rafsi*
	( gismu
	/ cvv_final_rafsi
	/ stressed_y_less_rafsi short_final_rafsi		{ $1 ++ $2 }
	/ y_rafsi
	/ stressed_y_rafsi
	/ stressed_y_less_rafsi? initial_pair y
		{ maybe [fst $2, snd $2, $3] (++ [fst $2, snd $2, $3]) $1 })
	{ concat $1 ++ $2 }

-------------------------------------------------------------------- 1461

gismu :: String = stressed_long_rafsi &final_syllable vowel &post_word
	{ $1 ++ [$2] }

cvv_final_rafsi :: String
	= consonant stressed_vowel h &final_syllable vowel &post_word
	{ [$1, $2, $3, $4] }
short_final_rafsi :: String = &final_syllable
	( consonant diphthong	{ [$1, fst $2, snd $2] }
	/ initial_pair vowel	{ [fst $1, snd $1, $2] } )
	&post_word

stressed_y_rafsi :: String
	= (stressed_long_rafsi / stressed_cvc_rafsi) y	{ $1 ++ [$2] }
stressed_y_less_rafsi :: String
	= stressed_cvc_rafsi !y / stressed_ccv_rafsi / stressed_cvv_rafsi

stressed_long_rafsi :: String
	= (stressed_ccv_rafsi / stressed_cvc_rafsi) consonant
						{ $1 ++ [$2] }
stressed_cvc_rafsi :: String
	= consonant stressed_vowel consonant	{ [$1, $2, $3] }
stressed_ccv_rafsi :: String
	= initial_pair stressed_vowel		{ [fst $1, snd $1, $2] }
stressed_cvv_rafsi :: String = consonant
	( unstressed_vowel h stressed_vowel	{ [$1, $2, $3] }
	/ stressed_diphthong			{ [ fst $1, snd $1 ] } )
	r_hyphen?
	{ maybe ($1 : $2) ((($1 : $2) ++) . (: [])) $3 }

y_rafsi :: String = (long_rafsi / cvc_rafsi) y h?
	{ maybe ($1 ++ [$2]) ((($1 ++ [$2]) ++) . (: [])) $3 }
y_less_rafsi :: String = !y_rafsi (cvc_rafsi !y / ccv_rafsi / cvv_rafsi)
	!any_extended_rafsi

long_rafsi :: String = (ccv_rafsi / cvc_rafsi) consonant	{ $1 ++ [$2] }
cvc_rafsi :: String = consonant unstressed_vowel consonant	{ $1 : $2 : [$3] }
ccv_rafsi :: String = initial_pair unstressed_vowel	{ [fst $1, snd $1, $2] }
cvv_rafsi :: String = consonant
	( unstressed_vowel h unstressed_vowel	{ [$1, $2, $3] }
	/ unstressed_diphthong			{ [fst $1, snd $1] } )
	r_hyphen?
	{ maybe ($1 : $2) ((($1 : $2) ++) . (: "")) $3 }
r_hyphen :: Char = r &consonant / n &r

-------------------------------------------------------------------- 1500

final_syllable :: String
	= onset !y !stressed nucleus !cmene &post_word	{ $1 ++ $2 }

stressed_syllable :: String = &stressed syllable / syllable &stress
stressed_diphthong :: (Char, Char) = &stressed diphthong / diphthong &stress
stressed_vowel :: Char = &stressed vowel / vowel &stress

unstressed_syllable :: String = !stressed syllable !stress / consonantal_syllable
unstressed_diphthong :: (Char, Char) = !stressed diphthong !stress
unstressed_vowel :: Char = !stressed vowel !stress

stress :: () = consonant* y? syllable pause	{ () }
stressed :: () = onset comma* [AEIOU]		{ () }

any_syllable :: String
	= onset nucleus coda?		{ maybe ($1 ++ $2) (($1 ++ $2) ++) $3 }
	/ consonantal_syllable

syllable :: String = onset !y nucleus coda?
					{ maybe ($1 ++ $2) (($1 ++ $2) ++) $3 }

consonantal_syllable :: String
	= consonant syllabic &(consonantal_syllable / onset)
		(consonant &spaces)?		{ $1 : $2 : maybe [] (: []) $3 }

coda :: String
	= !any_syllable consonant &any_syllable	{ [$1] }
	/ syllabic? consonant? &pause		{ catMaybes [$1, $2] }

onset :: String
	= h					{ [$1] }
	/ consonant? glide			{ maybe [$2] (: [$2]) $1 }
	/ initial

nucleus :: String
	= vowel					{ [$1] }
	/ diphthong				{ [fst $1, snd $1] }
	/ y !nucleus				{ [$1] }

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
initial :: String =
	( affricate				{ [fst $1, snd $1] }
	/ sibilant? other? liquid?		{ catMaybes [$1, $2, $3] } )
	!consonant !glide

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

-------------------------------------------------------------------- 1612

digit :: Char = comma* [0123456789] !h !nucleus 		{ $2 }
post_word :: () = pause / !nucleus lojban_word			{ () }
pause :: () = comma* space_char { () } / eof
eof :: () = comma* !.						{ () }
comma :: () = [,]						{ () }
non_lojban_word :: String = !lojban_word non_space+
non_space :: Char = !space_char .
space_char :: () = [.?! ] { () } / space_char1 / space_char2
space_char1 :: () = '\t'					{ () }
space_char2 :: () = '\r'					{ () }

-------------------------------------------------------------------- 1635

spaces :: () = !y initial_spaces				{ () }
initial_spaces :: ()
	= (comma* space_char { () } / !ybu y { () })+ eof?	{ () }
	/ eof
ybu :: Lerfu = y space_char* bu					{ Lerfu }
lujvo :: String = !gismu !fuhivla brivla

-------------------------------------------------------------------- 1645

bu :: () = &cmavo (b u) &post_word	{ () }

|]

data Lerfu = Lerfu
