{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy

[peggy|

-------------------------------------------------------------------- 1396

{-
cmene :: String = !h &consonant_final coda? (any_syllable / digit)* &pause
consonant_final :: () = (non_space &non_space)* consonant &pause
-}

-------------------------------------------------------------------- 1408

-- cmavo :: String = !cmene !cvcy_lujvo cmavo_form &post_word
-- cvcy_lujvo :: String = cvd_rafsi y h? initial_rafsi* brivla_core

-------------------------------------------------------------------- 1418

-- lojban_word :: String = cmene / cmavo / brivla

-------------------------------------------------------------------- 1428

-------------------------------------------------------------------- 1454

-- cmene :: String = !h &consonant_final coda? (any_syllable / digit)* &pause

-------------------------------------------------------------------- 1488

{-
any_syllable :: ()
	= onset nucleus coda?
	/ consonantal_syllable

consonantal_syllable :: ()
	= consonant syllabic &(consonantal_syllable / onset) (consonant &spaces)?

coda :: (Maybe Char, Maybe Char)
	= !any_syllable consonant &any_syllable	{ (Just $1, Nothing) }
	/ syllabic? consonant? &pause		{ ($1, $2) }

onset :: ()
	= h
	/ consonant? glide
	/ initial
-}

nucleus :: (Char, Maybe Char)
	= vowel				{ ($1, Nothing) }
	/ diphthong			{ (fst $1, Just $ snd $1) }
	/ y !nucleus			{ ($1, Nothing) }

-------------------------------------------------------------------- 1521

glide :: Char = (i / u) &nucleus !glide

diphthong :: (Char, Char) = (a i / a u / e i / o i) !nucleus !glide

vowel :: Char = (a / e / i / o / u) !nucleus

a :: Char = comma* [aA]			{ $2 }
e :: Char = comma* [eE]			{ $2 }
i :: Char = comma* [iI]			{ $2 }
o :: Char = comma* [oO]			{ $2 }
u :: Char = comma* [uU]			{ $2 }
y :: Char = comma* [yY]			{ $2 }

-------------------------------------------------------------------- 1541

-- consonant :: Char = voiced / unvoiced / syllabic

unvoiced :: Char = c / f / k / p / s / t / x

l :: Char = comma* [lL] 
b :: Char = comma* [bB] !h !b !unvoiced	{ $2 }
h :: Char = comma* ['h] &nucleus	{ $2 }

-------------------------------------------------------------------- 1601

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

-------------------------------------------------------------------- 1624

-- spaces :: ()
--	= !y initial_spaces
-- initial_spaces :: ()
--	= (comma* space_char / !ybu y)+ eof? / eof
-- ybu :: Lerfu = y space_char* bu		{ Lerfu }
-- lujvo :: String = !gismu !fuhivla brivla

-------------------------------------------------------------------- 1634

-- bu :: () = &cmavo (b u) &post_word

|]

data Lerfu = Lerfu
