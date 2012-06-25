{-# LANGUAGE DoRec #-}

module Main where

import Text.Parsers.Frisby
import Data.Char
import Control.Applicative hiding (many, optional)

main :: IO ()
main = do
--	interact $ (++ "\n") . runPeg parser
	return ()

-- parser :: PM s (P s String)
parser = do
	rec
--		lojban_word <- newRule $ cmene // cmavo // brivla

		----------------------------------------------------------------

		cmene <- newRule $
			neek h ->> peek consonant_final ->> option "" coda <++>
			manyCat (any_syllable // single digit) <<- peek pause
		consonant_final <- newRule $ many (non_space <<- peek non_space)
			<+++> consonant <<- peek pause

		----------------------------------------------------------------

		cmavo <- newRule $ neek cmene ->> neek _CVCy_lujvo ->>
			cmavo_form <<- peek post_word

		_CVCy_lujvo <- newRule
			$  _CVC_rafsi <+++> y <<- neek h <++>
				manyCat initial_rafsi <++> brivla_core
			// stressed_CVC_rafsi <+++> y <++> short_final_rafsi

		cmavo_form <- newRule
			$  neek h ->> neek cluster ->>
				onset <++> manyCat (nucleus <+++> h) <++>
				(  neek stressed ->> nucleus
				// nucleus <<- neek cluster )
			// many1 y
			// single digit

		----------------------------------------------------------------

		brivla <- newRule $
			neek cmavo ->> manyCat initial_rafsi <++> brivla_core

		brivla_core <- newRule $
			fuhivla // gismu // _CVV_final_rafsi //
			stressed_initial_rafsi <++> short_final_rafsi

		stressed_initial_rafsi <- newRule
			$  stressed_extended_rafsi
			// stressed_y_rafsi
			// stressed_y_less_rafsi

		initial_rafsi <- newRule
			$  extended_rafsi
			// y_rafsi
			// neek any_extended_rafsi ->> y_less_rafsi

		----------------------------------------------------------------

		any_extended_rafsi <- newRule $
			fuhivla // extended_rafsi // stressed_extended_rafsi

		fuhivla <- newRule $
			{-- fuhivla_head <++> --} stressed_syllable <++>
			many1Cat consonantal_syllable <++> final_syllable
			//
			{-- fuhivla_head <++> --} unstressed_syllable <++>
			many1Cat consonantal_syllable <++>
			many (neek stressed_syllable ->> (consonant // vowel)) <++>
			stressed_syllable <++> final_syllable

{-
		fuhivla <- newRule $
			fuhivla_head <++> stressed_syllable <++>
			manyCat consonantal_syllable <++> final_syllable
-}

		stressed_extended_rafsi <- newRule $
			stressed_brivla_rafsi // stressed_fuhivla_rafsi
		extended_rafsi <- newRule $ brivla_rafsi // fuhivla_rafsi

		stressed_fuhivla_rafsi <- newRule $
			(fuhivla_head <++> stressed_syllable <<- peek consonant)
			<++> onset <+++> y
			
		fuhivla_rafsi <- newRule $ (peek unstressed_syllable ->>
			fuhivla_head <<- peek consonant) <++> onset <+++> y <++>
			opt h

		stressed_brivla_rafsi <- newRule $
			peek unstressed_syllable ->> brivla_head <++>
			stressed_syllable <+++> h <+++> y
		brivla_rafsi <- newRule $
			peek (syllable <++> manyCat consonantal_syllable <++>
				syllable) <++> brivla_head <+++> h <+++> y <++>
			opt h

		fuhivla_head <- newRule $ neek rafsi_string ->> brivla_head
		brivla_head <- newRule $ neek cmavo ->> neek slinkuhi ->>
			neek h ->> peek onset ->> manyCat unstressed_syllable
		slinkuhi <- newRule $ consonant <:> rafsi_string
		rafsi_string <- newRule $ manyCat y_less_rafsi <++>
			(  gismu
			// _CVV_final_rafsi
			// stressed_y_less_rafsi <++> short_final_rafsi
			// y_rafsi
			// stressed_y_rafsi
			// option "" stressed_y_less_rafsi <++> initial_pair
				<+++> y )

		----------------------------------------------------------------

		gismu <- newRule $
			(stressed_long_rafsi <<- peek final_syllable) <+++> vowel
			<<- peek post_word

		_CVV_final_rafsi <- newRule $
			consonant <::> stressed_vowel <+++>
			(h <<- peek final_syllable) <+++> vowel <<- peek post_word

		short_final_rafsi <- newRule $ peek final_syllable ->>
			(  consonant <:> diphthong
			// initial_pair <+++> vowel ) <<- peek post_word

		stressed_y_rafsi <- newRule $
			(stressed_long_rafsi // stressed_CVC_rafsi) <+++> y
		stressed_y_less_rafsi <- newRule
			$  stressed_CVC_rafsi <<- neek y
			// stressed_CCV_rafsi
			// stressed_CVV_rafsi
		stressed_long_rafsi <- newRule $
			(stressed_CCV_rafsi // stressed_CVC_rafsi) <+++> consonant
		stressed_CVC_rafsi <- newRule $
			consonant <::> stressed_vowel <+++> consonant
		stressed_CCV_rafsi <- newRule $ initial_pair <+++> stressed_vowel
		stressed_CVV_rafsi <- newRule $ consonant <:>
			(  unstressed_vowel <::> h <+++> stressed_vowel
			// stressed_diphthong ) <++> opt r_hyphen

		y_rafsi <- newRule $ (long_rafsi // _CVC_rafsi) <+++> y <<- neek h
		y_less_rafsi <- newRule $ neek y_rafsi ->>
			choice [_CVC_rafsi <<- neek y, _CCV_rafsi, _CVV_rafsi]
			<<- neek any_extended_rafsi
		long_rafsi <- newRule $ (_CCV_rafsi // _CVC_rafsi) <+++> consonant
		_CVC_rafsi <- newRule $
			consonant <::> unstressed_vowel <+++> consonant
		_CCV_rafsi <- newRule $ initial_pair <+++> unstressed_vowel
		_CVV_rafsi <- newRule $ consonant <:>
			(  unstressed_vowel <::> h <+++> unstressed_vowel
			// unstressed_diphthong) <++> opt r_hyphen
		r_hyphen <- newRule $ r <<- peek consonant // n <<- peek r

		----------------------------------------------------------------

		final_syllable <- newRule $
			onset <<- neek y <<- neek stressed <++> nucleus <<-
			neek cmene  <<- peek post_word
		stressed_syllable <- newRule $
			peek stressed ->> syllable // syllable <<- peek stress
		stressed_diphthong <- newRule $
			peek stressed ->> diphthong // diphthong <<- peek stress
		stressed_vowel <- newRule $
			peek stressed ->> vowel // vowel <<- peek stress
		unstressed_syllable <- newRule $
			neek stressed ->> syllable <<- neek stress
		unstressed_diphthong <- newRule $
			neek stressed ->> diphthong <<- neek stress
		unstressed_vowel <- newRule $
			neek stressed ->> vowel <<- neek stress
		stress <- newRule $
			many consonant <++> opt y <++> syllable <<- pause
		stressed <- newRule $ (onset <<- many comma) <+++> oneOf "AEIOU"
		any_syllable <- newRule
			$  onset <++> nucleus <++> option "" coda
			// consonantal_syllable
		syllable <- newRule $
			onset <<- neek y <++> nucleus <++> option "" coda
		consonantal_syllable <- newRule $
			consonant <::> syllabic <<-
			peek (consonantal_syllable // onset) <++>
			opt (consonant <<- peek spaces)
		coda <- newRule
			$  neek any_syllable ->> single consonant <<-
				peek any_syllable
			// opt syllabic <++> opt consonant <<- peek pause
		onset <- newRule $
			single h // opt consonant <+++> glide // initial
		nucleus <- newRule $
			 single vowel // diphthong // single y <<- neek nucleus

		----------------------------------------------------------------

		glide <- newRule $ (i // u) <<- peek nucleus <<- neek glide
		diphthong <- newRule $
			choice [ a <::> i, a <::> u, e <::> i, o <::> i] <<-
			neek nucleus <<- neek glide
		vowel <- newRule $ choice [a, e, i, o, u] <<- neek nucleus

		----------------------------------------------------------------

		cluster <- newRule $ consonant <:> many1 consonant
		initial_pair <- newRule $ peek initial ->>
			consonant <::> consonant <<- neek consonant
		initial <- newRule $
			(  affricate
			// opt sibilant <++> opt other <++> opt liquid )
				<<- neek consonant <<- neek glide
		affricate <- newRule $
			choice [t <::> c, t <::> s, d <::> j, d <::> z]
		liquid <- newRule $ l // r
		other <- newRule $ choice [p, t <<- neek l, k, f, x, b,
			d <<- neek l, g, v, m, n <<- neek liquid]
		sibilant <- newRule $ choice
			[c, s <<- neek x, (j // z) <<- neek n <<- neek liquid]
		consonant <- newRule $ choice [voiced, unvoiced, syllabic]
		syllabic <- newRule $ choice [l, m, n, r]
		voiced <- newRule $ choice [b, d, g, j, v, z]
		unvoiced <- newRule $ choice [c, f, k, p, s, t, x]
		l <- newRule $ many comma ->> oneOf "lL" <<- neek h <<- neek l
		m <- newRule $ many comma ->> oneOf "mM" <<- neek h <<- neek m
			<<- neek z
		n <- newRule $ many comma ->> oneOf "nN" <<- neek h <<- neek n
			<<- neek affricate
		r <- newRule $ many comma ->> oneOf "rR" <<- neek h <<- neek r
		b <- newRule $ many comma ->> oneOf "bB" <<- neek h <<- neek b
			<<- neek unvoiced
		d <- newRule $ many comma ->> oneOf "dD" <<- neek h <<- neek d
			<<- neek unvoiced
		g <- newRule $ many comma ->> oneOf "gG" <<- neek h <<- neek g
			<<- neek unvoiced
		v <- newRule $ many comma ->> oneOf "vV" <<- neek h <<- neek v
			<<- neek unvoiced
		j <- newRule $ many comma ->> oneOf "jJ" <<- neek h <<- neek j
			<<- neek z <<- neek unvoiced
		z <- newRule $ many comma ->> oneOf "zZ" <<- neek h <<- neek z
			<<- neek j <<- neek unvoiced
		s <- newRule $ many comma ->> oneOf "sS" <<- neek h <<- neek s
			<<- neek c <<- neek voiced
		c <- newRule $ many comma ->> oneOf "cC" <<- neek h <<- neek c
			<<- neek s <<- neek x <<- neek voiced
		x <- newRule $ many comma ->> oneOf "xX" <<- neek h <<- neek x
			<<- neek c <<- neek k <<- neek voiced
		k <- newRule $ many comma ->> oneOf "kK" <<- neek h <<- neek k
			<<- neek x <<- neek voiced
		f <- newRule $ many comma ->> oneOf "fF" <<- neek h <<- neek f
			<<- neek voiced
		p <- newRule $ many comma ->> oneOf "pP" <<- neek h <<- neek p
			<<- neek voiced
		t <- newRule $ many comma ->> oneOf "tT" <<- neek h <<- neek t
			<<- neek voiced
		h <- newRule $ many comma ->> oneOf "'h" <<- peek nucleus

		----------------------------------------------------------------

		digit <- newRule $ many comma ->>
			oneOf "0123456789" <<- neek h <<- neek nucleus

		post_word <- newRule
			$  pause
--			// neek nucleus ->> discard lojban_word
--		non_lojban_word <- newRule $ neek lojban_word ->> many1 non_space

		----------------------------------------------------------------
		-- Spaces, LUJVO

		spaces <- newRule $ neek _Y ->> initial_spaces
		initial_spaces <- newRule
			$  many1
				(  many comma ->> space_char
				// neek ybu <<- _Y ) ->> optional _EOF
			// _EOF
		ybu <- newRule $ _Y <<- many space_char <<- _BU
--		lujvo <- newRule $ neek gismu ->> neek fuhivla ->> brivla

		----------------------------------------------------------------
		-- CMAVO

		_BU <- newRule $ peek cmavo ->> b <> u <<- peek post_word
		_Y <- newRule $ peek cmavo ->> many1 y <<- peek post_word

		----------------------------------------------------------------

	return brivla

alphabet c = many comma ->> oneOf [c, toUpper c]
[a, e, i, o, u, y] = map alphabet "aeiouy"

pause = discard (many comma <> space_char) // _EOF
_EOF = many comma ->> neek anyChar
comma = discard $ char ','
non_space = neek space_char ->> anyChar
space_char = discard $ choice [oneOf ".?! ", space_char1, space_char2]
space_char1 = char '\t'
space_char2 = oneOf "\r\n"


neek = doesNotMatch
opt p = option "" $ single p
p <:> q = p <> q ## (uncurry (:))
p <::> q = p <> q ## (\(c, d) -> [c, d])
manyCat p = many p ## concat
many1Cat p = many1 p ## concat
(<+++>) :: P s String -> P s Char -> P s String
p <+++> q = p <> q ## (\(s, c) -> s ++ [c])
single = (## (: []))
