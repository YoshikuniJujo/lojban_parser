{-# LANGUAGE DoRec #-}

module Main where

import Text.Parsers.Frisby
import Data.Char
import Data.Maybe
import Control.Applicative hiding (many, optional)

main :: IO ()
main = do
	interact $ (++ "\n") . show . runPeg parser

-- parser :: PM s (P s String)
parser = do
	rec
		_CMENE <- newRule $ cmene ## CMENE
		_BRIVLA <- newRule $ (gismu // lujvo // fuhivla) ## BRIVLA
		_CMAVO <- newRule $ _BU

		----------------------------------------------------------------

		words <- newRule $
			optional pause ->> many (word <<- optional pause)
		word <- newRule $ lojban_word // non_lojban_word
		lojban_word <- newRule $ cmene // cmavo // brivla

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
			// neek nucleus ->> discard lojban_word
		non_lojban_word <- newRule $ neek lojban_word ->> many1 non_space

		----------------------------------------------------------------
		-- Spaces, LUJVO

		spaces <- newRule $ neek _Y ->> initial_spaces
		initial_spaces <- newRule
			$  many1
				(  many comma ->> space_char
				// neek ybu <<- _Y ) ->> optional _EOF
			// _EOF
		ybu <- newRule $ _Y <<- many space_char <<- _BU
		lujvo <- newRule $ neek gismu ->> neek fuhivla ->> brivla

		----------------------------------------------------------------
		-- CMAVO

		let	dict = [
				('a', a), ('e', e), ('i', i), ('o', o), ('u', u),
				('y', y),
				('m', m), ('f', f), ('v', v), ('p', p), ('b', b),
				('s', s), ('z', z), ('c', c), ('j', j), ('t', t),
				('d', d), ('k', k), ('g', g), ('x', x), ('l', l),
				('n', n), ('r', r), ('h', h)
			 ]
			pcmavo = newRule . parse_cmavo dict cmavo post_word

		_A    <- pcmavo A
		_BAI  <- pcmavo BAI
		_BAhE <- pcmavo BAhE
		_BE   <- pcmavo BE
		_BEI  <- pcmavo BEI
		_BEhO <- pcmavo BEhO
		_BIhE <- pcmavo BIhE
		_BIhI <- pcmavo BIhI
		_BO   <- pcmavo BO
		_BOI  <- pcmavo BOI
		_BU   <- pcmavo BU
		__BY  <- pcmavo BY
		_BY   <- newRule $ ybu ## (const YBU) // __BY
		_CAhA <- pcmavo CAhA
		_CAI  <- pcmavo CAI
		_CEI  <- pcmavo CEI
		_CEhE <- pcmavo CEhE
		_CO   <- pcmavo CO
		_COI  <- pcmavo COI
		_CU   <- pcmavo CU
		_CUhE <- pcmavo CUhE
		_DAhO <- pcmavo DAhO
		_DOI  <- pcmavo DOI
		_DOhU <- pcmavo DOhU
		_FA   <- pcmavo FA
		_FAhA <- pcmavo FAhA
		_FAhO <- pcmavo FAhO
		_FEhE <- pcmavo FEhE
		_FEhU <- pcmavo FEhU
		_FIhO <- pcmavo FIhO
		_FOI  <- pcmavo FOI
		_FuhA <- pcmavo FUhA
		_FUhE <- pcmavo FUhE
		_FUhO <- pcmavo FUhO
		_GA   <- pcmavo GA
		_GAhO <- pcmavo GAhO
		_GEhU <- pcmavo GEhU
		_GI   <- pcmavo GI
		_GIhA <- pcmavo GIhA
		_GOI  <- pcmavo GOI
		_GOhA <- pcmavo GOhA
		_GUhA <- pcmavo GUhA
		_I    <- pcmavo I
		_JA   <- pcmavo JA
		_JOhI <- pcmavo JOhI
		_JOI  <- pcmavo JOI
		_KE   <- pcmavo KE
		_KEhE <- pcmavo KEhE
		_KEI  <- pcmavo KEI
		_KI   <- pcmavo KI
		_KOhA <- pcmavo KOhA
		_KU   <- pcmavo KU
		_KUhE <- pcmavo KUhE
		_KUhO <- pcmavo KUhO
		_LA   <- pcmavo LA
		_LAU  <- pcmavo LAU
		_LAhE <- pcmavo LAhE
		_LE   <- pcmavo LE
		_LEhU <- pcmavo LEhU
		_LI   <- pcmavo LI
		_LIhU <- pcmavo LIhU
		_LOhO <- pcmavo LOhO
		_LOhU <- pcmavo LOhU
		_LU   <- pcmavo LU
		_LUhU <- pcmavo LUhU
		_MAhO <- pcmavo MAhO
		_MAI  <- pcmavo MAI
		_ME   <- pcmavo ME
		_MEhU <- pcmavo MEhU
		_MOhE <- pcmavo MOhE
		_MOhI <- pcmavo MOhI
		_MOI  <- pcmavo MOI
		_NA   <- pcmavo NA
		_NAI  <- pcmavo NAI
		_NAhE <- pcmavo NAhE
		_NAhU <- pcmavo NAhU
		_NIhE <- pcmavo NIhE
		_NIhO <- pcmavo NIhO
		_NOI  <- pcmavo NOI
		_NU   <- pcmavo NU
		_NUhA <- pcmavo NUhA
		_NUhI <- pcmavo NUhI
		_NUhU <- pcmavo NUhU
		__PA  <- pcmavo PA
		_PA   <- newRule $ __PA // digit ## digitToPA
		_PEhE <- pcmavo PEhE
		_PEhO <- pcmavo PEhO
		_PU   <- pcmavo PU
		_RAhO <- pcmavo RAhO
		_ROI  <- pcmavo ROI
		_SA   <- pcmavo SA
		_SE   <- pcmavo SE
		_SEI  <- pcmavo SEI
		_SEhU <- pcmavo SEhU
		_SI   <- pcmavo SI
		_SOI  <- pcmavo SOI
		_SU   <- pcmavo SU
		_TAhE <- pcmavo TAhE
		_TEhU <- pcmavo TEhU
		_TEI  <- pcmavo TEI
		_TO   <- pcmavo TO
		_TOI  <- pcmavo TOI
		_TUhE <- pcmavo TUhE
		_TUhU <- pcmavo TUhU
		_UI   <- pcmavo UI
		_VA   <- pcmavo VA
		_VAU  <- pcmavo VAU
		_VEI  <- pcmavo VEI
		_VEhO <- pcmavo VEhO
		_VUhU <- pcmavo VUhU
		_VEhA <- pcmavo VEhA
		_VIhA <- pcmavo VIhA
		_VUhO <- pcmavo VUhO
		_XI   <- pcmavo XI
		_Y <- newRule $ peek cmavo ->> many1 y <<- peek post_word
		_ZAhO <- pcmavo ZAhO
		_ZEhA <- pcmavo ZEhA
		_ZEI  <- pcmavo ZEI
		_ZI   <- pcmavo ZI
		_ZIhE <- pcmavo ZIhE
		_ZO   <- pcmavo ZO
		_ZOI  <- pcmavo ZOI
		_ZOhU <- pcmavo ZOhU

		----------------------------------------------------------------

	return _UI -- words

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
pcat :: [P s a] -> P s [a]
pcat [p] = single p
pcat (p : ps) = p <:> pcat ps

parse_cmavo :: [(Char, P s Char)] -> P s a -> P s b -> CMAVO -> P s CMAVO
parse_cmavo dict pre post selmaho = let pairs = look selmaho cmavo_list in
	peek pre ->>
	choice (map (\(s, cm) -> pcat (map (flip look dict) s) ## const cm) pairs)
	<<- peek post

look :: (Eq a, Show a) => a -> [(a, b)] -> b
look x = fromMaybe (error $ "no such item " ++ show x) . lookup x

digitToPA :: Char -> CMAVO
digitToPA '0' = NO
digitToPA '1' = PA
digitToPA '2' = RE
digitToPA '3' = CI
digitToPA '4' = VO
digitToPA '5' = MU
digitToPA '6' = XA
digitToPA '7' = ZE
digitToPA '8' = BI
digitToPA '9' = SO
digitToPA _ = error "not digit"

data BRIVLA = BRIVLA String deriving Show
data CMENE = CMENE String deriving Show
data CMAVO
	= A    | E    | JI   | O    | U
	| DUhO | SIhU | ZAU  | KIhI | DUhI | CUhU | TUhI | TIhU | DIhO | JIhU
	| RIhA | NIhI | MUhI | KIhU | VAhU | KOI  | CAhI | TAhI | PUhE | JAhI
	| KAI  | BAI  | FIhE | DEhI | CIhO | MAU  | MUhU | RIhI | RAhI | KAhA
	| PAhU | PAhA | LEhA | KUhU | TAI  | BAU  | MAhI | CIhE | FAU  | POhI
	| CAU  | MAhE | CIhU | RAhA | PUhA | LIhE | LAhU | BAhI | KAhI | SAU
        | FAhE | BEhI | TIhI | JAhE | GAhA | VAhO | JIhO | MEhA | DOhE | JIhE
	| PIhO | GAU  | ZUhE | MEhE | RAI
	| BAhE | ZAhE
	| BE
	| BEI
	| BEhO
	| BIhE
	| MIhI | BIhO | BIhI
	| BO
	| BOI
	| BU
	| YBU  | JOhO | RUhO | GEhO | JEhO | LOhA | NAhA | SEhE | TOhA | GAhE
	| YhY  | BY   | CY   | DY   | FY   | GY   | JY   | KY   | LY   | MY
	| NY   | PY   | RY   | SY   | TY   | VY   | XY   | ZY
	| CAhA | PUhI | NUhO | KAhE
	| PEI  | CAI  | CUhI | SAI  | RUhE
	| CEI
	| CEhE
	| CO
	| JUhI | COI  | FIhI | TAhA | MUhO | FEhO | COhO | PEhU | KEhO | NUhE
	| REhI | BEhE | JEhE | MIhE | KIhE | VIhO
	| CU
	| CUhE | NAU
	| DAhO
	| DOI
	| DOhU
	| FAI  | FA   | FE   | FO   | FU   | FIhA | FI
	| DUhA | BEhA | NEhU | VUhA | GAhU | TIhA | NIhA | CAhU | ZUhA | RIhU
	| RUhU | REhO | TEhE | BUhU | NEhA | PAhO | NEhI | TOhO | ZOhI | ZEhO
	| ZOhA | FAhA
	| FAhO
	| FEhE
	| FEhU
	| FIhO
	| FOI
	| FUhA
	| FUhE
	| FUhO
	| GEhI | GE   | GO   | GA   | GU
	| KEhI | GAhO
	| GEhU
	| GI
	| GIhE | GIhI | GIhO | GIhA | GIhU
	| NOhU | NE   | GOI  | POhU | PE   | POhE | PO
	| MO   | NEI  | GOhU | GOhO | GOhI | NOhA | GOhE | GOhA | DU   | BUhA
	| BUhE | BUhI | COhE
	| GUhE | GUhI | GUhO | GUhA | GUhU
	| I
	| JEhI | JE   | JO   | JA   | JU
	| JAI
	| JOhI
	| FAhU | PIhU | JOI  | CEhO | CE   | JOhU | KUhA | JOhE | JUhE
	| KE
	| KEhE
	| KEI
	| KI
	| DAhU | DAhE | DIhU | DIhE | DEhU | DEhE | DEI  | DOhI | MIhO | MAhA
	| MIhA | DOhO | KOhA | FOhU | KOhE | KOhI | KOhO | KOhU | FOhA | FOhE
	| FOhI | FOhO | VOhA | VOhE | VOhI | VOhO | VOhU | RU   | RI   | RA
	| TA   | TU   | TI   | ZIhO | KEhA | MA   | ZUhI | ZOhE | CEhU | DA
	| DE   | DI   | KO   | MI   | DO
	| KU
	| KUhE
	| KUhO
	| LAI  | LAhI | LA
	| CEhA | LAU  | ZAI  | TAU
	| TUhA | LUhA | LUhO | LAhE | VUhI | LUhI | LUhE
	| LEI  | LOI  | LEhI | LOhI | LEhE | LOhE | LO   | LE
	| LEhU
	| MEhO | LI
	| LIhU
	| LOhO
	| LOhU
	| LU
	| LUhU
	| MAhO
	| MOhO | MAI
	| ME
	| MEhU
	| MOhE
	| MOhI
	| MEI  | MOI  | SIhE | CUhO | VAhE
	| JAhA | NA
	| NAI
	| TOhE | JEhA | NAhE | NOhE
	| NAhU
	| NIhE
	| NIhO | NOhI
	| VOI  | NOI  | POI
	| NI   | DUhU | SIhO | NU   | LIhI | KA   | JEI  | SUhU | ZUhO | MUhE
	| PUhU | ZAhI
	| NUhA
	| NUhI
	| NUhU
	| DAU  | FEI  | GAI  | JAU  | REI  | VAI  | PIhE | PI   | FIhU | ZAhU
	| MEhI | NIhU | KIhO | CEhI | MAhU | RAhE | DAhA | SOhA | JIhI | SUhO
	| SUhE | RO   | RAU  | SOhU | SOhI | SOhE | SOhO | MOhA | DUhE | TEhO
	| KAhO | CIhI | TUhO | XO   | PAI  | NOhO | NO   | PA   | RE   | CI
	| VO   | MU   | XA   | ZE   | BI   | SO
	| PEhE
	| PEhO
	| BA   | PU   | CA
	| RAhO
	| REhU | ROI
	| SA
	| SE   | TE   | VE   | XE
	| SEI  | TIhO
	| SEhU
	| SI
	| SOI
	| SU
	| RUhI | TAhE | DIhI | NAhO
	| TEhU
	| TEI
	| TOhI | TO
	| TOI
	| TUhE
	| TUhU
	| IhA  | IE   | AhE  | UhI  | IhO  | IhE  | AhA  | IA   | OhI  | OhE
	| EhE  | OI   | UO   | EhI  | UhO  | AU   | UA   | AhI  | IhU  | II
	| UhA  | UI   | AhO  | AI   | AhU  | IU   | EI   | OhO  | EhA  | UU
	| OhA  | OhU  | UhU  | EhO  | IO   | EhU  | UE   | IhI  | UhE  | BAhA
	| JAhO | CAhE | SUhA | TIhE | KAhU | SEhO | ZAhA | PEhI | RUhA | JUhA
	| TAhO | RAhU | LIhA | BAhU | MUhA | DOhA | TOhU | VAhI | PAhE | ZUhU
	| SAhE | LAhA | KEhU | SAhU | DAhI | JEhU | SAhA | KAU  | TAhU | NAhI
	| JOhA | BIhU | LIhO | PAU  | MIhU | KUhI | JIhA | SIhA | POhO | PEhA
	| ROhI | ROhE | ROhO | ROhU | ROhA | REhE | LEhO | JUhO | FUhI | DAI
	| GAhI | ZOhO | BEhU | RIhE | SEhI | SEhA | VUhE | KIhA | XU   | GEhE
	| BUhO
	| VI   | VA   | VU
	| VAU
	| VEI
	| VEhO
	| GEhA | FUhU | PIhI | FEhI | VUhU | SUhI | JUhU | GEI  | PAhI | FAhI
	| TEhA | CUhA | VAhA | NEhO | DEhO | FEhA | SAhO | REhA | RIhO | SAhI
	| PIhA | SIhI
	| VEhU | VEhA | VEhI | VEhE
	| VIhI | VIhA | VIhU | VIhE
	| VUhO
	| XI
	| Y
	| COhI | PUhO | COhU | MOhU | CAhO | COhA | DEhA | BAhO | DIhA | ZAhO
	| ZEhU | ZEhA | ZEhI | ZEhE
	| ZEI
	| ZU   | ZA   | ZI
	| ZIhE
	| ZO
	| ZOI  | LAhO
	| ZOhU
	deriving (Show, Eq)

cmavo_list :: [(CMAVO, [(String, CMAVO)])]
cmavo_list = [
	(A   ,[	("a"   , A   ), ("e"   , E   ), ("ji"  , JI  ), ("o"   , O   ),
		("u"   , U   ) ]),
	(BAI ,[	("duho", DUhO), ("sihu", SIhU), ("zau" , ZAU ), ("kihi", KIhI),
		("duhi", DUhI), ("cuhu", CUhU), ("tuhi", TUhI), ("tihu", TIhU),
		("diho", DIhO), ("jihu", JIhU), ("riha", RIhA), ("nihi", NIhI),
		("muhi", MUhI), ("kihu", KIhU), ("vahu", VAhU), ("koi" , KOI ),
		("cahi", CAhI), ("tahi", TAhI), ("puhe", PUhE), ("jahi", JAhI),
		("kai" , KAI ), ("bai" , BAI ), ("fihe", FIhE), ("dehi", DEhI),
		("ciho", CIhO), ("mau" , MAU ), ("muhu", MUhU), ("rihi", RIhI),
		("rahi", RAhI), ("kaha", KAhA), ("pahu", PAhU), ("paha", PAhA),
		("leha", LEhA), ("kuhu", KUhU), ("tai" , TAI ), ("bau" , BAU ),
		("mahi", MAhI), ("cihe", CIhE), ("fau" , FAU ), ("pohi", POhI),
		("cau" , CAU ), ("mahe", MAhE), ("cihu", CIhU), ("raha", RAhA),
		("puha", PUhA), ("lihe", LIhE), ("lahu", LAhU), ("bahi", BAhI),
		("kahi", KAhI), ("sau" , SAU ), ("fahe", FAhE), ("behi", BEhI),
		("tihi", TIhI), ("jahe", JAhE), ("gaha", GAhA), ("vaho", VAhO),
		("jiho", JIhO), ("meha", MEhA), ("dohe", DOhE), ("jihe", JIhE),
		("piho", PIhO), ("gau" , GAU ), ("zuhe", ZUhE), ("mehe", MEhE),
		("rai" , RAI ) ]),
	(BAhE,[	("bahe", BAhE), ("zahe", ZAhE) ]),
	(BE  ,[	("be"  , BE  ) ]),
	(BEI ,[	("bei" , BEI ) ]),
	(BEhO,[	("beho", BEhO) ]),
	(BIhE,[	("bihe", BIhE) ]),
	(BIhI,[	("mihi", MIhI), ("biho", BIhO), ("bihi", BIhI)]),
	(BO  ,[	("bo"  , BO  ) ]),
	(BOI ,[	("boi" , BOI ) ]),
	(BU  ,[	("bu"  , BU  ) ]),
	(BY  ,[	("joho", JOhO), ("ruho", RUhO), ("geho", GEhO), ("jeho", JEhO),
		("loha", LOhA), ("naha", NAhA), ("sehe", SEhE), ("toha", TOhA),
		("gahe", GAhE), ("yhy" , YhY ), ("by"  , BY  ), ("cy"  , CY  ),
		("dy"  , DY  ), ("fy"  , FY  ), ("gy"  , GY  ), ("jy"  , JY  ),
		("ky"  , KY  ), ("ly"  , LY  ), ("my"  , MY  ), ("ny"  , NY  ),
		("py"  , PY  ), ("ry"  , RY  ), ("sy"  , TY  ), ("vy"  , VY  ),
		("xy"  , XY  ), ("zy"  , ZY  ) ]),
	(CAhA,[	("caha", CAhA), ("puhi", PUhI), ("nuho", NUhO), ("kahe", KAhE) ]),
	(CAI ,[	("pei" , PEI ), ("cai" , CAI ), ("cuhi", CUhI), ("ruhe", RUhE) ]),
	(CEI ,[	("cei" , CEI ) ]),
	(CEhE,[	("cehe", CEhE) ]),
	(CO  ,[	("co"  , CO  ) ]),
	(COI ,[	("juhi", JUhI), ("coi" , COI ), ("fihi", FIhI), ("taha", TAhA),
		("muho", MUhO), ("feho", FEhO), ("coho", COhO), ("pehu", PEhU),
		("keho", KEhO), ("nuhe", NUhE), ("rehi", REhI), ("behe", BEhE),
		("jehe", JEhE), ("mihe", MIhE), ("kihe", KIhE), ("viho", VIhO) ]),
	(CU  ,[	("cu"  , CU  ) ]),
	(CUhE,[	("cuhe", CUhE), ("nau" , NAU ) ]),
	(DAhO,[	("daho", DAhO) ]),
	(DOI ,[	("doi" , DOI ) ]),
	(DOhU,[	("dohu", DOhU) ]),
	(FA  ,[	("fai" , FAI ), ("fa"  , FA  ), ("fe"  , FE  ), ("fo"  , FO  ),
		("fu"  , FU  ), ("fiha", FIhA), ("fi"  , FI  ) ]),
	(FAhA,[	("duha", DUhA), ("beha", BEhA), ("nehu", NEhU), ("vuha", VUhA),
		("gahu", GAhU), ("tiha", TIhA), ("niha", NIhA), ("cahu", CAhU),
		("zuha", ZUhA), ("rihu", RIhU), ("ruhu", RUhU), ("reho", REhO),
		("tehe", TEhE), ("buhu", BUhU), ("neha", NEhA), ("paho", PAhO),
		("nehi", NEhI), ("toho", TOhO), ("zohi", ZOhI), ("zeho", ZEhO),
		("zoha", ZOhA), ("faha", FAhA) ]),
	(FAhO,[	("faho", FAhO) ]),
	(FEhE,[	("fehe", FEhE) ]),
	(FEhU,[	("fehu", FEhU) ]),
	(FIhO,[	("fiho", FIhO) ]),
	(FOI ,[	("foi" , FOI ) ]),
	(FUhA,[	("fuha", FUhA) ]),
	(FUhE,[	("fuhe", FUhE) ]),
	(FUhO,[	("fuho", FUhO) ]),
	(GA  ,[	("gehi", GEhI), ("ge"  , GE  ), ("go"  , GO  ), ("ga"  , GA),
		("gu"  , GU  ) ]),
	(GAhO,[	("kehi", KEhI), ("gaho", GAhO) ]),
	(GEhU,[	("gehu", GEhU) ]),
	(GI  ,[	("gi"  , GI  ) ]),
	(GIhA,[	("gihe", GIhE), ("gihi", GIhI), ("giho", GIhO), ("giha", GIhA),
		("gihu", GIhU) ]),
	(GOI ,[	("nohu", NOhU), ("ne"  , NE  ), ("goi" , GOI ), ("pohu", POhU),
		("po"  , PO  ) ]),
	(GOhA,[	("mo"  , MO  ), ("nei" , NEI ), ("gohu", GOhU), ("goho", GOhO),
		("gohi", GOhI), ("noha", NOhA), ("gohe", GOhE), ("goha", GOhA),
		("du"  , DU  ), ("buha", BUhA), ("buhe", BUhE), ("buhi", BUhI),
		("cohe", COhE) ]),
	(GUhA,[	("guhe", GUhE), ("guhi", GUhI), ("guho", GUhO), ("guha", GUhA),
		("guhu", GUhU) ]),
	(I   ,[	("i"   , I   ) ]),
	(JA  ,[	("jehi", JEhI), ("je"  , JE  ), ("jo"  , JO  ), ("ja"  , JA  ),
		("ju"  , JU  ) ]),
	(JAI ,[	("jai" , JAI ) ]),
	(JOhI,[	("johi", JOhI) ]),
	(JOI ,[	("fahu", FAhU), ("pihu", PIhU), ("joi" , JOI ), ("ceho", CEhO),
		("ce"  , CE  ), ("johu", JOhU), ("kuha", KUhA), ("johe", JOhE),
		("juhe", JUhE) ]),
	(KE  ,[	("ke"  , KE  ) ]),
	(KEhE,[	("kehe", KEhE) ]),
	(KEI ,[	("kei" , KEI ) ]),
	(KI  ,[	("ki"  , KI  ) ]),
	(KOhA,[	("duhu", DUhU), ("dahe", DAhE), ("dihu", DIhU), ("dihe", DIhE),
		("dehu", DEhU), ("dehe", DEhE), ("dei" , DEI ), ("dohi", DOhI),
		("miho", MIhO), ("maha", MAhA), ("miha", MIhA), ("doho", DOhO),
		("koha", KOhA), ("fohu", FOhU), ("kohe", KOhE), ("kohi", KOhI),
		("koho", KOhO), ("foha", FOhA), ("fohe", FOhE), ("fohi", FOhI),
		("foho", FOhO), ("voha", VOhA), ("vohi", VOhI), ("voho", VOhO),
		("vohu", VOhU), ("ru"  , RU  ), ("ri"  , RI  ), ("ra"  , RA  ),
		("ta"  , TA  ), ("tu"  , TU  ), ("ti"  , TI  ), ("ziho", ZIhO),
		("kehe", KEhE), ("ma"  , MA  ), ("zuhi", ZUhI), ("zohe", ZOhE),
		("cehu", CEhU), ("da"  , DA  ), ("de"  , DE  ), ("di"  , DI  ),
		("ko"  , KO  ), ("mi"  , MI  ), ("do"  , DO  ) ]),
	(KU  ,[	("ku"  , KU  ) ]),
	(KUhE,[	("kuhe", KUhE) ]),
	(KUhO,[	("kuho", KUhO) ]),
	(LA  ,[	("lai" , LAI ), ("lahi", LAhI), ("la"  , LA  ) ]),
	(LAU ,[	("ceha", CEhA), ("lau" , LAU ), ("zai" , ZAI ), ("tau" , TAU ) ]),
	(LAhE,[	("tuha", TUhA), ("luha", LUhA), ("luho", LUhO), ("lahe", LAhE),
		("vuhi", VUhI), ("luhi", LUhI), ("luhe", LUhE) ]),
	(LE  ,[	("lei" , LEI ), ("loi" , LOI ), ("lehi", LEhI), ("loho", LOhO),
		("lo"  , LO  ), ("le"  , LE  ) ]),
	(LEhU,[	("lehu", LEhU) ]),
	(LI  ,[	("meho", MEhO), ("li"  , LI  ) ]),
	(LIhU,[	("lihu", LIhU) ]),
	(LOhO,[	("loho", LOhO) ]),
	(LOhU,[ ("lohu", LOhU) ]),
	(LU  ,[	("lu"  , LU  ) ]),
	(LUhU,[	("luhu", LUhU) ]),
	(MAhO,[	("maho", MAhO) ]),
	(MAI ,[	("moho", MOhO), ("mai" , MAI ) ]),
	(ME  ,[	("me"  , ME  ) ]),
	(MEhU,[	("mehu", MEhU) ]),
	(MOhE,[	("mohe", MOhE) ]),
	(MOhI,[	("mohi", MOhI) ]),
	(MOI ,[	("moi" , MOI ) ]),
	(NA  ,[	("jaha", JAhA), ("na"  , NA  ) ]),
	(NAI ,[	("nai" , NAI ) ]),
	(NAhE,[	("tohe", TOhE), ("jeha", JEhA), ("nahe", NAhE), ("nohe", NOhE) ]),
	(NAhU,[	("nahu", NAhU) ]),
	(NIhE,[	("nihe", NIhE) ]),
	(NIhO,[	("niho", NIhO), ("nohi", NOhI) ]),
	(NOI ,[	("voi" , VOI ), ("noi" , NOI ), ("poi" , POI ) ]),
	(NU  ,[	("ni"  , NI  ), ("duhu", DUhU), ("siho", SIhO), ("nu"  , NU  ),
		("lihi", LIhI), ("suhu", SUhU), ("zuho", ZUhO), ("muhe", MUhE),
		("puhu", PUhU), ("zahi", ZAhI) ]),
	(NUhA,[	("nuha", NUhA) ]),
	(NUhI,[	("nuhi", NUhI) ]),
	(NUhU,[	("nuhu", NUhU) ]),
	(PA  ,[	("dau" , DAU ), ("fei" , FEI ), ("gai" , GAI ), ("jau" , JAU ),
		("rei" , REI ), ("vai" , VAI ), ("pihe", PIhE), ("pi"  , PI  ),
		("fihu", FIhU), ("zahu", ZAhU), ("mehi", MEhI), ("nihu", NIhU),
		("kiho", KIhO), ("cehi", CEhI), ("mahu", MAhU), ("rahe", RAhE),
		("daha", DAhA), ("soha", SOhA), ("jihi", JIhI), ("suho", SUhO),
		("suhe", SUhE), ("ro"  , RO  ), ("rau" , RAU ), ("sohu", SOhU),
		("sohi", SOhI), ("sohe", SOhE), ("soho", SOhO), ("moha", MOhA),
		("duhe", DUhE), ("teho", TEhO), ("kaho", KAhO), ("cihi", CIhI),
		("tuho", TUhO), ("xo"  , XO  ), ("pai" , PAI ), ("noho", NOhO),
		("no"  , NO  ), ("pa"  , PA  ), ("re"  , RE  ), ("ci"  , CI  ),
		("vo"  , VO  ), ("mu"  , MU  ), ("xa"  , XA  ), ("ze"  , ZE  ),
		("bi"  , BI  ), ("so"  , SO  ) ]),
	(PEhE,[	("pehe", PEhE) ]),
	(PEhO,[	("peho", PEhO) ]),
	(PU  ,[	("ba"  , BA  ), ("pu"  , PU  ), ("ca"  , CA  ) ]),
	(RAhO,[	("raho", RAhO) ]),
	(ROI ,[	("rehu", REhU), ("roi" , ROI ) ]),
	(SA  ,[	("sa"  , SA  ) ]),
	(SE  ,[	("se"  , SE  ), ("te"  , TE  ), ("ve"  , VE ), ("xe"  , XE) ]),
	(SEI ,[	("sei" , SEI ), ("tiho", TIhO) ]),
	(SEhU,[	("sehu", SEhU) ]),
	(SI  ,[	("si"  , SI  ) ]),
	(SOI ,[	("soi" , SOI ) ]),
	(SU  ,[	("su"  , SU  ) ]),
	(TAhE,[	("ruhi", RUhI), ("tahe", TAhE), ("dihi", DIhI), ("naho", NAhO) ]),
	(TEhU,[	("tehu", TEhU) ]),
	(TEI ,[	("tei" , TEI ) ]),
	(TO  ,[	("tohi", TOhI), ("to"  , TO  ) ]),
	(TOI ,[	("toi" , TOI ) ]),
	(TUhE,[	("tuhe", TUhE) ]),
	(TUhU,[	("tuhu", TUhU) ]),
	(UI  ,[	("iha" , IhA ), ("ie"  , IE  ), ("ahe" , AhE ), ("uhi" , UhI ),
		("iho" , IhO ), ("ihe" , IhE ), ("aha" , AhA ), ("ia"  , IA  ),
		("ohi" , OhI ), ("ohe" , OhE ), ("ehe" , EhE ), ("oi"  , OI  ),
		("uo"  , UO  ), ("ehi" , EhI ), ("uho" , UhO ), ("au"  , AU  ),
		("ua"  , UA  ), ("ahi" , AhI ), ("ihu" , IhU ), ("ii"  , II  ),
		("uha" , UhA ), ("ui"  , UI  ), ("aho" , AhO ), ("ai"  , AI  ),
		("ahu" , AhU ), ("iu"  , IU  ), ("ei"  , EI  ), ("oho" , OhO ),
		("eha" , EhA ), ("uu"  , UU  ), ("oha" , OhA ), ("ohu" , OhU ),
		("uhu" , UhU ), ("eho" , EhO ), ("io"  , IO  ), ("ehu" , EhU ),
		("ue"  , UE  ), ("ihi" , IhI ), ("uhe" , UhE ), ("baha", BAhA),
		("jaho", JAhO), ("cahe", CAhE), ("suha", SUhA), ("tihe", TIhE),
		("kahu", KAhU), ("seho", SEhO), ("zaha", ZAhA), ("pehi", PEhI),
		("ruha", RUhA), ("juha", JUhA), ("taho", TAhO), ("rahu", RAhU),
		("liha", LIhA), ("bahu", BAhU), ("muha", MUhA), ("doha", DOhA),
		("tohu", TOhU), ("vahi", VAhI), ("pahe", PAhE), ("zuhu", ZUhU),
		("sahe", SAhE), ("laha", LAhA), ("kehu", KEhU), ("sahu", SAhU),
		("dahi", DAhI), ("jehu", JEhU), ("saha", SAhA), ("kau" , KAU ),
		("tahu", TAhU), ("nahi", NAhI), ("joha", JOhA), ("bihu", BIhU),
		("liho", LIhO), ("pau" , PAU ), ("mihu", MIhU), ("kuhi", KUhI),
		("jiha", JIhA), ("siha", SIhA), ("poho", POhO), ("peha", PEhA),
		("rohi", ROhI), ("rohe", ROhE), ("roho", ROhO), ("rohu", ROhU),
		("roha", ROhA), ("rehe", REhE), ("leho", LEhO), ("juho", JUhO),
		("fuhi", FUhI), ("dai" , DAI ), ("gahi", GAhI), ("zoho", ZOhO),
		("behu", BEhU), ("rihe", RIhE), ("sehi", SEhI), ("seha", SEhA),
		("vuhe", VUhE), ("kiha", KIhA), ("xu"  , XU  ), ("gehe", GEhE),
		("buho", BUhO) ]),
	(VA  ,[]),
	(VAU ,[]),
	(VEI ,[]),
	(VEhO,[]),
	(VUhU,[]),
	(VEhA,[]),
	(VIhA,[]),
	(VUhO,[]),
	(XI  ,[]),
	(Y   ,[]),
	(ZAhO,[]),
	(ZEhA,[]),
	(ZEI ,[]),
	(ZI  ,[]),
	(ZIhE,[]),
	(ZO  ,[]),
	(ZOI ,[]),
	(ZOhU,[])
 ]
