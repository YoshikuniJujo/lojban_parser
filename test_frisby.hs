{-# LANGUAGE DoRec, TupleSections #-}

module Main where

import CmavoList

import Text.Parsers.Frisby
import Data.Char
import Data.Maybe
-- import Control.Applicative hiding (many, optional)

main :: IO ()
main = do
	interact $ (++ "\n") . show . runPeg parser

-- parser :: PM s (P s (Clause [CMAVO] [CMAVO] [Indicators]))
parser = do
	rec
		let	clause p' = pre p' <> post_clause
				## \(x', y') -> case x' of
					Raw r' -> if null y' then Raw r'
						else Post r' y'
					Pre pr r' -> if null y' then Pre pr r'
						else Both pr r' y'
					_ -> error "not occur 7"
			clause_i p' = pre p' <<- post_clause_ind
			pre p' = pre_clause <> p' <<- optional spaces
				## \(x', y') -> if null x' then Raw y'
					else (Pre :: PreT a b) x' y'
			nai_i p' = p' <> option EmptyClause (clause_i _NAI)
				## (\(ui, nai') -> case nai' of
					Pre bahe _ -> (Pre :: PreT a b) bahe $
						Nai ui
					Raw _ -> Raw $ Nai ui
					EmptyClause -> Raw $ Aru ui
					_ -> error "not occur 8")
			nai p' = p' <> option EmptyClause (clause _NAI)
				## (\(ui, nai') -> case nai' of
					Pre bahe _ -> Pre bahe $ Nai ui
					Raw _ -> Raw $ Nai ui
					EmptyClause -> Raw $ Aru ui
					Post _ ind -> Post (Nai ui) ind
					Both bahe _ ind -> Both bahe (Nai ui) ind )
			addFree p' = p' <> many free	## uncurry AddFree

		----------------------------------------------------------------
		-- text

		----------------------------------------------------------------
		-- statement, paragraph

		----------------------------------------------------------------
		-- sentence, bridi

		----------------------------------------------------------------
		-- term

		----------------------------------------------------------------
		-- sumti

		li_clause <- newRule $ addFree (clause _LI) <> mex <>
			addFree (mb $ clause _LOhO)

		sumti_tail <- newRule sumti_tail_1
{-
		sumti_tail <- newRule
			$  mb (sumti_6 <> mb relative_clauses) <> sumti_tail_1
			// relative_clauses <> sumti_tail_1
-}

		sumti_tail_1 <- newRule
			$  selbri -- <> mb relative_clauses
			## STSelbri
			// quantifier <> selbri -- <> mb relative_clauses
			## \(x1, x2) -> STQSelbri x1 x2
--			// quantifier <> sumti

		----------------------------------------------------------------
		-- relative clause

--		relative_clause_1 <- 

		----------------------------------------------------------------
		-- selbri

		selbri <- newRule $ mb tag <> selbri_1
			## \(x1, x2) -> maybe x2 (flip SelbriTag x2) x1

		selbri_1 <- newRule
			$  selbri_2
			// addFree (clause _NA) <> selbri
			## uncurry SelbriNA

		selbri_2 <- newRule $ selbri_3 <>
			mb ((addFree $ clause _CO) <> selbri_2)
			## \(x1, x2) ->
				maybe x1 (uncurry $ Selbri2 x1) x2

		selbri_3 <- newRule $ many1 selbri_4
			## \x1 -> case x1 of
				[s] -> s
				_ -> Selbris x1

		selbri_4 <- newRule $ selbri_5 <> many
				(  joik_jek <> selbri_5
				## Left
				// joik <> mb stag <> addFree (clause _KE) <>
					selbri_3 <> addFree (mb $ clause _KEhE)
				## \((((x1, x2), x3), x4), x5) ->
					Right (x1, x2, x3, x4, x5) )
			## \(x1, x2) -> if null x2 then x1 else Selbri4 x1 x2

		selbri_5 <- newRule $ selbri_6 <> mb
			((jek ## Left // joik ## Right) <> mb stag <>
				addFree (clause _BO) <> selbri_5)
			## \(x1, x2) -> maybe x1
				(uncurry $ uncurry $ uncurry $ SelbriJekJoikBO x1)
				x2

		selbri_6 <- newRule
			$  tanru_unit <> mb(addFree (clause _BO) <> selbri_6)
			## \(x1, x2) -> maybe (STanruUnit x1) (uncurry $ SelbriBO x1) x2
--			// addFree (clause _NAhE) <> guhek <> selbri <> gik <>
--				selbri_6

		----------------------------------------------------------------
		-- tanru unit

		tanru_unit <- newRule $ tanru_unit_1 <>
			many (addFree (clause _CEI) <> tanru_unit_1)
			## \(x1, x2) -> if null x2 then x1 else TUCEI x1 x2

		tanru_unit_1 <- newRule $ tanru_unit_2 <> mb linkargs
			## \(x1, x2) -> maybe x1 (TanruUnit1 x1) x2

		tanru_unit_2 <- newRule
			$  addFree _BRIVLA_clause	## TUBrivla
			// clause _GOhA <> addFree (mb $ clause _RAhO)
			## (\(x1, x2) -> TUGOhA x1 x2)
--			// addFree (clause _KE) <> selbri_3 <>
--				addFree (clause _KEhE)
--			## (\((x1, x2), x3) -> TUKE x1 x2 x3)
--			// addFree (clause _ME) <>
--				(  sumti	## 
--				// lerfu_string	## ) <>
--				addFree (mb $ clause _MEhO) <>
--				addFree (mb $ clause _MOI)
--			## (\(((x1, x2), x3), x4) -> TUME x1 x2 x3 x4)
			// (number // lerfu_string) <>
				addFree (clause _MOI)
			## (\(x1, x2) -> TUMOI x1 x2)
			// addFree (clause _NUhA) <> mex_operator
			## (\(x1, x2) -> TUMex x1 x2)
			// addFree (clause _SE) <> tanru_unit_2
			## (\(x1, x2) -> TUSE x1 x2)
			// addFree (clause _JAI) <> mb tag <> tanru_unit_2
			## (\((x1, x2), x3) -> TUJAI x1 x2 x3)
			// addFree (clause _NAhE) <> tanru_unit_2
			## (\(x1, x2) -> TUNAhE x1 x2)
--			// (clause _NU) <> addFree (mb $ clause _NAI) <>
--				many (joik_jek <> clause _NU <>
--					addFree (mb $ clause _NAI))
--				subsentence <> addFree (mb $ clause _KEI)
--			##

		----------------------------------------------------------------
		-- linkargs

		linkargs <- newRule $ many linkargs_sa ->> linkargs_1
			## \(((x1, x2), x3), x4) -> LinkArgs x1 x2 x3 x4

		linkargs_1 <- newRule $ addFree (clause _BE) <> term <>
			many links <> addFree (mb $ clause _BEhO)

		linkargs_sa <- newRule $
			linkargs_start <> many (neek linkargs_start <>
			(  sa_word				## const ()
			// clause _SA <> neek linkargs_start	## const () )) <>
			clause _SA <> peek linkargs_1

		linkargs_start <- newRule $ clause _BE

		term <- newRule $ _KOhA

		links <- newRule $ many links_sa ->> links_1

		links_1 <- newRule $ addFree (clause _BEI) <> term

		links_sa <- newRule $ links_start <> many (neek links_start <>
			(  sa_word				## const ()
			// clause _SA <> neek links_start	## const () )) <>
			clause _SA <> peek links_1

		links_start <- newRule $ clause _BEI

		----------------------------------------------------------------
		-- mekso

		quantifier <- newRule
			$  (number <<- neek (clause _MOI)) <>
				addFree (mb $ clause _BOI)
			## (\(x1, x2) -> QNumber x1 x2)
			// addFree (clause _VEI) <> mex <> addFree (clause _VEhO)
			## (\((x1, x2), x3) -> QMex x1 x2 x3)

		mex <- newRule $ many mex_sa ->> mex_0

		mex_0 <- newRule
			$  mex_1 <> many (operator <> mex_1)
			## (\(x1, x2) -> Mex0 x1 x2)
			// rp_clause

		mex_sa <- newRule $
			mex_start <> many (neek mex_start <>
				(  sa_word			## const ()
				// clause _SA <> neek mex_start	## const () )) <>
			clause _SA <> peek mex_0
			## const ()

		mex_start <- newRule
			$  clause _FUhA		## const ()
			// clause _PEhO		## const ()
			// clause operand_start	## const ()

		rp_clause <- newRule $ addFree (clause _FUhA) <> rp_expression
			## \(x1, x2) -> MexRP x1 x2

		mex_1 <- newRule $
			mex_2 <> mb (addFree (clause _BIhE) <> operator <> mex_1)
			## \(x1, x2) -> Mex1 x1 x2

		mex_2 <- newRule
			$  operand
			## MexOperand
			// mex_forethougt

		mex_forethougt <- newRule $ addFree (mb $ clause _PEhO) <>
			operator <> fore_operands <> addFree (mb $ clause _KUhE)
			##  \(((x1, x2), x3), x4) ->
				MexForethought x1 x2 x3 x4
		fore_operands <- newRule $ many1 mex_2

		rp_expression <- newRule $ operand <> rp_expression_tail
			## \(x1, x2) -> RPExpression x1 x2
		rp_expression_tail <- newRule
			$  rp_expression <> operator <> rp_expression_tail
			## (\((x1, x2), x3) -> RPExpressionTail x1 x2 x3)
			// unit RPExpressionEmpty

		operator <- newRule $ many operator_sa ->> operator_0

		operator_0 <- newRule $ operator_1 <> many
			(  joik_jek <> operator_1 		## Left
			// joik <> mb stag <> addFree (clause _KE) <> operator <>
				addFree (mb $ clause _KEhE)	## Right )
			## uncurry Operator0

		operator_sa <- newRule $ operator_start <> many
			(neek operator_start <>
				(  sa_word				## const ()
				// _SA_clause <> neek operator_start	## const () )) <>
			clause _SA <> peek operator_0
			## const ()

		operator_start <- newRule
			$  guhek				## const ()
			// clause _KE				## const ()
			// mb (clause _SE) <> clause _NAhE	## const ()
			// mb (clause _SE) <> clause _MAhO	## const ()
			// mb (clause _SE) <> clause _VUhU	## const ()

		operator_1 <- newRule
			$  operator_2
			// guhek <> operator_1 <> gik <> operator_2
			## (\(((x1, x2), x3), x4) -> OperatorGuhek x1 x2 x3 x4)
			// operator_2 <> (jek ## Left // joik ## Right) <>
				mb stag <> addFree (clause _BO) <> operator_1
			## (\((((x1, x2), x3), x4), x5) ->
				OperatorJekJoik x1 x2 x3 x4 x5)

		operator_2 <- newRule
			$  mex_operator
			// addFree (clause _KE) <> operator <>
				addFree (clause _KEhE)
			## \((x1, x2), x3) -> OperatorKE x1 x2 x3

		mex_operator <- newRule
			$  addFree (clause _SE) <> mex_operator
			## (\(x1, x2) -> OperatorSE x1 x2)
			// addFree (clause _NAhE) <> mex_operator
			## (\(x1, x2) -> OperatorNAhE x1 x2)
			// addFree (clause _MAhO) <> mex <> addFree (clause _TEhU)
			## (\((x1, x2), x3) -> OperatorMAhO x1 x2 x3)
--			// addFree (clause _NAhU) <> selbri <> addFree (clause _TEhU)
--			## (\((x1, x2), x3) -> OperatorNAhU x1 x2 x3)
			// addFree (clause _VUhU)
			## OperatorVUhU

		operand <- newRule $ many operand_sa ->> operand_0
		operand_2 <- newRule $ operand_3 <> mb (
			joik_ek <> mb stag <> addFree (clause _BO) <> operand_2)
			## (\(x1, x2) -> Operand2 x1 x2)
		operand_3 <- newRule
			$  quantifier	## OperandQuantifier
			// (lerfu_string <<- neek (clause _MOI)) <>
				addFree (mb $ clause _BOI)
			## (\(x1, x2) -> OperandLerfu x1 x2)
--			// addFree (clause _NIhE) <> selbri <>
--				addFree (mb $ clause _TEhU)
--			// addFree (clause _MOhE) <> sumti <>
--				addFree (mb $ clause _TEhU)
			// addFree (clause _JOhI) <> many1 mex_2 <>
				addFree (mb $ clause _TEhU)
			## (\((x1, x2), x3) -> OperandJOhI x1 x2 x3)
			// gek <> operand <> gik <> operand_3
			## (\(((x1, x2), x3), x4) -> OperandGek x1 x2 x3 x4)
			//	(  addFree (clause _LAhE)		## Left
				// clause _NAhE <> addFree (clause _BO)	## Right ) <>
				operand <> addFree (mb $ clause _LUhU)
			## (\((x1, x2), x3) -> OperandLAhENAhE x1 x2 x3)

		let	operand_0 = operand_1 <> mb (
				joik_ek <> mb stag <>
				(addFree (clause _KE) ## \x' -> x' :: AddFree WordClause) <>
				operand <> (addFree (mb $ clause _KEhE)
					## \x' -> x' :: (AddFree (Maybe WordClause))) )
				## (\(x1, x2) -> Operand0 x1 x2)
			operand_sa = operand_start <> many (neek operand_start <>
				(  sa_word				## const ()
				// clause _SA <> neek operand_start	## const () )) <>
				clause _SA <> peek operand_0
			operand_start
				=  quantifier	## const ()
				// lerfu_word	## const ()
				// clause _NIhE	## const ()
				// clause _MOhE	## const ()
				// clause _JOhI	## const ()
				// gek		## const ()
				// clause _LAhE	## const ()
				// clause _NAhE	## const ()
			operand_1
				= operand_2 <> many (joik_ek <> operand_2)
			number = (clause _PA ## Right) <:> many
				(lerfu_word ## Left // clause _PA ## Right)
			lerfu_string = (lerfu_word ## Left) <:> many
				(lerfu_word ## Left // clause _PA ## Right)

		lerfu_word <- newRule
			$  _BY_clause
			// clause _LAU <> lerfu_word
				## (\(lau, lerfu) -> LLAU lau lerfu)
			// clause _TEI <> lerfu_string <<- clause _FOI
				## (\(_, ls) -> LTEI ls)

		----------------------------------------------------------------
		-- ek, gihek, jek, joik
		
		gihek <- newRule $ many gihek_sa ->> gihek_1
		joik_ek <- newRule $ many joik_ek_SA ->> joik_ek_1

		let	ek = mb (clause _NA) <> mb (clause _SE) <> nai (clause _A)
			gihek_1 = mb (clause _NA) <> mb (clause _SE) <>
				nai (clause _GIhA)
				## \((x1, x2), x3) -> Gihek x1 x2 x3
			gihek_sa = gihek_1 <> many (neek gihek_1 <>
				(  sa_word ## const ()
				// clause _SA <> neek gihek_1 ## const () )) <>
				(clause _SA) <> peek gihek
			jek = mb (clause _NA) <> mb (clause _SE) <> nai (clause _JA)
				## (\((x1, x2), x3) -> Jek x1 x2 x3)
			joik	=  mb (clause _SE) <> nai (clause _JOI)
				## (\(x1, x2) -> JoikJOI x1 x2)
				// interval	## JoikInterval
				// clause _GAhO <> interval <> clause _GAhO
				## (\((x1, x2), x3) -> JoikGAhO x1 x2 x3)
			interval = mb (clause _SE) <> nai (clause _BIhI)
				## \(x1, x2) -> Interval x1 x2
			joik_ek_1
				=  addFree joik	## JoikEkJoik
				// addFree ek	## JoikEkEk
			joik_ek_SA = joik_ek_1 <> (neek joik_ek_1 <> many
				(  sa_word			## const ()
				// clause _SA <> neek joik_ek_1	## const () )) <>
				clause _SA <> peek joik_ek
				## const ()
			joik_jek = addFree joik ## Left // addFree jek ## Right
			gek	=  mb (clause _SE) <> addFree (nai $ clause _GA)
				## (\(x1, x2) -> GekGA x1 x2)
				// joik <> addFree (clause _GI)
				## (\(x1, x2) -> GekJoik x1 x2)
				// stag <> gik
				## (\(x1, x2) -> GekStag x1 x2)
			guhek = mb (clause _SE) <> (addFree $ nai $ clause _GUhA)
				## \(x1, x2) -> Guhek x1 x2
			gik = addFree (nai (clause _GI))	## Gik

		----------------------------------------------------------------
		-- Tense

		let	tag = tense_modal <> many (joik_jek <> tense_modal)
				## (\(x1, x2) -> Tag x1 x2)
			stag =	(  simple_tense_modal <>
					many	(( jek ## Right . flip AddFree []
						// joik ## Left . flip AddFree [] )
							<> simple_tense_modal)
				// tense_modal <> many (joik_jek <> tense_modal) )
				## uncurry Tag

			tense_modal
				=  addFree simple_tense_modal
				## TMAddFree
{-
				// addFree clause _FIhO <> selbri <>
					addFree (mb $ clause _FEhU)
-}
			simple_tense_modal
				=  mb (clause _NAhE) <> mb (clause _SE) <>
					nai (clause _BAI) <> mb (clause _KI)
				## (\(((x1, x2), x3), x4) -> TMBAI x1 x2 x3 x4)
				// mb (clause _NAhE) <> (
					just (  just time <> mb space
					// mb time <> just space ) <>
						mb (clause _CAhA)
					// just (clause _CAhA)
					## (Nothing ,) ) <> mb (clause _KI)
				## (\((x1, (x2, x3)), x4) -> TMTimeSpace x1 x2 x3 x4)
				// clause _KI	## TMKI
				// clause _CUhE	## TMCUhE

			time =	(  (clause _ZI ## Just) <> many time_offset <>
					mb (clause _ZEhA <> mb (nai $ clause _PU)) <>
					many interval_property
				// mb (clause _ZI) <> many1 time_offset <>
					mb (clause _ZEhA <> mb (nai $ clause _PU)) <>
					many interval_property
				// mb (clause _ZI) <> many time_offset <>
					(clause _ZEhA <> mb (nai $ clause _PU)
						## Just) <>
					many interval_property
				// mb (clause _ZI) <> many time_offset <>
					mb (clause _ZEhA <> mb (nai $ clause _PU)) <>
					many1 interval_property )
				## \(((x1, x2), x3), x4) -> Time x1 x2 x3 x4
			time_offset =
				nai (clause _PU) <> mb (clause _ZI)
			space =	(  (clause _VA ## Just) <> (many space_offset) <>
					mb space_interval <>
					mb (clause _MOhI <> space_offset)
				// mb (clause _VA) <> (many1 space_offset) <>
					mb space_interval <>
					mb (clause _MOhI <> space_offset)
				// mb (clause _VA) <> (many space_offset) <>
					(space_interval ## Just) <>
					mb (clause _MOhI <> space_offset)
				// mb (clause _VA) <> (many space_offset) <>
					mb space_interval <>
					(clause _MOhI <> space_offset ## Just) )
				## \(((x1, x2), x3), x4) -> Space_ x1 x2 x3 x4
			space_offset = nai (clause _FAhA) <>
				option EmptyClause (clause _VA)
			space_interval =
				(  clause _VEhA <> option EmptyClause (clause _VIhA)
				// clause _VIhA		## (, EmptyClause) ) <>
				option [] space_int_props //
					space_int_props
					## ((EmptyClause, EmptyClause) ,)
					
			space_int_props = many1 (clause _FEhE <> interval_property)
			interval_property
				=  nai (number <> clause _ROI	## uncurry IROI)
				// nai (clause _TAhE		## ITAhE)
				// nai (clause _ZAhO		## IZAhO)

		----------------------------------------------------------------
		-- Free

		free <- newRule
--			$  clause _SEI <> many free <>
--				option .. (terms <> clause _CU <> many free) <>
--				selbri <> option EmptyClause (clause _SEhU)
--			// clause _SOI <> many free <> sumti <> option ... sumti <>
--				clause _SEhU
--			// vocative <> option ... relative_clauses <>
--				selbri <> option ... relative_clauses <>
--				option EmptyClause (clause _DOhU)
--			// vocative <> option ... relative_clauses <>
--				many1 (_clause _CMENE)  <> many free <>
--				option ... relative_clauses <>
--				option EmptyClause (clause _DOhU)
--			// vocative <> option ... sumti <>
--				option EmptyClause (clause _DOhU)
			$  (number // lerfu_string) <> clause _MAI
				## uncurry FMAI
--			// clause _TO <> text <> clause _TOI
			// xi_clause ## (\((xi, fr), cnt) -> FXI xi fr cnt)

		let	xi_clause =  clause _XI <> many free <>
				(number // lerfu_string) <<- optional (clause _BOI)
			vocative
				=  many1 (nai $ clause _COI) <>
					option EmptyClause (clause _DOI)
				// clause _DOI ## ([] ,)

		----------------------------------------------------------------
		-- Indicator

		let	indicators = option EmptyClause (clause_i _FUhE) <>
				many1 indicator
			indicator
				= nai_i (clause_i _UI // clause_i _CAI)
				// clause_i _DAhO ## Raw . Aru
				// clause_i _FUhO ## Raw . Aru

		----------------------------------------------------------------
		-- Magic Words

		zei_clause <- newRule $ pre zei_clause_no_pre
			## \x' -> case x' of
				Raw r_ -> r_
				Pre pr r_ -> case r_ of
					Raw r' -> Pre pr r'
					Pre pr' r' -> Pre (pr ++ pr') r'
					Post r' pst' -> Both pr r' pst'
					Both pr' r' pst' -> Both (pr ++ pr') r' pst'
					_ -> error "not occur 1"
				_ -> error "not occur 2"
		zei_clause_no_pre <- newRule $
			pre_zei_bu <> (manyCat (option [] zei_tail <++> bu_tail) <++>
			zei_tail) <> post_clause
			## \((x', y'), w) -> case x' of
				Raw r' -> if null w then Raw (r' : y')
						else Post (r' : y') w
				Pre pr r' -> if null w then Pre pr (r' : y')
						else Both pr (r' : y') w
				_ -> error "not occur 3"

		bu_clause <- newRule $ pre bu_clause_no_pre
			## \x' -> case x' of
				Raw r_ -> r_
				Pre pr r_ -> case r_ of
					Raw r' -> Pre pr r'
					Pre pr' r' -> Pre (pr ++ pr') r'
					Post r' pst' -> Both pr r' pst'
					Both pr' r' pst' -> Both (pr ++ pr') r' pst'
					_ -> error "not occur 4"
				_ -> error "not occur 5"
		bu_clause_no_pre <- newRule $
			pre_zei_bu <> (manyCat (option [] bu_tail <++> zei_tail) <++>
			bu_tail) <> post_clause
			## \((x', y'), w) -> case x' of
				Raw r' -> if null w then Raw (r' : y')
						else Post (r' : y') w
				Pre pr r' -> if null w then Pre pr (r' : y')
						else Both pr (r' : y') w
				_ -> error "not occur 6"
			
		let	zei_tail = many1Cat (_ZEI_clause <::> any_word)
			bu_tail = many1 _BU_clause

		pre_zei_bu <- newRule $ (
			neek _BU_clause ->> neek _ZEI_clause ->>
			neek _SI_clause ->> neek _SA_clause ->>
			neek (clause _SU) ->> neek _FAhO_clause ->>
			any_word_SA_handling ) <<- optional si_clause

		any_word <- newRule $
			(_BRIVLA // _CMENE // _CMAVO) <<- optional spaces

		let	dot_star = newRule $ many anyChar

		----------------------------------------------------------------
		-- General Morphology Issues

		post_clause <- newRule $
			optional spaces ->> optional si_clause ->>
			neek _ZEI_clause ->> neek _BU_clause ->> many indicators
		post_clause_ind <- newRule $
			optional spaces ->> optional si_clause <<-
			neek _ZEI_clause <<- neek _BU_clause
		let	pre_clause = _BAhE_clause
		any_word_SA_handling <- newRule $ choice
			[pre _BRIVLA, known_cmavo_SA, pre _CMAVO, pre _CMENE]
		known_cmavo_SA <- newRule $ choice $ map pre not_BAhE

		----------------------------------------------------------------
		-- SPACE
		
		si_clause <- newRule $ many1 (choice [
				erasable_clause		## const (),
				si_word			## const (),
				_SA_clause		## const () ] <>
			_SI_clause)

		erasable_clause <- newRule
			$  bu_clause_no_pre <<- neek _ZEI_clause <<- neek _BU_clause
				## const ()
			// zei_clause_no_pre <<- neek _ZEI_clause <<- neek _BU_clause
				## const ()

		let	sa_word = pre_zei_bu
			si_word = pre_zei_bu


		----------------------------------------------------------------
		-- SELMAHO

		_BAhE_clause <- newRule $ many (_BAhE_pre <<- _BAhE_post)
		_BAhE_pre <- newRule $ _BAhE <<- optional spaces
		_BAhE_post <- newRule $ {- optional si_clause <<- -}
			neek _ZEI_clause <<- neek _BU_clause

		_BRIVLA_clause <- newRule
			$  clause _BRIVLA	## BCBRIVLA
			// zei_clause		## BCZEI

		_BU_clause <- newRule $ _BU <<- optional spaces

		let	_BY_clause
				=  clause _BY	## LBY
				// bu_clause	## LBU

		_FAhO_clause <- newRule $ pre _FAhO <<- optional spaces

		_SA_clause <- newRule $ pre _SA
		_SI_clause <- newRule $ optional spaces ->> _SI <<- optional spaces

		_ZEI_clause <- newRule $ _ZEI <<- optional spaces
			

-- MORPHOLOGY	----------------------------------------------------------------

		_CMENE <- newRule $ cmene ## CMENE
		_BRIVLA <- newRule $ (gismu // lujvo // fuhivla) ## BRIVLA
		_CMAVO <- newRule $ choice all_cmavos // cmavo ## CMAVO

		let	all_cmavos = not_BAhE ++ [_BAhE]
			not_BAhE = [
				_A   , _BAI ,         _BE , _BEI , _BEhO,
				_BIhE, _BIhI, _BO  , _BOI , _BU  , _BY  ,
				_CAhA, _CAI , _CEI , _CEhE, _CO  , _COI ,
				_CU  , _CUhE, _DAhO, _DOI , _DOhU, _FA  ,
				_FAhA, _FAhO, _FEhE, _FEhU, _FIhO, _FOI ,
				_FUhA, _FUhE, _FUhO, _GA  , _GAhO, _GEhU,
				_GI  , _GIhA, _GOI , _GOhA, _GUhA, _I   ,
				_JA  , _JAI , _JOhI, _JOI , _KE  , _KEhE,
				_KEI , _KI  , _KOhA, _KU  , _KUhE, _KUhO,
				_LA  , _LAU , _LAhE, _LE  , _LEhU, _LI  ,
				_LIhU, _LOhO, _LOhU, _LU  , _LUhU, _MAhO,
				_MAI , _ME  , _MEhU, _MOhE, _MOhI, _MOI ,
				_NA  , _NAI , _NAhE, _NAhU, _NIhE, _NIhO,
				_NOI , _NU  , _NUhA, _NUhI, _PA  , _PEhE,
				_PEhO, _PU  , _RAhO, _ROI , _SA  , _SE  ,
				_SEI , _SEhU, _SI  , _SOI , _SU  , _TAhE,
				_TEhU, _TEI , _TO  , _TOI , _TUhE, _TUhU,
				_UI  , _VA  , _VAU , _VEI , _VEhO, _VUhU,
				_VEhA, _VIhA, _VUhO, _XI  , _ZAhO, _ZEhA,
				_ZEI , _ZI  , _ZIhE, _ZO  , _ZOI , _ZOhU ]

		----------------------------------------------------------------

		_words <- newRule $
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

		fuhivla <- newRule $ neek cmavo ->> neek slinkuhi ->>
			{-- fuhivla_head <++> --} stressed_syllable <++>
			many1Cat consonantal_syllable <++> final_syllable
			//
			{-- fuhivla_head <++> --} neek cmavo ->> unstressed_syllable <++>
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
		_FUhA <- pcmavo FUhA
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
		_JAI  <- pcmavo JAI
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

	return li_clause

alphabet :: Char -> P s Char
alphabet c = many comma ->> oneOf [c, toUpper c]
a, e, i, o, u, y :: P s Char
[a, e, i, o, u, y] = map alphabet "aeiouy"

pause :: P s ()
pause = discard (many comma <> space_char) // _EOF
_EOF :: P s ()
_EOF = many comma ->> neek anyChar
comma :: P s ()
comma = discard $ char ','
non_space :: P s Char
non_space = neek space_char ->> anyChar
space_char :: P s ()
space_char = discard $ choice [oneOf ".?! ", space_char1, space_char2]
space_char1, space_char2 :: P s Char
space_char1 = char '\t'
space_char2 = oneOf "\r\n"

neek :: P s a -> P s ()
neek = doesNotMatch
opt :: P s a -> P s [a]
opt p = option [] $ single p
(<:>) :: P s a -> P s [a] -> P s [a]
p <:> q = p <> q ## (uncurry (:))
(<::>) :: P s a -> P s a -> P s [a]
p <::> q = p <> q ## (\(c, d) -> [c, d])
manyCat :: P s [a] -> P s [a]
manyCat p = many p ## concat
many1Cat :: P s [a] -> P s [a]
many1Cat p = many1 p ## concat
(<+++>) :: P s String -> P s Char -> P s String
p <+++> q = p <> q ## (\(s, c) -> s ++ [c])
single :: P s a -> P s [a]
single = (## (: []))
pcat :: [P s a] -> P s [a]
pcat [p] = single p
pcat (p : ps) = p <:> pcat ps
pcat _ = error "pcat: empty"
mb :: P s a -> P s (Maybe a)
mb = option Nothing . (## Just)
just = (## Just)

parse_cmavo :: [(Char, P s Char)] -> P s a -> P s b -> CMAVO -> P s CMAVO
parse_cmavo dict pre post selmaho = let pairs = look selmaho cmavo_list in
	peek pre ->>
	choice (map (\(s, cm) -> pcat (map (flip look dict) s) ## const cm) pairs)
	<<- peek post

look :: (Eq a, Show a) => a -> [(a, b)] -> b
look x = fromMaybe (error $ "no such item " ++ show x) . lookup x

data SumtiTail
	= STSelbri Selbri
	| STQSelbri Quantifier Selbri
	deriving Show

data Selbri
	= STanruUnit TanruUnit
	| SelbriBO TanruUnit (AddFree WordClause) Selbri
	| SelbriJekJoikBO Selbri (Either Jek Joik) (Maybe Tag)
		(AddFree WordClause) Selbri
	| Selbri4 Selbri [Either (Either (AddFree Joik) (AddFree Jek), Selbri)
		(Joik, Maybe Tag, AddFree WordClause, Selbri,
		AddFree (Maybe WordClause))]
	| Selbri2 Selbri (AddFree WordClause) Selbri
	| Selbris [Selbri]
	| SelbriNA (AddFree WordClause) Selbri
	| SelbriTag Tag Selbri
	deriving Show

data TanruUnit
	= TUBrivla (AddFree BRIVLAClause)
	| TUGOhA WordClause (AddFree (Maybe WordClause))
	| TUMOI [Either Lerfu WordClause] (AddFree WordClause)
	| TUMex (AddFree WordClause) Operator
	| TUSE (AddFree WordClause) TanruUnit
	| TUJAI (AddFree WordClause) (Maybe Tag) TanruUnit
	| TUNAhE (AddFree WordClause) TanruUnit
	| TanruUnit1 TanruUnit LinkArgs
	| TUCEI TanruUnit [(AddFree WordClause, TanruUnit)]
	deriving Show

data BRIVLAClause
	= BCBRIVLA WordClause
	| BCZEI (Clause [CMAVO] [CMAVO] [Indicators])
	deriving Show

data LinkArgs = LinkArgs (AddFree WordClause) CMAVO [(AddFree WordClause, CMAVO)]
	(AddFree (Maybe WordClause))
	deriving Show

data Quantifier
	= QNumber [Either Lerfu WordClause] (AddFree (Maybe WordClause))
	| QMex (AddFree WordClause) Mex (AddFree WordClause)
	deriving Show

data Mex
	= MexOperand Operand
	| MexForethought (AddFree (Maybe WordClause))
		Operator
		[Mex] (AddFree (Maybe WordClause))
	| Mex1 Mex
		(Maybe ((AddFree WordClause, Operator), Mex))
	| Mex0 Mex [(Operator, Mex)]
	| MexRP (AddFree WordClause) RPExpression
	deriving Show

data RPExpression
	= RPExpression Operand RPExpressionTail
	deriving Show

data RPExpressionTail
	= RPExpressionTail RPExpression Operator RPExpressionTail
	| RPExpressionEmpty
	deriving Show

type TmpOperator = (Operator, [(Either (AddFree Joik) (AddFree Jek), Operator)])

data Operator
	= OperatorSE (AddFree WordClause) Operator
	| OperatorNAhE (AddFree WordClause) Operator
	| OperatorVUhU (AddFree WordClause)
	| OperatorGuhek Guhek Operator Gik Operator
	| OperatorJekJoik Operator (Either Jek Joik)
		(Maybe Tag) (AddFree WordClause) Operator
	| OperatorJoikJek Int Operator
	| Operator0 Operator [Either
		(Either (AddFree Joik) (AddFree Jek), Operator)
		((((Joik, Maybe Tag), AddFree WordClause), Operator),
			AddFree (Maybe WordClause))]
	| OperatorKE (AddFree WordClause) Operator (AddFree WordClause)
	| OperatorMAhO (AddFree WordClause) Mex (AddFree WordClause)
	deriving Show

data Operand
	= OperandLerfu [Either Lerfu WordClause] (AddFree (Maybe WordClause))
	| Operand2 Operand
		(Maybe (((JoikEk, Maybe Tag), AddFree WordClause), Operand))
	| Operand0 (Operand, [(JoikEk, Operand)]) (Maybe ((((JoikEk, Maybe Tag),
		AddFree WordClause), Operand), AddFree (Maybe WordClause)))
	| OperandQuantifier Quantifier
	| OperandGek Gek Operand Gik Operand
	| OperandLAhENAhE (Either (AddFree WordClause)
			(WordClause, AddFree WordClause))
		Operand (AddFree (Maybe WordClause))
	| OperandJOhI (AddFree WordClause) [Mex] (AddFree (Maybe WordClause))
	deriving Show

data Gihek = Gihek (Maybe WordClause) (Maybe WordClause) NaiClause
	deriving Show

type NaiClause = Clause [CMAVO] (Nai WordClause) [Indicators]

data Jek = Jek (Maybe WordClause) (Maybe WordClause)
		(Clause [CMAVO] (Nai WordClause) [Indicators])
	deriving Show

data JoikEk
	= JoikEkJoik (AddFree Joik)
	| JoikEkEk (AddFree ((Maybe WordClause, Maybe WordClause), NaiClause))
	deriving Show

data Joik
	= JoikJOI (Maybe WordClause) (Clause [CMAVO] (Nai WordClause) [Indicators])
	| JoikInterval Interval
	| JoikGAhO WordClause Interval WordClause
	deriving Show

data Interval = Interval (Maybe (Clause [CMAVO] CMAVO [Indicators]))
	(Clause [CMAVO] (Nai WordClause) [Indicators])
	deriving Show

data Gek
	= GekGA (Maybe WordClause)
		(AddFree (Clause [CMAVO] (Nai WordClause) [Indicators]))
	| GekJoik Joik (AddFree WordClause)
	| GekStag Tag Gik
	deriving Show

data Guhek = Guhek (Maybe WordClause)
	(AddFree (Clause [CMAVO] (Nai WordClause) [Indicators]))
	deriving Show

data Gik = Gik (AddFree (Clause [CMAVO] (Nai WordClause) [Indicators]))
	deriving Show

data Tag = Tag TenseModal [(Either (AddFree Joik) (AddFree Jek), TenseModal)]
	deriving Show

data Nai a = Nai a | Aru a deriving Show
data Clause a b c = Raw b | Pre a b | Post b c | Both a b c | EmptyClause
instance (Show a, Show b, Show c) => Show (Clause a b c) where
	show (Raw y') = "<" ++ show y' ++ ">"
	show (Pre x y') = "<" ++ show x ++ " " ++ show y' ++ ">"
	show (Post y' z) = "<" ++ show y' ++ " " ++ show z ++ ">"
	show (Both x y' z) = "<" ++ show x ++ " " ++ show y' ++ " " ++ show z ++ ">"
	show EmptyClause = "<>"
type PreT a b = a -> b -> Clause a b ()
type PostT b c = b -> c -> Clause () b c

type Indicators = (Clause [CMAVO] CMAVO (),
	[Clause [CMAVO] (Nai (Clause [CMAVO] CMAVO ())) ()])
type LerfuString = [Either Lerfu (Clause [CMAVO] CMAVO [Indicators])]
type WordClause = Clause [CMAVO] CMAVO [Indicators]

data Lerfu
	= LBY (Clause [CMAVO] CMAVO [Indicators])
	| LBU (Clause [CMAVO] [CMAVO] [Indicators])
	| LLAU (Clause [CMAVO] CMAVO [Indicators]) Lerfu
	| LTEI [Either Lerfu (Clause [CMAVO] CMAVO [Indicators])]
	deriving Show


data TenseModal
	= TMBAI (Maybe WordClause) (Maybe WordClause)
		(Clause [CMAVO] (Nai WordClause) [Indicators]) (Maybe WordClause)
	| TMTimeSpace (Maybe WordClause) (Maybe (Maybe Time,  Maybe Space_))
		(Maybe WordClause) (Maybe WordClause)
	| TMKI WordClause
	| TMCUhE WordClause
	| TMAddFree (AddFree TenseModal)
	deriving Show

data Time
	= Time (Maybe WordClause)
		[(Clause [CMAVO] (Nai WordClause) [Indicators], Maybe WordClause)]
		(Maybe (WordClause,
			Maybe (Clause [CMAVO] (Nai WordClause) [Indicators])))
		[Clause [CMAVO] (Nai IntervalProperty) [Indicators]]
	deriving Show

data Space_
	= Space_ (Maybe WordClause)
		[(Clause [CMAVO] (Nai WordClause) [Indicators], WordClause)]
		(Maybe ((WordClause, WordClause),
			[(WordClause, Clause [CMAVO] (Nai IntervalProperty)
				[Indicators])]))
		(Maybe (WordClause, (Clause [CMAVO] (Nai WordClause) [Indicators],
			WordClause)))
	deriving Show

data Free
	= FMAI LerfuString WordClause
	| FXI WordClause [Free] LerfuString
	deriving Show

data AddFree a = AddFree a [Free]
instance Show a => Show (AddFree a) where
	show (AddFree x f) = if null f then show x ++ " -"
		else show x ++ " - " ++ show f

data IntervalProperty
	= IROI LerfuString WordClause
	| ITAhE WordClause
	| IZAhO WordClause
	deriving Show
