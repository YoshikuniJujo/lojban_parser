{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Prelude hiding (words)

import Text.Peggy
import Data.List
import Data.Maybe
import Data.Char
import Control.Arrow

[peggy|

test_parser :: (Maybe Prenex, (Maybe Term, (Selbri, Maybe Term))) = text eof	{ $1 }

--* GRAMMAR *************************************************************** 23

text :: (Maybe Prenex, (Maybe Term, (Selbri, Maybe Term))) =
	intro_null
	_NAI_clause*
	text_1
	faho_clause eof?
	{ $3 }

intro_null :: () = spaces? su_clause* intro_si_clause	{ () }
text_part_2 :: () =
	( _CMENE_clause+ 	{ () }
	/ indicators?		{ () } ) free*
	{ () }

intro_si_clause :: () = si_clause? _SI_clause*		{ () }
faho_clause :: () = (_FAhO_clause dot_star)?		{ () }

text_1 :: (Maybe Prenex, (Maybe Term, (Selbri, Maybe Term)))
	= statement

statement :: (Maybe Prenex, (Maybe Term, (Selbri, Maybe Term)))
	= prenex? sentence

prenex :: Prenex
	= terms _ZOhU_clause		{ Prenex $1 $2 }

sentence :: (Maybe Term, (Selbri, Maybe Term))
	= terms? bridi_tail

bridi_tail :: (Selbri, Maybe Term)
	= selbri terms?

sentence_sa :: () =
	sentence_start
	(!sentence_start
		( sa_word			{ () }
		/ _SA_clause !sentence_start	{ () }))*
	_SA_clause
	&text_1
	{ () }

sentence_start :: () = _I_pre	{ () } / _NIhO_pre	{ () }

terms :: Term
	= term

term :: Term
	= term_1

term_1 :: Term
	= sumti		{ TSumti $1 }
	/ !gek	( tag			{ Left $1 }
		/ _FA_clause free*	{ Right $ AddFree $1 $2 } )
		( sumti			{ Left $1 }
		/ _KU_clause? free*	{ Right $ AddFree $1 $2 } )
	{ TTag $1 $2 }

sumti :: Sumti
	= sumti_1

sumti_1 :: Sumti
	= sumti_2

sumti_2 :: Sumti
	= sumti_3

sumti_3 :: Sumti
	= sumti_4

sumti_4 :: Sumti
	= sumti_5

sumti_5 :: Sumti
	= sumti_6 relative_clauses?	{ maybe $1 (SRelative $1) $2 }

sumti_6 :: Sumti
	= _ZO_clause			{ SQuote $1 }
	/ _ZOI_clause			{ SQuote $1 }
	/ lerfu_string			{ SLerfuStr $1 }
	/ _KOhA_clause free* 		{ if null $2 then SKOhA (NoF $1)
						else SKOhA (AddFree $1 $2) }
	/ _LA_clause _CMENE_clause+ free*
		{ SLA $1 $ if null $3 then NoF $2 else AddFree $2 $3 }
	/ _LE_clause selbri
		{ SLE $2 }

relative_clauses :: Relative
	= relative_clause (_ZIhE_clause relative_clause)*
		{ if null $2 then $1 else RMany $1 $2 }

relative_clause :: Relative
	= relative_clause_1

relative_clause_1 :: Relative
	= _GOI_clause term		{ RelativePhrase $1 $2 }
--	/ _NOI_clause subsentence

selbri :: Selbri
	= selbri_1

selbri_1 :: Selbri
	= selbri_2

selbri_2 :: Selbri
	= selbri_3

selbri_3 :: Selbri
	= selbri_4

selbri_4 :: Selbri
	= selbri_5

selbri_5 :: Selbri
	= selbri_6

selbri_6 :: Selbri
	= tanru_unit		{ STanruUnit $1 }

tanru_unit :: TanruUnit
	= tanru_unit_1

tanru_unit_1 :: TanruUnit
	= tanru_unit_2

tanru_unit_2 :: TanruUnit
	= _BRIVLA_clause free*	{ TUBRIVLA $ if null $2 then NoF $1
					else AddFree $1 $2 }

-- 245

mex :: Mex = _PA_clause		{ Mex $1 }

-- 355
number :: [Either (Clause Lerfu) (Clause PA)] = _PA_clause
	( _PA_clause	{ Right $1 }
	/ lerfu_word	{ Left $1 } )*
	{ Right $1 : $2 }

lerfu_string :: [Either (Clause Lerfu) (Clause PA)] = lerfu_word
	( _PA_clause	{ Right $1 }
	/ lerfu_word	{ Left $1 } )*
	{ Left $1 : $2 }

lerfu_word :: Clause Lerfu
	= _BY_clause
--	/ _LAU_clause lerfu_word
--	/ _TEI_clause lerfu_string _FOI_clause

ek :: Ek
	= _NA_clause? _SE_clause? _A_clause _NAI_clause?
	{ Ek $1 $2 $3 $4 }

gihek :: Gihek = gihek_sa* gihek_1	{ $2 }

gihek_1 :: Gihek
	= _NA_clause? _SE_clause? _GIhA_clause _NAI_clause?
	{ Gihek $1 $2 $3 $4 }

gihek_sa :: () = gihek_1 (!gihek_1
		( sa_word		{ () }
		/ _SA_clause !gihek_1	{ () } ))
	_SA_clause &gihek
	{ () }

jek :: Jek
	= _NA_clause? _SE_clause? _JA_clause _NAI_clause?
		{ Jek $1 $2 $3 $4 }

joik :: Joik
	= _SE_clause? _JOI_clause _NAI_clause?
		{ Joik $1 $2 $3 }
	/ interval
		{ JoikInterval Nothing $1 Nothing }
	/ _GAhO_clause interval _GAhO_clause
		{ JoikInterval (Just $1) $2 (Just $3) }

interval :: (Maybe (Clause SE), Clause Unit, Maybe (Clause Unit))
	= _SE_clause? _BIhI_clause _NAI_clause?

joik_ek :: AddFree (Either Joik Ek) = joik_ek_sa* joik_ek_1	{ $2 }

joik_ek_1 :: AddFree (Either Joik Ek)
	= joik free*	{ AddFree (Left $1) $2 }
	/ ek free*	{ AddFree (Right $1) $2 }

joik_ek_sa :: () = joik_ek_1 (!joik_ek_1
		( sa_word		{ () }
		/ _SA_clause !joik_ek_1	{ () } ))*
	_SA_clause &joik_ek
	{ () }

joik_jek :: AddFree (Either Joik Jek)
	= joik free*	{ if null $2 then NoF (Left $1)
				else AddFree (Left $1) $2 }
	/ jek free*	{ if null $2 then NoF (Right $1)
				else AddFree (Right $1) $2 }

gek :: AddFree Gek
	= _SE_clause? _GA_clause _NAI_clause? free*
		{ AddFree (GekGA $1 $2 $3) $4 }
	/ joik _GI_clause free*
		{ AddFree Gek $3 }
	/ stag gik
		{ NoF Gek }

guhek :: AddFree Guhek
	= _SE_clause? _GUhA_clause _NAI_clause? free*
	{ AddFree (Guhek $1 $2 $3) $4 }

gik :: AddFree (Clause Unit, Bool)
	= _GI_clause _NAI_clause? free*	{ AddFree ($1, (isJust $2)) $3 }

tag :: Tag
	= tense_modal (joik_jek tense_modal)*	{ Tag $1 $2 }

stag :: Tag
	= simple_tense_modal
		((jek { Right $1 } / joik { Left $1 } ) simple_tense_modal)*
		{ Stag $1 $2 }
	/ tense_modal (joik_jek tense_modal)*
		{ Tag $1 $2 }

tense_modal :: AddFree TenseModal
	= simple_tense_modal free*	{ if null $2 then NoF $1
						else AddFree $1 $2 }
	/ _FIhO_clause free* selbri _FEhU_clause? free*
		{ if null $ $2 ++ $5 then NoF (TMFIhO $3)
			else AddFree (TMFIhO $3) $ $2 ++ $5 }

simple_tense_modal :: TenseModal
	= _NAhE_clause? _SE_clause? _BAI_clause _NAI_clause? _KI_clause?
		{ TMBAI $1 $2 $3 $4 $5 }
	/ _NAhE_clause?
		( (time space_? {TimeSpace $1 $2} / space_ time? {SpaceTime $1 $2})
			_CAhA_clause
			{ (Just $1, Just $2) }
		/ (time space_? {TimeSpace $1 $2} / space_ time? {SpaceTime $1 $2})
			{ (Just $1, Nothing) }
		/ _CAhA_clause
			{ (Nothing, Just $1) } )
		_KI_clause?
			{ TMTense $1 $2 $3 }
	/ _KI_clause	{ TMKI $1 }
	/ _CUhE_clause	{ TMCUhE $1 }

time :: Time
	= _ZI_clause time_offset*
		(_ZEhA_clause (_PU_clause _NAI_clause?)?)? interval_property*
		{ Time (Just $1) $2 $3 $4 }
	/ _ZI_clause? time_offset+
		(_ZEhA_clause (_PU_clause _NAI_clause?)?)? interval_property*
		{ Time $1 $2 $3 $4 }
	/ _ZI_clause? time_offset*
		(_ZEhA_clause (_PU_clause _NAI_clause?)?) interval_property*
		{ Time $1 $2 (Just $3) $4 }
	/ _ZI_clause? time_offset*
		(_ZEhA_clause (_PU_clause _NAI_clause?)?)? interval_property+
		{ Time $1 $2 $3 $4 }

time_offset :: TimeOffset
	= _PU_clause _NAI_clause? _ZI_clause?
	{ TimeOffset $1 $2 $3 }

space_ :: Space_
	= _VA_clause space_offset* space_interval? (_MOhI_clause space_offset)?
	{ Space_ (Just $1) $2 $3 $4 }

space_offset :: (Clause FAhA, Maybe (Clause Unit), Maybe (Clause VA))
	= _FAhA_clause _NAI_clause? _VA_clause?

space_interval :: SpaceInterval
	=	( _VEhA_clause			{ (Just $1, Nothing) }
		/ _VIhA_clause			{ (Nothing, Just $1) }
		/ _VEhA_clause _VIhA_clause	{ (Just $1, Just $2) } )
			(_FAhA_clause _NAI_clause?)? space_int_props
				{ SIVVSIP $1 $2 $3 }
	/	( _VEhA_clause			{ (Just $1, Nothing) }
		/ _VIhA_clause			{ (Nothing, Just $1) }
		/ _VEhA_clause _VIhA_clause	{ (Just $1, Just $2) } )
			(_FAhA_clause _NAI_clause?)?
				{ SIVV $1 $2 }
	/ space_int_props	{ SISIP $1 }

space_int_props :: [(Clause Unit, Interval)]
	= (_FEhE_clause interval_property)+

interval_property :: Interval
	= number _ROI_clause _NAI_clause?	{ IROI $1 $2 }
	/ _TAhE_clause _NAI_clause?		{ ITAhE $1 $2 }
	/ _ZAhO_clause _NAI_clause?		{ IZAhO $1 $2 }

free :: Free
	= _SEI_clause free* (terms _CU_clause? free*)? selbri _SEhU_clause?
		{ FSEI $2 $3 $4 }
	/ _SOI_clause free* sumti sumti? _SEhU_clause?
		{ FSOI $2 $3 $4 }
	/ vocative relative_clauses? selbri relative_clauses? _DOhU_clause?
		{ FVocativeSelbri $1 $2 $3 $4 }
	/ vocative relative_clauses? _CMENE_clause+ free* relative_clauses?
		_DOhU_clause?
		{ FVocativeCMENE $1 $2 $3 $4 $5 }
	/ vocative sumti? _DOhU_clause?		{ FVocativeSumti $1 $2 }
	/ (number / lerfu_string) _MAI_clause	{ FMAI $1 $2 }
	/ xi_clause				{ FXI $1 }

-- 459
xi_clause :: AddFree XIString -- [Either (Clause Lerfu) (Clause PA)]
	= _XI_clause free* (number / lerfu_string) _BOI_clause?
	{ if null $2 then NoF $ XIString $3 else AddFree (XIString $3) $2 }
	/ _XI_clause free* _VEI_clause free* mex _VEhO_clause?
	{ if null ($2 ++ $4) then NoF (XIMex $5)
		else AddFree (XIMex $5) ($2 ++ $4) }

vocative :: Vocative
	= (_COI_clause _NAI_clause?)+ _DOI_clause
	{ Vocative $1 (Just $2) }
	/ (_COI_clause _NAI_clause?) (_COI_clause _NAI_clause?)*
	{ Vocative [$1] Nothing }
	/ _DOI_clause
	{ Vocative [] (Just $1) }

indicators :: Indicators = _FUhE_clause? indicator+
	{ maybe (Ind $2) (const $ IFUhE $2) $1 }
indicator :: Indicator =
	( (_UI_clause { Left $1 } / _CAI_clause { Right $1 }) _NAI_clause?
				{ case ($1, $2) of
					(Left ui, Nothing) -> IUI ui
					(Left ui, Just nai) -> IUINAI ui nai
					(Right cai, Nothing) -> ICAI cai
					(Right cai, Just nai) -> ICAINAI cai nai }
	/ _DAhO_clause		{ IDAhO $1 }
	/ _FUhO_clause		{ IFUhO $1 } )
	!_BU_clause

-- ****************** 473
-- Magic Words
-- ******************

zei_clause :: ([BAhE], BRIVLA, [Indicators])
	= pre_clause zei_clause_no_pre
	{ let (pre, zei, post) = $2 in ($1 ++ pre, zei, post) }
zei_clause_no_pre :: ([BAhE], BRIVLA, [Indicators])
	= pre_zei_bu (zei_tail? bu_tail)* zei_tail post_clause
	{ (fst $1, ZEI (snd $1) (map (fromMaybe [] . fst) $2) $3, $4) }
zei_clause_no_SA :: ()
	= pre_zei_bu_no_SA (zei_tail? bu_tail)* zei_tail	{ () }

bu_clause :: Clause Lerfu = pre_clause bu_clause_no_pre
	{ let (pre, l, post) = $2
		in prePost (LBU (fst l) (snd l)) ($1 ++ pre) post }
bu_clause_no_pre :: ([BAhE], (Word, [(Bool, [String])]), [Indicators])
	= pre_zei_bu (bu_tail? zei_tail)* bu_tail post_clause
	{ (fst $1, (snd $1, map (first isJust) $2), $4) }
bu_clause_no_SA :: ()
	= pre_zei_bu_no_SA (bu_tail? zei_tail)* bu_tail		{ () }

zei_tail :: [String] = (_ZEI_clause any_word)+		{ map snd $1 }
bu_tail :: () = _BU_clause+				{ () }

pre_zei_bu :: ([BAhE], Word)
	= (!_BU_clause !_ZEI_clause !_SI_clause !_SA_clause !_SU_clause
		!_FAhO_clause any_word_SA_handling) si_clause?
	{ $1 }
pre_zei_bu_no_SA :: ()
	= _LOhU_pre	{ () }
	/ _ZO_pre	{ () }
	/ _ZOI_pre	{ () }
	/ !_ZEI_clause !_BU_clause !_FAhO_clause !_SI_clause !_SA_clause
		!_SU_clause any_word si_clause?	{ () }

any_word :: String = lojban_word spaces?	{ $1 }
dot_star :: () = .*				{ () }

-- General Morphology Issues 498

-- 1. Spaces (including '.y') and UI are eaten *after* a word.

-- 3. BAhE is eaten *before* a word.

-- Handling of what can go after a cmavo
post_clause :: [Indicators]
	= spaces? si_clause? !_ZEI_clause !_BU_clause indicators*	{ $3 }
post_clause_ind :: ()
	= spaces? si_clause? !_ZEI_clause !_BU_clause			{ () }

pre_clause :: [BAhE] = _BAhE_clause?			{ fromMaybe [] $1 }

any_word_SA_handling :: ([BAhE], Word)
	= _BRIVLA_pre	{ second WBRIVLA $1 }
	/ known_cmavo_SA
	/ _CMAVO_pre
	/ _CMENE_pre	{ second WCMENE $1 }

known_cmavo_SA :: ([BAhE], Word)
	= _A_pre    { second WA    $1 } / _BAI_pre  { second WBAI  $1 }
	/ _BAhE_pre { ([], WBAhE $1)  } / _BE_pre   { ($1, WBE)       }
	/ _BEI_pre  { ($1, WBEI)      } / _BEhO_pre { ($1, WBEhO)     }
	/ _BIhE_pre { ($1, WBIhE)     } / _BIhI_pre { ($1, WBIhI)     }
	/ _BO_pre   { ($1, WBO)       } / _BOI_pre  { ($1, WBOI)      }
	/ _BOI_pre  { ($1, WBOI)      } / _BU_pre   { ($1, WBU)       }
	/ _BY_pre   { second WBY $1   } / _CAI_pre  { second WCAI $1  }
	/ _CAhA_pre { second WCAhA $1 } / _CEI_pre  { ($1, WCEI)      }
	/ _CEhE_pre { ($1, WCEhE)     } / _CO_pre   { ($1, WCO )      }
	/ _COI_pre  { second WCOI $1  } / _CU_pre   { ($1, WCU )      }
	/ _CUhE_pre { second WCUhE $1 } / _DAhO_pre { ($1, WDAhO)     }
	/ _DOI_pre  { ($1, WDOI)      } / _DOhU_pre { ($1, WDOhU)     }
	/ _FA_pre   { second WFA $1   } / _FAhA_pre { second WFAhA $1 }
	/ _FEhE_pre { ($1, WFEhE)     } / _FEhU_pre { ($1, WFEhU)     }
	/ _FIhO_pre { ($1, WFIhO)     } / _FOI_pre  { ($1, WFOI)      }
	/ _FUhA_pre { ($1, WFUhA)     } / _FUhE_pre { ($1, WFUhE)     }
	/ _FUhO_pre { ($1, WFUhO)     } / _GA_pre   { second WGA $1   }
	/ _GAhO_pre { second WGAhO $1 } / _GEhU_pre { ($1, WGEhU)     }
	/ _GI_pre   { ($1, WGI)       } / _GIhA_pre { second WGIhA $1 }
	/ _GOI_pre  { second WGOI $1  } / _GOhA_pre { second WGOhA $1 }
	/ _GUhA_pre { second WGUhA $1 } / _I_pre    { ($1, WI)        }
	/ _JA_pre   { second WJA $1   } / _JAI_pre  { ($1, WJAI)      }
	/ _JOI_pre  { second WJOI $1  } / _JOhI_pre { ($1, WJOhI)     }
	/ _KE_pre   { ($1, WKE)       } / _KEI_pre  { ($1, WKEI)      }
	/ _KEhE_pre { ($1, WKEhE)     } / _KI_pre   { ($1, WKI)       }
	/ _KOhA_pre { second WKOhA $1 } / _KU_pre   { ($1, WKU)       }
	/ _KUhE_pre { ($1, WKUhE)     } / _KUhO_pre { ($1, WKUhO)     }
	/ _LA_pre   { second WLA $1   } / _LAU_pre  { second WLAU $1  }
	/ _LAhE_pre { second WLAhE $1 } / _LE_pre   { second WLE $1   }
	/ _LEhU_pre { ($1, WLEhU)     } / _LI_pre   { ($1, WLI)       }
	/ _LIhU_pre { ($1, WLIhU)     } / _LOhO_pre { ($1, WLOhO)     }
	/ _LOhU_pre { ($1, WLOhU)     } / _LU_pre   { ($1, WLU)       }
	/ _LUhU_pre { ($1, WLUhU)     } / _MAI_pre  { second WMAI $1  }
	/ _MAhO_pre { ($1, WMAhO)     } / _ME_pre   { ($1, WME)       }
	/ _MEhU_pre { ($1, WMEhU)     } / _MOI_pre  { second WMOI $1  }
	/ _MOhE_pre { ($1, WMOhE)     } / _MOhI_pre { ($1, WMOhI)     }
	/ _NA_pre   { second WNA $1   } / _NAI_pre  { ($1, WNAI)      }
	/ _NAhE_pre { second WNAhE $1 } / _NAhU_pre { ($1, WNAhU)     }
	/ _NIhE_pre { ($1, WNIhE)     } / _NIhO_pre { second WNIhO $1 }
	/ _NOI_pre  { second WNOI $1  } / _NU_pre   { second WNU $1   }
	/ _NUhA_pre { ($1, WNUhA)     } / _NUhI_pre { ($1, WNUhI)     }
	/ _NUhU_pre { ($1, WNUhU)     } / _PA_pre   { second WPA $1   }
	/ _PEhE_pre { ($1, WPEhE)     } / _PEhO_pre { ($1, WPEhO)     }
	/ _PU_pre   { second WPU $1   } / _RAhO_pre { ($1, WRAhO)     }
	/ _ROI_pre  { second WROI $1  } / _SA_pre   { ($1, WSA)       }
	/ _SE_pre   { second WSE $1   } / _SEI_pre  { second WSEI $1  }
	/ _SEhU_pre { ($1, WSEhU)     } / _SI_clause{ ([], WSI)       }
	/ _SOI_pre  { ($1, WSOI)      } / _SU_pre   { ($1, WSU)       }
	/ _TAhE_pre { second WTAhE $1 } / _TEI_pre  { ($1, WTEI)      }
	/ _TEhU_pre { ($1, WTEhU)     } / _TO_pre   { second WTO $1   }
	/ _TOI_pre  { ($1, WTOI)      } / _TUhE_pre { ($1, WTUhE)     }
	/ _TUhU_pre { ($1, WTUhU)     } / _UI_pre   { second WUI $1   }
	/ _VA_pre   { second WVA $1   } / _VAU_pre  { ($1, WVAU)      }
	/ _VEI_pre  { ($1, WVEI)      } / _VEhA_pre { second WVEhA $1 }
	/ _VEhO_pre { ($1, WVEhO)     } / _VIhA_pre { second WVIhA $1 }
	/ _VUhO_pre { ($1, WVUhO)     } / _VUhU_pre { second WVUhU $1 }
	/ _XI_pre   { ($1, WXI)       } / _ZAhO_pre { second WZAhO $1 }
	/ _ZEI_pre  { ($1, WZEI)      } / _ZEhA_pre { second WZEhA $1 }
	/ _ZI_pre   { second WZI $1   } / _ZIhE_pre { ($1, WZIhE)     }
	/ _ZO_pre   { (fst $1, WZO)   } / _ZOI_pre  { second WZOI $ fst $1 }
	/ _ZOhU_pre { ($1, WZOhU)     }

-- Handling of spaces and things like spaces.
--- SPACE --- 534

-- SU clauses
su_clause :: () = (erasable_clause / su_word)* _SU_clause	{ () }

-- Handling of SI and interactions with zo and lo'u...le'u

si_clause :: ()
	= ((erasable_clause / si_word { () } / _SA_clause { () })
		si_clause? _SI_clause)+				{ () }

erasable_clause :: ()
	= bu_clause_no_pre !_ZEI_clause !_BU_clause		{ () }
	/ zei_clause_no_pre !_ZEI_clause !_BU_clause		{ () }

sa_word :: ([BAhE], Word) = pre_zei_bu

si_word :: ([BAhE], Word) = pre_zei_bu

su_word :: () = !_NIhO_clause !_LU_clause !_TUhE_clause !_TO_clause !_SU_clause
	!_FAhO_clause any_word_SA_handling			{ () }

--- SELMAHO --------------------------------------------------------------- 557

_BRIVLA_clause :: Clause BRIVLA
	= _BRIVLA_pre _BRIVLA_post	{ prePost (snd $1) (fst $1) $2 }
	/ zei_clause			{ let (pre, b, post) = $1 in
						prePost b pre post }
_BRIVLA_pre :: ([BAhE], BRIVLA)
	= pre_clause _BRIVLA spaces?	{ ($1, $2) }
_BRIVLA_post :: [Indicators] = post_clause
_BRIVLA_no_SA_handling :: ()
	= pre_clause _BRIVLA post_clause{ () }
	/ zei_clause_no_SA		{ () }

_CMENE_clause :: Clause CMENE
	= _CMENE_pre _CMENE_post	{ prePost (snd $1) (fst $1) $2 }
_CMENE_pre :: ([BAhE], CMENE) = pre_clause _CMENE spaces?	{ ($1, $2) }
_CMENE_post :: [Indicators] = post_clause
_CMENE_no_SA_handling :: () = pre_clause _CMENE post_clause	{ () }

_CMAVO_clause :: Clause Word
	= _CMAVO_pre _CMAVO_post	{ prePost (snd $1) (fst $1) $2 }
_CMAVO_pre :: ([BAhE], Word) = pre_clause _CMAVO spaces?	{ ($1, $2) }
_CMAVO_post :: [Indicators] = post_clause
_CMAVO_no_SA_handling :: () = pre_clause _CMAVO post_clause	{ () }

--	*** A: eks; basic afterthought logical connectives ***
_A_clause :: Clause A = _A_pre _A_post		{ prePost (snd $1) (fst $1) $2 }
_A_pre :: ([BAhE], A) = pre_clause _A spaces?	{ ($1, $2) }
_A_post :: [Indicators] = post_clause
_A_no_SA_handling :: () = pre_clause _A post_clause	{ () }

--	*** BAI: modal operators ***
_BAI_clause :: Clause BAI = _BAI_pre _BAI_post	{ prePost (snd $1) (fst $1) $2 }
_BAI_pre :: ([BAhE], BAI) = pre_clause _BAI spaces?	{ ($1, $2) }
_BAI_post :: [Indicators] = post_clause
_BAI_no_SA_handling :: () = pre_clause _BAI post_clause	{ () }

--	*** BAhE: next word intensifier ***
_BAhE_clause :: [BAhE] = (_BAhE_pre _BAhE_post)+		{ map fst $1 }
_BAhE_pre :: BAhE = _BAhE spaces?				{ $1 }
_BAhE_post :: () = si_clause? !_ZEI_clause !_BU_clause		{ () }
_BAhE_no_SA_handling :: () = _BAhE spaces? _BAhE_post		{ () }

--	*** BE: sumti link to attach sumti to a selbri
_BE_clause :: Clause Unit = _BE_pre _BE_post	{ prePost () $1 $2 }
_BE_pre :: [BAhE] = pre_clause _BE spaces?		{ $1 }
_BE_post :: [Indicators] = post_clause
_BE_no_SA_handling :: () = pre_clause _BE post_clause	{ () }

--	*** BEI: multiple sumti separator between BE, BEI
_BEI_clause :: Clause Unit = _BEI_pre _BEI_post	{ prePost () $1 $2 }
_BEI_pre :: [BAhE] = pre_clause _BEI spaces?		{ $1 }
_BEI_post :: [Indicators] = post_clause
_BEI_no_SA_handling :: () = pre_clause _BEI post_clause	{ () }

--	*** BEhO: terminates BEBEI specified descriptors
_BEhO_clause :: Clause Unit = _BEhO_pre _BEhO_post	{ prePost () $1 $2 }
_BEhO_pre :: [BAhE] = pre_clause _BEhO spaces?		{ $1 }
_BEhO_post :: [Indicators] = post_clause
_BEhO_no_SA_handling :: () = pre_clause _BEhO post_clause	{ () }

--	*** BIhE: prefix for high-priority MEX operator
_BIhE_clause :: Clause Unit = _BIhE_pre _BIhE_post	{ prePost () $1 $2 }
_BIhE_pre :: [BAhE] = pre_clause _BIhE spaces?		{ $1 }
_BIhE_post :: [Indicators] = post_clause
_BIhE_no_SA_handling :: () = pre_clause _BIhE post_clause	{ () }

---	*** BIhI: interval component of JOI ***
_BIhI_clause :: Clause Unit = _BIhI_pre _BIhI_post	{ prePost () $1 $2 }
_BIhI_pre :: [BAhE] = pre_clause _BIhI spaces?		{ $1 }
_BIhI_post :: [Indicators] = post_clause
_BIhI_no_SA_handling :: () = pre_clause _BIhI post_clause	{ () }

---	*** BO: joins two units with shortest scope ***
_BO_clause :: Clause Unit = _BO_pre _BO_post		{ prePost () $1 $2 }
_BO_pre :: [BAhE] = pre_clause _BO spaces?		{ $1 }
_BO_post :: [Indicators] = post_clause
_BO_no_SA_handling :: () = pre_clause _BO post_clause	{ () }

---	*** BOI: number or lerfu-string terminator ***
_BOI_clause :: Clause Unit = _BOI_pre _BOI_post		{ prePost () $1 $2 }
_BOI_pre :: [BAhE] = pre_clause _BOI spaces?		{ $1 }
_BOI_post :: [Indicators] = post_clause
_BOI_no_SA_handling :: () = pre_clause _BOI post_clause	{ () }

---	*** BU: terns any word into a BY lerfu word ***
_BU_clause :: Clause Unit = _BU_pre _BU_post		{ prePost () $1 [] }
_BU_clause_no_SA :: () = _BU_pre_no_SA _BU _BU_post	{ () }
_BU_pre :: [BAhE] = pre_clause _BU spaces?		{ $1 }
_BU_pre_no_SA :: () = pre_clause			{ () }
_BU_post :: () = spaces?				{ () }
_BU_no_SA_handling :: () = pre_clause _BU spaces?	{ () }

---	*** BY: individual lerfu words
_BY_clause :: Clause Lerfu
	= _BY_pre _BY_post			{ prePost (snd $1) (fst $1) $2 }
	/ bu_clause
_BY_pre :: ([BAhE], Lerfu) = pre_clause _BY spaces?	{ ($1, $2) }
_BY_post :: [Indicators] = post_clause
_BY_no_SA_handling :: ()
	= pre_clause _BY post_clause	{ () } / bu_clause_no_SA

---	*** CAhA: specifies actualitypotentiality of tense ***
_CAhA_clause :: Clause CAhA
	= _CAhA_pre _CAhA_post			{ prePost (snd $1) (fst $1) $2 }
_CAhA_pre :: ([BAhE], CAhA) = pre_clause _CAhA spaces?		{ ($1, $2) }
_CAhA_post :: [Indicators] = post_clause
_CAhA_no_SA_handling :: () = pre_clause _CAhA post_clause	{ () }

--	*** CAI: afterthought intensity marker ***
_CAI_clause :: Clause CAI = _CAI_pre _CAI_post	{ prePost (snd $1) (fst $1) [] }
_CAI_pre :: ([BAhE], CAI) = pre_clause _CAI spaces?		{ ($1, $2) }
_CAI_post :: () = post_clause_ind
_CAI_no_SA_handling :: () = pre_clause _CAI post_clause_ind	{ () }

--	*** CEI: pro-bridi assignment operator ***
_CEI_clause :: Clause Unit = _CEI_pre _CEI_post		{ prePost () $1 $2 }
_CEI_pre :: [BAhE] = pre_clause _CEI spaces?		{ $1 }
_CEI_post :: [Indicators] = post_clause
_CEI_no_SA_handling :: () = pre_clause _CEI post_clause	{ () }

--	*** CEhE: afterthought term list connective ***
_CEhE_clause :: Clause Unit = _CEhE_pre _CEhE_post	{ prePost () $1 $2 }
_CEhE_pre :: [BAhE] = pre_clause _CEhE spaces?		{ $1 }
_CEhE_post :: [Indicators] = post_clause
_CEhE_no_SA_handling :: () = pre_clause _CEhE post_clause	{ () }

---	*** CO: tanru inversion ***
_CO_clause :: Clause Unit = _CO_pre _CO_post		{ prePost () $1 $2 }
_CO_pre :: [BAhE] = pre_clause _CO spaces?		{ $1 }
_CO_post :: [Indicators] = post_clause
_CO_no_SA_handling :: () = pre_clause _CO post_clause	{ () }

---	*** COI: vocative marker permitted inside name ***
_COI_clause :: Clause COI = _COI_pre _COI_post	{ prePost (snd $1) (fst $1) $2 }
_COI_pre :: ([BAhE], COI) = pre_clause _COI spaces?	{ ($1, $2) }
_COI_post :: [Indicators] = post_clause
_COI_no_SA_handling :: () = pre_clause _COI post_clause	{ () }

---	*** CU: separator between head sumti and selbri ***
_CU_clause :: Clause Unit = _CU_pre _CU_post		{ prePost () $1 $2 }
_CU_pre :: [BAhE] = pre_clause _CU spaces?		{ $1 }
_CU_post :: [Indicators] = post_clause
_CU_no_SA_handling :: () = pre_clause _CU post_clause	{ () }

---	*** CUhE: tensemodal question ***
_CUhE_clause :: Clause CUhE
	= _CUhE_pre _CUhE_post			{ prePost (snd $1) (fst $1) $2 }
_CUhE_pre :: ([BAhE], CUhE) = pre_clause _CUhE spaces?		{ ($1, $2) }
_CUhE_post :: [Indicators] = post_clause
_CUhE_no_SA_handling :: () = pre_clause _CUhE post_clause	{ () }

--	*** DAhO: cancel anaphoracataphora assignments ***
_DAhO_clause :: Clause Unit = _DAhO_pre _DAhO_post	{ prePost () $1 $2 }
_DAhO_pre :: [BAhE] = pre_clause _DAhO spaces?		{ $1 }
_DAhO_post :: [Indicators] = post_clause
_DAhO_no_SA_handling :: () = pre_clause _DAhO post_clause	{ () }

--	*** DOI: vocative marker ***
_DOI_clause :: Clause Unit = _DAhO_pre _DAhO_post	{ prePost () $1 $2 }
_DOI_pre :: [BAhE] = pre_clause _DOI spaces?		{ $1 }
_DOI_post :: [Indicators] = post_clause
_DOI_no_SA_handling :: () = pre_clause _DOI post_clause	{ () }

--	*** DOhU: terminator for DOI_marked vocatives ***
_DOhU_clause :: Clause Unit = _DOhU_pre _DOhU_post	{ prePost () $1 $2 }
_DOhU_pre :: [BAhE] = pre_clause _DOhU spaces?		{ $1 }
_DOhU_post :: [Indicators] = post_clause
_DOhU_post_no_SA_handling :: () = pre_clause _DOhU post_clause	{ () }

--	*** FA: modifier head generic case tag ***
_FA_clause :: Clause FA = _FA_pre _FA_post	{ prePost (snd $1) (fst $1) $2 }
_FA_pre :: ([BAhE], FA) = pre_clause _FA spaces?	{ ($1, $2) }
_FA_post :: [Indicators] = post_clause
_FA_no_SA_handling :: () = pre_clause _FA post_clause	{ () }

--	*** FAhA: superdirections in space ***
_FAhA_clause :: Clause FAhA
	= _FAhA_pre _FAhA_post			{ prePost (snd $1) (fst $1) $2 }
_FAhA_pre :: ([BAhE], FAhA) = pre_clause _FAhA spaces?		{ ($1, $2) }
_FAhA_post :: [Indicators] = post_clause
_FAhA_no_SA_handling :: () = pre_clause _FAhA post_clause	{ () }

--	*** FAhO: normally elided 'done pause' to indicate end of utterance string ***
_FAhO_clause :: () = pre_clause _FAhO spaces?	{ () }

--	*** FEhE: space interval mod flag ***
_FEhE_clause :: Clause Unit = _FEhE_pre _FEhE_post	{ prePost () $1 $2 }
_FEhE_pre :: [BAhE] = pre_clause _FEhE spaces?		{ $1 }
_FEhE_post :: [Indicators] = post_clause
_FEhE_no_SA_handling :: () = pre_clause _FEhE post_clause	{ () }

--	*** FEhU: ends bridi to modal conversion ***
_FEhU_clause :: Clause Unit = _FEhU_pre _FEhU_post	{ prePost () $1 $2 }
_FEhU_pre :: [BAhE] = pre_clause _FEhU spaces?		{ $1 }
_FEhU_post :: [Indicators] = post_clause
_FEhU_no_SA_handling :: () = pre_clause _FEhU post_clause	{ () }

--	*** FIhO: marks bridi to modal conversion ***
_FIhO_clause :: Clause Unit = _FIhO_pre _FIhO_post	{ prePost () $1 $2 }
_FIhO_pre :: [BAhE] = pre_clause _FIhO spaces?		{ $1 }
_FIhO_post :: [Indicators] = post_clause
_FIhO_no_SA_handling :: () = pre_clause _FIhO post_clause	{ () }

--	*** FOI: end compound lerfu ***
_FOI_clause :: Clause Unit = _FOI_pre _FOI_post		{ prePost () $1 $2 }
_FOI_pre :: [BAhE] = pre_clause _FOI spaces?		{ $1 }
_FOI_post :: [Indicators] = post_clause
_FOI_no_SA_handling :: () = pre_clause _FOI post_clause	{ () }

--	*** FUhA: reverse Polish flag ***
_FUhA_clause :: Clause Unit = _FUhA_pre _FUhA_post	{ prePost () $1 $2 }
_FUhA_pre :: [BAhE] = pre_clause _FUhA spaces?		{ $1 }
_FUhA_post :: [Indicators] = post_clause
_FUhA_no_SA_handling :: () = pre_clause _FUhA post_clause	{ () }

--	*** FUhE: open long scope for indicator ***
_FUhE_clause :: Clause Unit = _FUhE_pre _FUhE_post	{ prePost () $1 [] }
_FUhE_pre :: [BAhE] = pre_clause _FUhE spaces?			{ $1 }
_FUhE_post :: () = !_BU_clause spaces? !_ZEI_clause !_BU_clause	{ () }
_FUhE_no_SA_handling :: () = pre_clause _FUhE post_clause	{ () }

--	*** FUhO: close long scope for indicator ***
_FUhO_clause :: Clause Unit = _FUhO_pre _FUhO_post	{ prePost () $1 [] }
_FUhO_pre :: [BAhE] = pre_clause _FUhO spaces?		{ $1 }
_FUhO_post :: () = post_clause_ind
_FUhO_no_SA_handling :: () = pre_clause _FUhO post_clause	{ () }

--	*** GA: geks; forethought logical connectives ***
_GA_clause :: Clause GA = _GA_pre _GA_post	{ prePost (snd $1) (fst $1) $2 }
_GA_pre :: ([BAhE], GA) = pre_clause _GA spaces?	{ ($1, $2) }
_GA_post :: [Indicators] = post_clause
_GA_no_SA_handling :: () = pre_clause _GA post_clause	{ () }

--	*** GAhO: openclosed interval markers for BIhI ***
_GAhO_clause :: Clause GAhO
	= _GAhO_pre _GAhO_post			{ prePost (snd $1) (fst $1) $2 }
_GAhO_pre :: ([BAhE], GAhO) = pre_clause _GAhO spaces?	{ ($1, $2) }
_GAhO_post :: [Indicators] = post_clause
_GAhO_no_SA_handling :: () = pre_clause _GAhO post_clause	{ () }

---	*** GEhU: marker ending GOI relative clauses ***
_GEhU_clause :: Clause Unit = _GEhU_pre _GEhU_post	{ prePost () $1 $2 }
_GEhU_pre :: [BAhE] = pre_clause _GEhU spaces?		{ $1 }
_GEhU_post :: [Indicators] = post_clause
_GEhU_no_SA_handling :: () = pre_clause _GEhU post_clause	{ () }

---	*** GI: forethought medial marker ***
_GI_clause :: Clause Unit = _GI_pre _GI_post		{ prePost () $1 $2 }
_GI_pre :: [BAhE] = pre_clause _GI spaces?		{ $1 }
_GI_post :: [Indicators] = post_clause
_GI_no_SA_handling :: () = pre_clause _GI post_clause	{ () }

--	*** GIhA: logical connective for bridi-tails ***
_GIhA_clause :: Clause GIhA
	= _GIhA_pre _GIhA_post			{ prePost (snd $1) (fst $1) $2 }
_GIhA_pre :: ([BAhE], GIhA) = pre_clause _GIhA spaces?		{ ($1, $2) }
_GIhA_post :: [Indicators] = post_clause
_GIhA_no_SA_handling :: () = pre_clause _GIhA post_clause	{ () }

--	*** GOI: attaches a sumti modifier to a sumti ***
_GOI_clause :: Clause GOI = _GOI_pre _GOI_post	{ prePost (snd $1) (fst $1) $2 }
_GOI_pre :: ([BAhE], GOI) = pre_clause _GOI spaces?	{ ($1, $2) }
_GOI_post :: [Indicators] = post_clause
_GOI_no_SA_handling :: () = pre_clause _GOI post_clause	{ () }

--	*** GOhA: pro-bridi ***
_GOhA_clause :: Clause GOhA
	= _GOhA_pre _GOhA_post			{ prePost (snd $1) (fst $1) $2 }
_GOhA_pre :: ([BAhE], GOhA) = pre_clause _GOhA spaces?		{ ($1, $2) }
_GOhA_post :: [Indicators] = post_clause
_GOhA_no_SA_handling :: () = pre_clause _GOhA post_clause	{ () }

--	*** GUhA: GEK for tanru units, corresponds to JEKs ***
_GUhA_clause :: Clause GUhA
	= _GUhA_pre _GUhA_post			{ prePost (snd $1) (fst $1) $2 }
_GUhA_pre :: ([BAhE], GUhA) = pre_clause _GUhA spaces?	{ ($1, $2) }
_GUhA_post :: [Indicators] = post_clause
_GUhA_no_SA_handling :: () = pre_clause _GUhA post_clause	{ () }

--	*** I: sentence link ***
_I_clause :: Clause Unit = sentence_sa* _I_pre _I_post	{ prePost () $2 $3 }
_I_pre :: [BAhE] = pre_clause _I spaces?		{ $1 }
_I_post :: [Indicators] = post_clause
_I_no_SA_handling :: () = pre_clause _I post_clause	{ () }

--	*** JA: jeks; logical connectives within tanru ***
_JA_clause :: Clause JA = _JA_pre _JA_post	{ prePost (snd $1) (fst $1) $2 }
_JA_pre :: ([BAhE], JA) = pre_clause _JA spaces?	{ ($1, $2) }
_JA_post :: [Indicators] = post_clause
_JA_no_SA_handling :: () = pre_clause _JA post_clause	{ () }

--	*** JAI: modal conversion flag ***
_JAI_clause :: Clause Unit = _JAI_pre _JAI_post		{ prePost () $1 $2 }
_JAI_pre :: [BAhE] = pre_clause _JAI spaces?		{ $1 }
_JAI_post :: [Indicators] = post_clause
_JAI_no_SA_handling :: () = pre_clause _JAI post_clause	{ () }

--	*** JOhI: flags an array operand ***
_JOhI_clause :: Clause Unit = _JOhI_pre _JOhI_post	{ prePost () $1 $2 }
_JOhI_pre :: [BAhE] = pre_clause _JOhI spaces?		{ $1 }
_JOhI_post :: [Indicators] = post_clause
_JOhI_no_SA_handling :: () = pre_clause _JOhI post_clause	{ () }

--	*** JOI: non-logical connectives ***
_JOI_clause :: Clause JOI = _JOI_pre _JOI_post	{ prePost (snd $1) (fst $1) $2 }
_JOI_pre :: ([BAhE], JOI) = pre_clause _JOI spaces?	{ ($1, $2) }
_JOI_post :: [Indicators] = post_clause
_JOI_no_SA_handling :: () = pre_clause _JOI post_clause	{ () }

--	*** KE: left long scope marker ***
_KE_clause :: Clause Unit = _KE_pre _KE_post		{ prePost () $1 $2 }
_KE_pre :: [BAhE] = pre_clause _KE spaces?		{ $1 }
_KE_post :: [Indicators] = post_clause
_KE_no_SA_handling :: () = pre_clause _KE post_clause	{ () }

--	*** KEhE: right terminator for KE groups ***
_KEhE_clause :: Clause Unit = _KEhE_pre _KEhE_post	{ prePost () $1 $2 }
_KEhE_pre :: [BAhE] = pre_clause _KEhE spaces?		{ $1 }
_KEhE_post :: [Indicators] = post_clause
_KEhE_no_SA_handling :: () = pre_clause _KEhE post_clause	{ () }

--	*** KEI: right terminator, NU abstractions ***
_KEI_clause :: Clause Unit = _KEI_pre _KEI_post		{ prePost () $1 $2 }
_KEI_pre :: [BAhE] = pre_clause _KEI spaces?		{ $1 }
_KEI_post :: [Indicators] = post_clause
_KEI_no_SA_handling :: () = pre_clause _KEI post_clause	{ () }

--	*** KI: multiple utterance scope for tenses ***
_KI_clause :: Clause Unit = _KI_pre _KI_post		{ prePost () $1 $2 }
_KI_pre :: [BAhE] = pre_clause _KI spaces?		{ $1 }
_KI_post :: [Indicators] = post_clause
_KI_no_SA_handling :: () = pre_clause _KI post_clause	{ () }

--	*** KOhA: sumti anaphora ***
_KOhA_clause :: Clause KOhA
	= _KOhA_pre _KOhA_post			{ prePost (snd $1) (fst $1) $2 }
_KOhA_pre :: ([BAhE], KOhA) = pre_clause _KOhA spaces?	{ ($1, $2) }
_KOhA_post :: [Indicators] = post_clause
_KOhA_no_SA_handling :: () = pre_clause _KOhA spaces?	{ () }

--	*** KU: right terminator for descritions, etc. ***
_KU_clause :: Clause Unit = _KU_pre _KU_post		{ prePost () $1 $2 }
_KU_pre :: [BAhE] = pre_clause _KU spaces?		{ $1 }
_KU_post :: [Indicators] = post_clause
_KU_no_SA_handling :: () = pre_clause _KU post_clause	{ () }

--	*** KUhE: MEX forethought delimiter ***
_KUhE_clause :: Clause Unit = _KUhE_pre _KUhE_post	{ prePost () $1 $2 }
_KUhE_pre :: [BAhE] = pre_clause _KUhE spaces?		{ $1 }
_KUhE_post :: [Indicators] = post_clause
_KUhE_no_SA_handling :: () = pre_clause _KUhE post_clause	{ () }

--	*** KUhO: right terminator, NOI relative clauses ***
_KUhO_clause :: Clause Unit = _KUhO_pre _KUhO_post	{ prePost () $1 $2 }
_KUhO_pre :: [BAhE] = pre_clause _KUhO spaces?		{ $1 }
_KUhO_post :: [Indicators] = post_clause
_KUhO_no_SA_handling :: () = pre_clause _KUhO post_clause	{ () }

--	*** LA: name descriptors ***
_LA_clause :: Clause LA = _LA_pre _LA_post	{ prePost (snd $1) (fst $1) $2 }
_LA_pre :: ([BAhE], LA) = pre_clause _LA spaces?{ ($1, $2) }
_LA_post :: [Indicators] = post_clause
_LA_no_SA_handling :: () = pre_clause _LA post_clause		{ () }

--	*** LAU: lerfu prefixes ***
_LAU_clause :: Clause LAU = _LAU_pre _LAU_post	{ prePost (snd $1) (fst $1) $2 }
_LAU_pre :: ([BAhE], LAU) = pre_clause _LAU spaces?		{ ($1, $2) }
_LAU_post :: [Indicators] = post_clause
_LAU_no_SA_handling :: () = pre_clause _LAU post_clause		{ () }

--	*** LAhE: sumti qualifiers ***
_LAhE_clause :: Clause LAhE
	= _LAhE_pre _LAhE_post			{ prePost (snd $1) (fst $1) $2 }
_LAhE_pre :: ([BAhE], LAhE) = pre_clause _LAhE spaces?		{ ($1, $2) }
_LAhE_post :: [Indicators] = post_clause
_LAhE_no_SA_handling :: () = pre_clause _LAhE post_clause	{ () }

--	*** LE: sumti descriptors ***
_LE_clause :: Clause LE = _LE_pre _LE_post	{ prePost (snd $1) (fst $1) $2 }
_LE_pre :: ([BAhE], LE) = pre_clause _LE spaces?	{ ($1, $2) }
_LE_post :: [Indicators] = post_clause
_LE_no_SA_handling :: () = pre_clause _LE post_clause	{ () }

--	*** LEhU: posibbly ungrammatical text right quote ***
_LEhU_clause :: Clause Unit = _LEhU_pre _LEhU_post	{ prePost () $1 [] }
_LEhU_pre :: [BAhE] = pre_clause _LEhU spaces?		{ $1 }
_LEhU_post :: () = spaces?				{ () }
_LEhU_clause_no_SA :: () = _LEhU_pre_no_SA _LEhU_post	{ () }
_LEhU_pre_no_SA :: () = pre_clause _LEhU spaces?	{ () }
_LEhU_no_SA_handling :: () = pre_clause _LEhU post_clause{ () }

--	*** LI: convert number to sumti ***
_LI_clause :: Clause Unit = _LI_pre _LI_post	{ prePost () $1 $2 }
_LI_pre :: [BAhE] = pre_clause _LI spaces?		{ $1 }
_LI_post :: [Indicators] = post_clause
_LI_no_SA_handling :: () = pre_clause _LI post_clause	{ () }

--	*** LIhU: grammatical text right quote ***
_LIhU_clause :: Clause Unit = _LIhU_pre _LIhU_post	{ prePost () $1 $2 }
_LIhU_pre :: [BAhE] = pre_clause _LIhU spaces?		{ $1 }
_LIhU_post :: [Indicators] = post_clause
_LIhU_no_SA_handling :: () = pre_clause _LIhU post_clause	{ () }

--	*** LOhO: elidable terminator for LI ***
_LOhO_clause :: Clause Unit = _LOhO_pre _LOhO_post	{ prePost () $1 $2 }
_LOhO_pre :: [BAhE] = pre_clause _LOhO spaces?		{ $1 }
_LOhO_post :: [Indicators] = post_clause
_LOhO_no_SA_handling :: () = pre_clause _LOhO post_clause	{ () }

--	*** LOhU: possibly ungrammatical text left quote ***
_LOhU_clause :: Clause Unit = _LOhU_pre _LOhU_post	{ prePost () $1 $2 }
_LOhU_pre :: [BAhE] = pre_clause _LOhU spaces?		{ $1 }
_LOhU_post :: [Indicators] = post_clause
_LOhU_no_SA_handling :: () = pre_clause _LOhU post_clause	{ () }

--	*** LU: grammatical text left quote ***
_LU_clause :: Clause Unit = _LU_pre _LU_post		{ prePost () $1 $2 }
_LU_pre :: [BAhE] = pre_clause _LU spaces?		{ $1 }
_LU_post :: [Indicators] = post_clause
_LU_no_SA_handling :: () = pre_clause _LU post_clause	{ () }

--	*** LUhU: LAhE close delimiter ***
_LUhU_clause :: Clause Unit = _LUhU_pre _LUhU_post	{ prePost () $1 $2 }
_LUhU_pre :: [BAhE] = pre_clause _LUhU spaces?		{ $1 }
_LUhU_post :: [Indicators] = post_clause
_LUhU_no_SA_handling :: () = pre_clause _LUhU post_clause	{ () }

--	*** MAhO: change MEX expressions to MEX operators ***
_MAhO_clause :: Clause Unit = _MAhO_pre _MAhO_post	{ prePost () $1 $2 }
_MAhO_pre :: [BAhE] = pre_clause _MAhO spaces?		{ $1 }
_MAhO_post :: [Indicators] = post_clause
_MAhO_no_SA_handling :: () = pre_clause _MAhO post_clause	{ () }

--	*** MAI: change numbers to utterance ordinals ***
_MAI_clause :: Clause MAI = _MAI_pre _MAI_post	{ prePost (snd $1) (fst $1) $2 }
_MAI_pre :: ([BAhE], MAI) = pre_clause _MAI spaces?	{ ($1, $2) }
_MAI_post :: [Indicators] = post_clause
_MAI_no_SA_handling :: () = pre_clause _MAI post_clause	{ () }

--	*** ME: change numbers to utterance ordinals ***
_ME_clause :: Clause Unit = _ME_pre _ME_post		{ prePost () $1 $2 }
_ME_pre :: [BAhE] = pre_clause _ME spaces?		{ $1 }
_ME_post :: [Indicators] = post_clause
_ME_no_SA_handling :: () = pre_clause _ME post_clause	{ () }

--	*** MEhU: terminator for ME ***
_MEhU_clause :: Clause Unit = _MEhU_pre _MEhU_post	{ prePost () $1 $2 }
_MEhU_pre :: [BAhE] = pre_clause _MEhU spaces?		{ $1 }
_MEhU_post :: [Indicators] = post_clause
_MEhU_no_SA_handling :: () = pre_clause _MEhU post_clause	{ () }

--	*** MOhE: change sumti to operand, inverse of LI ***
_MOhE_clause :: Clause Unit = _MOhE_pre _MOhE_post	{ prePost () $1 $2 }
_MOhE_pre :: [BAhE] = pre_clause _MOhE spaces?		{ $1 }
_MOhE_post :: [Indicators] = post_clause
_MOhE_no_SA_handling :: () = pre_clause _MOhE post_clause	{ () }

--	*** MOhI: motion tense marker ***
_MOhI_clause :: Clause Unit = _MOhI_pre _MOhI_post	{ prePost () $1 $2 }
_MOhI_pre :: [BAhE] = pre_clause _MOhI spaces?		{ $1 }
_MOhI_post :: [Indicators] = post_clause
_MOhI_no_SA_handling :: () = pre_clause _MOhI post_clause	{ () }

--	*** MOI: change number to selbri ***
_MOI_clause :: Clause MOI = _MOI_pre _MOI_post	{ prePost (snd $1) (fst $1) $2 }
_MOI_pre :: ([BAhE], MOI) = pre_clause _MOI spaces?	{ ($1, $2) }
_MOI_post :: [Indicators] = post_clause
_MOI_no_SA_handling :: () = pre_clause _MOI post_clause	{ () }

--	*** NA: bridi negation ***
_NA_clause :: Clause NA = _NA_pre _NA_post	{ prePost (snd $1) (fst $1) $2 }
_NA_pre :: ([BAhE], NA) = pre_clause _NA spaces?	{ ($1, $2) }
_NA_post :: [Indicators] = post_clause
_NA_no_SA_handling :: () = pre_clause _NA post_clause	{ () }

--	*** NAI: attached to words to negate them ***
_NAI_clause :: Clause Unit = _NAI_pre _NAI_post		{ prePost () $1 [] }
_NAI_pre :: [BAhE] = pre_clause _NAI spaces?			{ $1 }
_NAI_post :: () = post_clause_ind
_NAI_no_SA_handling :: () = pre_clause _NAI post_clause_ind	{ () }

--	*** NAhE: scalar negation ***
_NAhE_clause :: Clause NAhE
	= _NAhE_pre _NAhE_post			{ prePost (snd $1) (fst $1) $2 }
_NAhE_pre :: ([BAhE], NAhE) = pre_clause _NAhE spaces?		{ ($1, $2) }
_NAhE_post :: [Indicators] = post_clause
_NAhE_no_SA_handling :: () = pre_clause _NAhE post_clause	{ () }

--	*** NAhU: change a selbri into an operator ***
_NAhU_clause :: Clause Unit = _NAhU_pre _NAhU_post	{ prePost () $1 $2 }
_NAhU_pre :: [BAhE] = pre_clause _NAhU spaces?		{ $1 }
_NAhU_post :: [Indicators] = post_clause
_NAhU_no_SA_handling :: () = pre_clause _NAhU post_clause	{ () }

--	*** NIhE: change selbri to operand; inverse of MOI ***
_NIhE_clause :: Clause Unit = _NIhE_pre _NIhE_post	{ prePost () $1 $2 }
_NIhE_pre :: [BAhE] = pre_clause _NIhE spaces?		{ $1 }
_NIhE_post :: [Indicators] = post_clause
_NIhE_no_SA_handling :: () = pre_clause _NIhE post_clause	{ () }

--	*** NIhO: new paragraph; change of subject ***
_NIhO_clause ::Clause NIhO
	= sentence_sa* _NIhO_pre _NIhO_post	{ prePost (snd $2) (fst $2) $3 }
_NIhO_pre :: ([BAhE], NIhO) = pre_clause _NIhO spaces?		{ ($1, $2) }
_NIhO_post :: [Indicators] = post_clause
_NIhO_no_SA_handling :: () = pre_clause _NIhO post_clause	{ () }

--	*** NOI: attaches a subordinate clause to a sumti ***
_NOI_clause :: Clause NOI = _NOI_pre _NOI_post	{ prePost (snd $1) (fst $1) $2 }
_NOI_pre :: ([BAhE], NOI) = pre_clause _NOI spaces?	{ ($1, $2) }
_NOI_post :: [Indicators] = post_clause
_NOI_no_SA_handling :: () = pre_clause _NOI post_clause	{ () }

--	*** NU: abstraction ***
_NU_clause :: Clause NU = _NU_pre _NU_post	{ prePost (snd $1) (fst $1) $2 }
_NU_pre :: ([BAhE], NU) = pre_clause _NU spaces?	{ ($1, $2) }
_NU_post :: [Indicators] = post_clause
_NU_no_SA_handling :: () = pre_clause _NU post_clause	{ () }

--	*** NUhA: change operator to selbri; inverse of MOhE ***
_NUhA_clause :: Clause Unit = _NUhA_pre _NUhA_post	{ prePost () $1 $2 }
_NUhA_pre :: [BAhE] = pre_clause _NUhA spaces?		{ $1 }
_NUhA_post :: [Indicators] = post_clause
_NUhA_no_SA_handling :: () = pre_clause _NUhA post_clause	{ () }

--	*** NUhI: marks the start of a termset ***
_NUhI_clause :: Clause Unit = _NUhI_pre _NUhI_post	{ prePost () $1 $2 }
_NUhI_pre :: [BAhE] = pre_clause _NUhI spaces?		{ $1 }
_NUhI_post :: [Indicators] = post_clause
_NUhI_no_SA_handling :: () = pre_clause _NUhI post_clause	{ () }

--	*** NUhU: marks the middle and end of a termset ***
_NUhU_clause :: Clause Unit = _NUhU_pre _NUhU_post	{ prePost () $1 $2 }
_NUhU_pre :: [BAhE] = pre_clause _NUhU spaces?			{ $1 }
_NUhU_post :: [Indicators] = post_clause
_NUhU_no_SA_handling :: () = pre_clause _NUhU post_clause	{ () }

--	*** PA: numbers and numeric punctuation ***
_PA_clause :: Clause PA = _PA_pre _PA_post	{ prePost (snd $1) (fst $1) $2 }
_PA_pre :: ([BAhE], PA) = pre_clause _PA spaces?{ ($1, $2) }
_PA_post :: [Indicators] = post_clause
_PA_no_SA_handling :: () = pre_clause _PA post_clause	{ () }

--	*** PEhE: afterthought termset connective prefix ***
_PEhE_clause :: Clause Unit = _PEhE_pre _PEhE_post	{ prePost () $1 $2 }
_PEhE_pre :: [BAhE] = pre_clause _PEhE spaces?		{ $1 }
_PEhE_post :: [Indicators] = post_clause
_PEhE_no_SA_handling :: () = pre_clause _PA post_clause	{ () }

--	*** PEhO: forethought (Polish) flag ***
_PEhU_clause :: Clause Unit = _PEhO_pre _PEhO_post	{ prePost () $1 $2 }
_PEhO_pre :: [BAhE] = pre_clause _PEhO spaces?		{ $1 }
_PEhO_post :: [Indicators] = post_clause
_PEhO_no_SA_handling :: () = pre_clause _PEhO post_clause	{ () }

--	*** PU: directions in time ***
_PU_clause :: Clause PU = _PU_pre _PU_post	{ prePost (snd $1) (fst $1) $2 }
_PU_pre :: ([BAhE], PU) = pre_clause _PU spaces?	{ ($1, $2) }
_PU_post :: [Indicators] = post_clause
_PU_no_SA_handling :: () = pre_clause _PU post_clause	{ () }

--	*** RAhO: flag for modified interpretation of GOhI ***
_RAhO_clause :: Clause Unit = _RAhO_pre _RAhO_post	{ prePost () $1 $2 }
_RAhO_pre :: [BAhE] = pre_clause _RAhO spaces?			{ $1 }
_RAhO_post :: [Indicators] = post_clause
_RAhO_no_SA_handling :: () = pre_clause _RAhO post_clause	{ () }

--	*** ROI: converts number to extensinal tense ***
_ROI_clause :: Clause ROI = _ROI_pre _ROI_post	{ prePost (snd $1) (fst $1) $2 }
_ROI_pre :: ([BAhE], ROI) = pre_clause _ROI spaces?	{ ($1, $2) }
_ROI_post :: [Indicators] = post_clause
_ROI_no_SA_handling :: () = pre_clause _ROI post_clause	{ () }

--	*** SA: metalinguistic eraser to the beginning of the current utterance ***
_SA_clause :: Clause Unit = _SA_pre _SA_post	{ prePost () $1 [] }
_SA_pre :: [BAhE] = pre_clause _SA spaces?	{ $1 }
_SA_post :: () = spaces?			{ () }

--	*** SE: conversions ***
_SE_clause :: Clause SE = _SE_pre _SE_post	{ prePost (snd $1) (fst $1) $2 }
_SE_pre :: ([BAhE], SE) = pre_clause _SE spaces?	{ ($1, $2) }
_SE_post :: [Indicators] = post_clause
_SE_no_SA_handling :: () = pre_clause _SE post_clause	{ () }

--	*** SEI: metalinguistic bridi insert marker ***
_SEI_clause :: Clause SEI = _SEI_pre _SEI_post	{ prePost (snd $1) (fst $1) $2 }
_SEI_pre :: ([BAhE], SEI) = pre_clause _SEI spaces?	{ ($1, $2) }
_SEI_post :: [Indicators] = post_clause
_SEI_no_SA_handling :: () = pre_clause _SEI post_clause	{ () }

--	*** SEhU: metalinguistic bridi end marker ***
_SEhU_clause :: Clause Unit = _SEhU_pre _SEhU_post	{ prePost () $1 $2 }
_SEhU_pre :: [BAhE] = pre_clause _SEhU spaces?		{ $1 }
_SEhU_post :: [Indicators] = post_clause
_SEhU_no_SA_handling :: () = pre_clause _SEhU post_clause	{ () }

--	*** SI: metalinguistic single word eraser ***
_SI_clause :: () = spaces? _SI spaces?		{ () }

--	*** SOI: reciprocal sumti marker ***
_SOI_clause :: Clause Unit = _SOI_pre _SOI_post		{ prePost () $1 $2 }
_SOI_pre :: [BAhE] = pre_clause _SOI spaces?		{ $1 }
_SOI_post :: [Indicators] = post_clause
_SOI_no_SA_handling :: () = pre_clause _SOI post_clause	{ () }

--	*** SU: metalinguistic eraser of the entire text ***
_SU_clause :: Clause Unit = _SU_pre _SU_post	{ prePost () $1 $2 }
_SU_pre :: [BAhE] = pre_clause _SU spaces?	{ $1 }
_SU_post :: [Indicators] = post_clause

--	*** TAhE: tense interval properties ***
_TAhE_clause :: Clause TAhE
	= _TAhE_pre _TAhE_post			{ prePost (snd $1) (fst $1) $2 }
_TAhE_pre :: ([BAhE], TAhE) = pre_clause _TAhE spaces?		{ ($1, $2) }
_TAhE_post :: [Indicators] = post_clause
_TAhE_no_SA_handling :: () = pre_clause _TAhE post_clause	{ () }

--	*** TEhU: closing gap for MEX constructs ***
_TEhU_clause :: Clause Unit = _TEhU_pre _TEhU_post	{ prePost () $1 $2 }
_TEhU_pre :: [BAhE] = pre_clause _TEhU spaces?		{ $1 }
_TEhU_post :: [Indicators] = post_clause
_TEhU_no_SA_handling :: () = pre_clause _TEhU post_clause	{ () }

--	*** TEI: start compound lerfu ***
_TEI_clause :: Clause Unit = _TEI_pre _TEI_post		{ prePost () $1 $2 }
_TEI_pre :: [BAhE] = pre_clause _TEI spaces?		{ $1 }
_TEI_post :: [Indicators] = post_clause
_TEI_no_SA_handling :: () = pre_clause _TEI post_clause	{ () }

--	*** TO: left discursive parenthesis ***
_TO_clause :: Clause TO = _TO_pre _TO_post	{ prePost (snd $1) (fst $1) $2 }
_TO_pre :: ([BAhE], TO) = pre_clause _TO spaces?{ ($1, $2) }
_TO_post :: [Indicators] = post_clause
_TO_no_SA_handling :: () = pre_clause _TO post_clause	{ () }

--	*** TOI: right discursive parenthesis ***
_TOI_clause :: Clause Unit = _TOI_pre _TOI_post		{ prePost () $1 $2 }
_TOI_pre :: [BAhE] = pre_clause _TOI spaces?		{ $1 }
_TOI_post :: [Indicators] = post_clause
_TOI_no_SA_handling :: () = pre_clause _TOI post_clause	{ () }

--	*** TUhE: multiple utterance scope mark ***
_TUhE_clause :: Clause Unit = _TUhE_pre _TUhE_post	{ prePost () $1 $2 }
_TUhE_pre :: [BAhE] = pre_clause _TUhE spaces?		{ $1 }
_TUhE_post :: [Indicators] = post_clause
_TUhE_no_SA_handling :: () = pre_clause _TUhE post_clause	{ () }

--	*** TUhU: multiple utterance end scope mark ***
_TUhU_clause :: Clause Unit = _TUhU_pre _TUhU_post	{ prePost () $1 $2 }
_TUhU_pre :: [BAhE] = pre_clause _TUhU spaces?		{ $1 }
_TUhU_post :: [Indicators] = post_clause
_TUhU_no_SA_handling :: () = pre_clause _TUhU post_clause	{ () }

--	*** UI: attitudinals, observationals, discursives ***
_UI_clause :: Clause UI = _UI_pre _UI_post	{ prePost (snd $1) (fst $1) [] }
_UI_pre :: ([BAhE], UI) = pre_clause _UI spaces?		{ ($1, $2) }
_UI_post :: () = post_clause_ind
_UI_no_SA_handling :: () = pre_clause _UI post_clause_ind	{ () }

--	*** VA: distance in space-time ***
_VA_clause :: Clause VA = _VA_pre _VA_post	{ prePost (snd $1) (fst $1) $2 }
_VA_pre :: ([BAhE], VA) = pre_clause _VA spaces?	{ ($1, $2) }
_VA_post :: [Indicators] = post_clause
_VA_no_SA_handling :: () = pre_clause _VA post_clause	{ () }

--	*** VAU: end simple bridi or bridi-tail ***
_VAU_clause :: Clause Unit = _VAU_pre _VAU_post		{ prePost () $1 $2 }
_VAU_pre :: [BAhE] = pre_clause _VAU spaces?		{ $1 }
_VAU_post :: [Indicators] = post_clause
_VAU_no_SA_handling :: () = pre_clause _VAU post_clause	{ () }

--	*** VEI: left MEX bracket ***
_VEI_clause :: Clause Unit = _VEI_pre _VEI_post		{ prePost () $1 $2 }
_VEI_pre :: [BAhE] = pre_clause _VEI spaces?		{ $1 }
_VEI_post :: [Indicators] = post_clause
_VEI_no_SA_handling :: () = pre_clause _VEI post_clause	{ () }

--	*** VEhO: right MEX bracket ***
_VEhO_clause :: Clause Unit = _VEhO_pre _VEhO_post	{ prePost () $1 $2 }
_VEhO_pre :: [BAhE] = pre_clause _VEhO spaces?		{ $1 }
_VEhO_post :: [Indicators] = post_clause
_VEhO_no_SA_handling :: () = pre_clause _VEhO post_clause	{ () }

--	*** VUhU: MEX operator
_VUhU_clause :: Clause VUhU
	= _VUhU_pre _VUhU_post			{ prePost (snd $1) (fst $1) $2 }
_VUhU_pre :: ([BAhE], VUhU) = pre_clause _VUhU spaces?		{ ($1, $2) }
_VUhU_post :: [Indicators] = post_clause
_VUhU_no_SA_handling :: () = pre_clause _VUhU post_clause	{ () }

--	*** VEhA: space-time interval size
_VEhA_clause :: Clause VEhA
	= _VEhA_pre _VEhA_post			{ prePost (snd $1) (fst $1) $2 }
_VEhA_pre :: ([BAhE], VEhA) = pre_clause _VEhA spaces?		{ ($1, $2) }
_VEhA_post :: [Indicators] = post_clause
_VEhA_no_SA_handling :: () = pre_clause _VEhA post_clause	{ () }

--	*** VIhA: space-time dimensionality marker
_VIhA_clause :: Clause VIhA
	= _VIhA_pre _VIhA_post			{ prePost (snd $1) (fst $1) $2 }
_VIhA_pre :: ([BAhE], VIhA) = pre_clause _VIhA spaces?		{ ($1, $2) }
_VIhA_post :: [Indicators] = post_clause
_VIhA_no_SA_handling :: () = pre_clause _VIhA post_clause	{ () }

--	*** VUhO: glue between logically connected sumti and relative clauses
_VUhO_clause :: Clause Unit = _VUhO_pre _VUhO_post	{ prePost () $1 $2 }
_VUhO_pre :: [BAhE] = pre_clause _VUhO spaces?		{ $1 }
_VUhO_post :: [Indicators] = post_clause
_VUhO_no_SA_handling :: () = pre_clause _VUhO post_clause	{ () }

--	*** XI: subscripting operator
_XI_clause :: Clause Unit = _XI_pre _XI_post		{ prePost () $1 $2 }
_XI_pre :: [BAhE] = pre_clause _XI spaces?		{ $1 }
_XI_post :: [Indicators] = post_clause
_XI_no_SA_handling :: () = pre_clause _XI post_clause	{ () }

--	*** Y: hesitation
-- Very very special case. Handled in the morphology section.
-- _Y_clause :: () = spaces? _Y spaces?	{ () }

--	*** ZAhO: event properties - inchoative, etc. ***
_ZAhO_clause :: Clause ZAhO
	= _ZAhO_pre _ZAhO_post			{ prePost (snd $1) (fst $1) $2 }
_ZAhO_pre :: ([BAhE], ZAhO) = pre_clause _ZAhO spaces?		{ ($1, $2) }
_ZAhO_post :: [Indicators] = post_clause
_ZAhO_no_SA_handling :: () = pre_clause _ZAhO post_clause	{ () }

--	*** ZEhA: time interval size tense ***
_ZEhA_clause :: Clause ZEhA
	= _ZEhA_pre _ZEhA_post			{ prePost (snd $1) (fst $1) $2 }
_ZEhA_pre :: ([BAhE], ZEhA) = pre_clause _ZEhA spaces?		{ ($1, $2) }
_ZEhA_post :: [Indicators] = post_clause
_ZEhA_no_SA_handling :: () = pre_clause _ZEhA post_clause	{ () }

--	*** ZEI: lujvo glue ***
_ZEI_clause :: Clause Unit = _ZEI_pre _ZEI_post		{ prePost () $1 [] }
_ZEI_clause_no_SA :: () = _ZEI_pre_no_SA _ZEI _ZEI_post	{ () }
_ZEI_pre :: [BAhE] = pre_clause _ZEI spaces?		{ $1 }
_ZEI_pre_no_SA :: () = pre_clause			{ () }
_ZEI_post :: () = spaces?				{ () }
_ZEI_no_SA_handling :: () = pre_clause _ZEI post_clause	{ () }

--	*** ZI: time distance tense ***
_ZI_clause :: Clause ZI = _ZI_pre _ZI_post	{ prePost (snd $1) (fst $1) $2 }
_ZI_pre :: ([BAhE], ZI) = pre_clause _ZI spaces?{ ($1, $2) }
_ZI_post :: [Indicators] = post_clause
_ZI_no_SA_handling :: () = pre_clause _ZI post_clause	{ () }

--	*** ZIhE: conjoins relative clauses ***
_ZIhE_clause :: Clause Unit = _ZIhE_pre _ZIhE_post	{ prePost () $1 $2 }
_ZIhE_pre :: [BAhE] = pre_clause _ZIhE spaces?		{ $1 }
_ZIhE_post :: [Indicators] = post_clause
_ZIhE_no_SA_handling :: () = pre_clause _ZIhE post_clause	{ () }

--	*** ZO: single word metalinguistic quote marker ***
_ZO_clause :: Clause Quote = _ZO_pre _ZO_post	{ prePost (snd $1) (fst $1) $2 }
_ZO_pre :: ([BAhE], Quote) = pre_clause _ZO spaces? any_word spaces?
						{ ($1, SingleWordQuote $4) }
_ZO_post :: [Indicators] = post_clause
_ZO_no_SA_handling :: () = pre_clause _ZO spaces? any_word spaces?	{ () }

--	*** ZOI: delimited quote marker ***

_ZOI_clause :: Clause Quote = _ZOI_pre _ZOI_post
					{ prePost (snd $1) (fst $ fst $1) $2 }
_ZOI_pre :: (([BAhE], ZOI), Quote)
	= pre_clause _ZOI spaces? zoi_open zoi_word* zoi_close spaces?
					{ (($1, $2), DelimitedQuote $2 $5) }
_ZOI_post :: [Indicators] = post_clause
_ZOI_no_SA_handling :: ()
	= pre_clause _ZOI spaces? zoi_open zoi_word* zoi_close spaces?	{ () }
zoi_open :: () = "\x00"
zoi_close :: () = "\x00"
zoi_word :: Char = !"\x00" .

--	*** ZOhU: prenex terminator (not elidable) ***

_ZOhU_clause :: Clause Unit = _ZOhU_pre _ZOhU_post	{ prePost () $1 $2 }
_ZOhU_pre :: [BAhE] = pre_clause _ZOhU spaces?		{ $1 }
_ZOhU_post :: [Indicators] = post_clause
_ZOhU_no_SA_handling :: () = pre_clause _ZOhU post_clause	{ () }

--* MORPHOLOGY ************************************************************ 1334

_CMENE :: CMENE = cmene						{ CMENE $1 }
_BRIVLA :: BRIVLA = (gismu / lujvo / fuhivla)			{ BRIVLA $1 }
_CMAVO :: Word
	= _A   	{ WA    $1 } / _BAI  { WBAI  $1 } / _BAhE { WBAhE $1 }
	/ _BE   { WBE      } / _BEI  { WBEI     } / _BEhO { WBEhO    }
	/ _BIhE { WBIhE    } / _BIhI { WBIhI    } / _BO   { WBO      }
	/ _BOI  { WBOI     } / _BU   { WBU      } / _BY   { WBY   $1 }
	/ _CAhA { WCAhA $1 } / _CAI  { WCAI  $1 } / _CEI  { WCEI     }
	/ _CEhE { WCEhE    } / _CO   { WCO      } / _COI  { WCOI  $1 }
	/ _CU   { WCU      } / _CUhE { WCUhE $1 } / _DAhO { WDAhO    }
	/ _DOI  { WDOI     } / _DOhU { WDOhU    } / _FA   { WFA   $1 }
	/ _FAhA { WFAhA $1 } / _FAhO { WFAhO    } / _FEhE { WFEhE    }
	/ _FEhU { WFEhU    } / _FIhO { WFIhO    } / _FOI  { WFOI     }
	/ _FUhA { WFUhA    } / _FUhE { WFUhE    } / _FUhO { WFUhO    }
	/ _GA   { WGA   $1 } / _GAhO { WGAhO $1 } / _GEhU { WGEhU    }
	/ _GI   { WGI      } / _GIhA { WGIhA $1 } / _GOI  { WGOI  $1 }
	/ _GOhA { WGOhA $1 } / _GUhA { WGUhA $1 } / _I    { WI       }
	/ _JA   { WJA   $1 } / _JAI  { WJAI     } / _JOhI { WJOhI    }
	/ _JOI  { WJOI  $1 } / _KE   { WKE      } / _KEhE { WKEhE    }
	/ _KEI  { WKEI     } / _KI   { WKI      } / _KOhA { WKOhA $1 }
	/ _KU   { WKU      } / _KUhE { WKUhE    } / _KUhO { WKUhO    }
	/ _LA   { WLA   $1 } / _LAU  { WLAU  $1 } / _LAhE { WLAhE $1 }
	/ _LE   { WLE   $1 } / _LEhU { WLEhU    } / _LI   { WLI      }
	/ _LIhU { WLIhU    } / _LOhO { WLOhO    } / _LOhU { WLOhU    }
	/ _LU   { WLU      } / _LUhU { WLUhU    } / _MAhO { WMAhO    }
	/ _MAI  { WMAI  $1 } / _ME   { WME      } / _MEhU { WMEhU    }
	/ _MOhE { WMOhE    } / _MOhI { WMOhI    } / _MOI  { WMOI  $1 }
	/ _NA   { WNA   $1 } / _NAI  { WNAI     } / _NAhE { WNAhE $1 }
	/ _NAhU { WNAhU    } / _NIhE { WNIhE    } / _NIhO { WNIhO $1 }
	/ _NOI  { WNOI  $1 } / _NU   { WNU   $1 } / _NUhA { WNUhA    }
	/ _NUhI { WNUhI    } / _NUhU { WNUhU    } / _PA   { WPA   $1 }
	/ _PEhE { WPEhE    } / _PEhO { WPEhO    } / _PU   { WPU   $1 }
	/ _RAhO { WRAhO    } / _ROI  { WROI  $1 } / _SA   { WSA      }
	/ _SE   { WSE   $1 } / _SEI  { WSEI  $1 } / _SEhU { WSEhU    }
	/ _SI   { WSI      } / _SOI  { WSOI     } / _SU   { WSU      }
	/ _TAhE { WTAhE $1 } / _TEhU { WTEhU    } / _TEI  { WTEI     }
	/ _TO   { WTO   $1 } / _TOI  { WTOI     } / _TUhE { WTUhE    }
	/ _TUhU { WTUhU    } / _UI   { WUI   $1 } / _VA   { WVA   $1 }
	/ _VAU  { WVAU     } / _VEI  { WVEI     } / _VEhO { WVEhO    }
	/ _VUhU { WVUhU $1 } / _VEhA { WVEhA $1 } / _VIhA { WVIhA $1 }
	/ _VUhO { WVUhO    } / _XI   { WXI      } / _ZAhO { WZAhO $1 }
	/ _ZEhA { WZEhA $1 } / _ZEI  { WZEI     } / _ZI   { WZI   $1 }
	/ _ZIhE { WZIhE    } / _ZO   { WZO      } / _ZOI  { WZOI  $1 }
	/ _ZOhU { WZOhU    } / cmavo { WCMAVO $1 }

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

-------------------------------------------------------------------- 1533

glide :: Char = (i / u) &nucleus !glide
diphthong :: (Char, Char) = (a i / a u / e i / o i) !nucleus !glide
vowel :: Char = (a / e / i / o / u) !nucleus

a :: Char = comma* [aA]			{ $2 }
e :: Char = comma* [eE]			{ $2 }
i :: Char = comma* [iI]			{ $2 }
o :: Char = comma* [oO]			{ $2 }
u :: Char = comma* [uU]			{ $2 }
y :: Char = comma* [yY]			{ $2 }

-------------------------------------------------------------------- 1553

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

-------------------------------------------------------------------- 1613

digit :: Char = comma* [0123456789] !h !nucleus 		{ $2 }
post_word :: () = pause / !nucleus lojban_word			{ () }
pause :: () = comma* space_char { () } / eof
eof :: () = comma* !.						{ () }
comma :: () = [,]						{ () }
non_lojban_word :: String = !lojban_word non_space+
non_space :: Char = !space_char .
space_char :: () = [.?! ] { () } / space_char1 / space_char2 / space_char3
space_char1 :: () = '\t'					{ () }
space_char2 :: () = '\r'					{ () }
space_char3 :: () = '\n'					{ () }

-------------------------------------------------------------------- 1636

spaces :: () = !y initial_spaces				{ () }
initial_spaces :: ()
	= (comma* space_char { () } / !ybu y { () })+ eof?	{ () }
	/ eof
ybu :: Lerfu = y space_char* _BU				{ Lerfu 'y' }
lujvo :: String = !gismu !fuhivla brivla

-------------------------------------------------------------------- 1646

_A :: A = &cmavo
	( a	{ A }
	/ e	{ E }
	/ j i	{ JI }
	/ o	{ O }
	/ u	{ U } )
	&post_word

_BAI :: BAI = &cmavo
	( d u h o	{ DUhO }
	/ s i h u	{ SIhU }
	/ z a u		{ ZAU  }
	/ k i h i	{ KIhI }
	/ d u h i	{ DUhI }
	/ c u h u	{ CUhU }
	/ t u h i	{ TUhI }
	/ t i h u	{ TIhU }
	/ d i h o	{ DIhO }
	/ j i h u	{ JIhU }
	/ r i h a	{ RIhA }
	/ n i h i	{ NIhI }
	/ m u h i	{ MUhI }
	/ k i h u	{ KIhU }
	/ b a i		{ BAI  }
	/ f i h e	{ FIhE }
	/ d e h i	{ DEhI }
	/ c i h o	{ CIhO }
	/ m a u		{ MAU  }
	/ m u h u	{ MUhU }
	/ r i h i	{ RIhI }
	/ r a h i	{ RAhI }
	/ k a h a	{ KAhA }
	/ p a h u	{ PAhU }
	/ p a h a	{ PAhA }
	/ l e h a	{ LEhA }
	/ k u h u	{ KUhU }
	/ t a i		{ TAI  }
	/ b a u		{ BAU  }
	/ m a h i	{ MAhI }
	/ c i h e	{ CIhE }
	/ f a u		{ FAU  }
	/ p o h i	{ POhI }
	/ c a u		{ CAU  }
	/ m a h e	{ MAhE }
	/ c i h u	{ CIhU }
	/ r a h a	{ RAhA }
	/ p u h a	{ PUhA }
	/ l i h e	{ LIhE }
	/ l a h u	{ LAhU }
	/ b a h i	{ BAhI }
	/ k a h i	{ KAhI }
	/ s a u		{ SAU  }
	/ f a h e	{ FAhE }
	/ b e h i	{ BEhI }
	/ t i h i	{ TIhI }
	/ j a h e	{ JAhE }
	/ g a h a	{ GAhA }
	/ v a h o	{ VAhO }
	/ j i h o	{ JIhO }
	/ m e h a	{ MEhA }
	/ d o h e	{ DOhE }
	/ j i h e	{ JIhE }
	/ p i h o	{ PIhO }
	/ g a u		{ GAU  }
	/ z u h e	{ ZUhE }
	/ m e h e	{ MEhE }
	/ r a i		{ RAI  } )
	&post_word

_BAhE :: BAhE = &cmavo
	( b a h e	{ BAhE }
	/ z a h e	{ ZAhE } )
	&post_word

_BE :: () = &cmavo b e &post_word		{ () }
_BEI :: () = &cmavo b e i &post_word		{ () }
_BEhO :: () = &cmavo b e h o &post_word	{ () }
_BIhE :: () = &cmavo b i h e &post_word	{ () }
_BIhI :: () = &cmavo b i h i &post_word	{ () }
_BO :: () = &cmavo b o &post_word		{ () }
_BOI :: () = &cmavo b o i &post_word		{ () }
_BU :: () = &cmavo (b u) &post_word		{ () }

_BY :: Lerfu
	= ybu
	/ &cmavo
	( j o h o	{ JOhO }
	/ r u h o	{ RUhO }
	/ g e h o	{ GEhO }
	/ j e h o	{ JEhO }
	/ l o h a	{ LOhA }
	/ n a h a	{ NAhA }
	/ s e h e	{ SEhE }
	/ t o h a	{ TOhA }
	/ g a h e	{ GAhE }
	/ y h y		{ Lerfu '\'' }
	/ b y		{ Lerfu 'b' }
	/ c y		{ Lerfu 'c' }
	/ d y		{ Lerfu 'd' }
	/ f y		{ Lerfu 'f' }
	/ g y		{ Lerfu 'g' }
	/ j y		{ Lerfu 'j' }
	/ k y		{ Lerfu 'k' }
	/ l y		{ Lerfu 'l' }
	/ m y		{ Lerfu 'm' }
	/ n y		{ Lerfu 'n' }
	/ p y		{ Lerfu 'p' }
	/ r y		{ Lerfu 'r' }
	/ s y		{ Lerfu 's' }
	/ t y		{ Lerfu 't' }
	/ v y		{ Lerfu 'v' }
	/ x y		{ Lerfu 'x' }
	/ z y		{ Lerfu 'z' } )
	&post_word

_CAhA :: CAhA = &cmavo
	( c a h a	{ CAhA }
	/ p u h i	{ PUhI }
	/ n u h o	{ NUhO }
	/ k a h e	{ KAhE } )
	&post_word

_CAI :: CAI = &cmavo
	( p e i		{ PEI }
	/ c a i		{ CAI }
	/ c u h i	{ CUhI }
	/ s a i		{ SAI }
	/ r u h e	{ RUhE } )
	&post_word

_CEI :: () = &cmavo c e i &post_word		{ () }
_CEhE :: () = &cmavo c e h e &post_word	{ () }
_CO :: () = &cmavo c o &post_word		{ () }

_COI :: COI = &cmavo
	( j u h i	{ JUhI }
	/ c o i		{ COI }
	/ f i h i	{ FIhI }
	/ t a h a	{ TAhA }
	/ m u h o	{ MUhO }
	/ f e h o	{ FEhO }
	/ c o h o	{ COhO }
	/ p e h u	{ PEhU }
	/ k e h o	{ KEhO }
	/ n u h e	{ NUhE }
	/ r e h i	{ REhI }
	/ b e h e	{ BEhE }
	/ j e h e	{ JEhE }
	/ m i h e	{ MIhE }
	/ k i h e	{ KIhE }
	/ v i h o	{ VIhO } )
	&post_word

_CU :: () = &cmavo c u &post_word		{ () }

_CUhE :: CUhE = &cmavo
	( c u h e	{ CUhE }
	/ n a u		{ NAU } )
	&post_word

_DAhO :: () = &cmavo d a h o &post_word	{ () }
_DOI :: () = &cmavo d o i &post_word		{ () }
_DOhU :: () = &cmavo d o h u &post_word	{ () }

_FA :: FA = &cmavo 
	( f a i		{ FAI }
	/ f a		{ FA }
	/ f e		{ FE }
	/ f o		{ FO }
	/ f u		{ FU }
	/ f i h a	{ FIhA }
	/ f i		{ FI } )
	&post_word

_FAhA :: FAhA = &cmavo
	( d u h a	{ DUhA }
	/ b e h a	{ BEhA }
	/ n e h u	{ NEhU }
	/ v u h a	{ VUhA }
	/ g a h u	{ GAhU }
	/ t i h a	{ TIhA }
	/ n i h a	{ NIhA }
	/ c a h u	{ CAhU }
	/ z u h a	{ ZUhA }
	/ r i h u	{ RIhU }
	/ r u h u	{ RUhU }
	/ r e h o	{ REhO }
	/ t e h e	{ TEhE }
	/ b u h u	{ BUhU }
	/ n e h a	{ NEhA }
	/ p a h o	{ PAhO }
	/ n e h i	{ NEhI }
	/ t o h o	{ TOhO }
	/ z o h i	{ ZOhI }
	/ z e h o	{ ZEhO }
	/ z o h a	{ ZOhA }
	/ f a h a	{ FAhA } )
	&post_word

_FAhO :: () = &cmavo f a h o &post_word	{ () }
_FEhE :: () = &cmavo f e h e &post_word	{ () }
_FEhU :: () = &cmavo f e h u &post_word	{ () }
_FIhO :: () = &cmavo f i h o &post_word	{ () }
_FOI :: () = &cmavo f o i &post_word		{ () }
_FUhA :: () = &cmavo f u h a &post_word	{ () }
_FUhE :: () = &cmavo f u h e &post_word	{ () }
_FUhO :: () = &cmavo f u h o &post_word	{ () }

_GA :: GA = &cmavo
	( g e h i	{ GEhI }
	/ g e		{ GE }
	/ g o		{ GO }
	/ g a		{ GA }
	/ g u		{ GU } )
	&post_word

_GAhO :: GAhO = &cmavo
	( k e h i	{ KEhI }
	/ g a h o	{ GAhO } )
	&post_word

_GEhU :: () = &cmavo g e h u &post_word	{ () }
_GI :: () = &cmavo g i &post_word		{ () }

_GIhA :: GIhA = &cmavo
	( g i h e	{ GIhE }
	/ g i h i	{ GIhI }
	/ g i h o	{ GIhO }
	/ g i h a	{ GIhA }
	/ g i h u	{ GIhU } )
	&post_word

_GOI :: GOI = &cmavo
	( n o h u	{ NOhU }
	/ n e		{ NE }
	/ g o i		{ GOI }
	/ p o h u	{ POhU }
	/ p e		{ PE }
	/ p o h e	{ POhE }
	/ p o		{ PO } )
	&post_word

_GOhA :: GOhA = &cmavo
	( m o		{ MO }
	/ n e i		{ NEI }
	/ g o h u	{ GOhU }
	/ g o h o	{ GOhO }
	/ g o h i	{ GOhI }
	/ n o h a	{ NOhA }
	/ g o h e	{ GOhE }
	/ g o h a	{ GOhA }
	/ d u		{ DU }
	/ b u h a	{ BUhA }
	/ b u h e	{ BUhE }
	/ b u h i	{ BUhI }
	/ c o h e	{ COhE } )
	&post_word

_GUhA :: GUhA = &cmavo
	( g u h e	{ GUhE }
	/ g u h i	{ GUhI }
	/ g u h o	{ GUhO }
	/ g u h a	{ GUhA }
	/ g u h u	{ GUhU } )
	&post_word

_I :: () = &cmavo i &post_word		{ () }

_JA :: JA = &cmavo
	( j e h i	{ JEhI }
	/ j e		{ JE }
	/ j o		{ JO }
	/ j a		{ JA }
	/ j u		{ JU } )
	&post_word

_JAI :: () = &cmavo j a i &post_word		{ () }
_JOhI :: () = &cmavo j o h i &post_word	{ () }

_JOI :: JOI = &cmavo
	( f a h u	{ FAhU }
	/ p i h u	{ PIhU }
	/ j o i		{ JOI }
	/ c e h o	{ CEhO }
	/ c e		{ CE }
	/ j o h u	{ JOhU }
	/ k u h a	{ KUhA }
	/ j o h e	{ JOhE }
	/ j u h e	{ JUhE } )
	&post_word

_KE :: () = &cmavo k e &post_word		{ () }
_KEhE :: () = &cmavo k e h e &post_word	{ () }
_KEI :: () = &cmavo k e i &post_word		{ () }
_KI :: () = &cmavo k i &post_word		{ () }

_KOhA :: KOhA = &cmavo
	( d a h u	{ DAhU }
	/ d a h e	{ DAhE }
	/ d i h u	{ DIhU }
	/ d i h e	{ DIhE }
	/ d e h u	{ DEhU }
	/ d e h e	{ DEhE }
	/ d e i		{ DEI }
	/ d o h i	{ DOhI }
	/ m i h o	{ MIhO }
	/ m a h a	{ MAhA }
	/ m i h a	{ MIhA }
	/ d o h o	{ DOhO }
	/ k o h a	{ KOhA }
	/ f o h u	{ FOhU }
	/ k o h e	{ KOhE }
	/ k o h i	{ KOhI }
	/ k o h o	{ KOhO }
	/ k o h u	{ KOhU }
	/ f o h a	{ FOhA }
	/ f o h e	{ FOhE }
	/ f o h i	{ FOhI }
	/ f o h o	{ FOhO }
	/ v o h a	{ VOhA }
	/ v o h e	{ VOhE }
	/ v o h i	{ VOhI }
	/ v o h o	{ VOhO }
	/ v o h u	{ VOhU }
	/ r u		{ RU }
	/ r i		{ RI }
	/ r a		{ RA }
	/ t a		{ TA }
	/ t u		{ TU }
	/ t i		{ TI }
	/ z i h o	{ ZIhO }
	/ k e h a	{ KEhA }
	/ m a		{ MA }
	/ z u h i	{ ZUhI }
	/ z o h e	{ ZOhE }
	/ c e h u	{ CEhU }
	/ d a		{ DA }
	/ d e		{ DE }
	/ d i		{ DI }
	/ k o		{ KO }
	/ m i		{ MI }
	/ d o		{ DO } )
	&post_word

_KU :: () = &cmavo k u &post_word		{ () }
_KUhE :: () = &cmavo k u h e &post_word	{ () }
_KUhO :: () = &cmavo k u h o &post_word	{ () }

_LA :: LA = &cmavo
	( l a i		{ LAI }
	/ l a h i	{ LAhI }
	/ l a		{ LA } )
	&post_word

_LAU :: LAU = &cmavo
	( c e h a	{ CEhA }
	/ l a u		{ LAU }
	/ z a i		{ ZAI }
	/ t a u		{ TAU } )
	&post_word

_LAhE :: LAhE = &cmavo
	( t u h a	{ TUhA }
	/ l u h a	{ LUhA }
	/ l u h o	{ LUhO }
	/ l a h e	{ LAhE }
	/ v u h i	{ VUhI }
	/ l u h i	{ LUhI }
	/ l u h e	{ LUhE } )
	&post_word

_LE :: LE = &cmavo
	( l e i		{ LEI }
	/ l o i		{ LOI }
	/ l e h i	{ LEhI }
	/ l o h i	{ LOhI }
	/ l e h e	{ LEhE }
	/ l o h e	{ LOhE }
	/ l o		{ LO }
	/ l e		{ LE } )
	&post_word

_LEhU :: () = &cmavo l e h u &post_word	{ () }
_LI :: () = &cmavo l i &post_word		{ () }
_LIhU :: () = &cmavo l i h u &post_word	{ () }
_LOhO :: () = &cmavo l o h o &post_word	{ () }
_LOhU :: () = &cmavo l o h u &post_word	{ () }
_LU :: () = &cmavo l u &post_word		{ () }
_LUhU :: () = &cmavo l u h u &post_word	{ () }
_MAhO :: () = &cmavo m a h o &post_word	{ () }

_MAI :: MAI = &cmavo
	( m o h o	{ MOhO }
	/ m a i		{ MAI } )
	&post_word

_ME :: () = &cmavo m e &post_word		{ () }
_MEhU :: () = &cmavo m e h u &post_word	{ () }
_MOhE :: () = &cmavo m o h e &post_word	{ () }
_MOhI :: () = &cmavo m o h i &post_word	{ () }

_MOI :: MOI = &cmavo
	( m e i		{ MEI }
	/ m o i		{ MOI }
	/ s i h e	{ SIhE }
	/ c u h o	{ CUhO }
	/ v a h e	{ VAhE } )
	&post_word

_NA :: NA = &cmavo
	( j a h a	{ JAhA }
	/ n a		{ NA } )
	&post_word

_NAI :: () = &cmavo n a i &post_word		{ () }

_NAhE :: NAhE = &cmavo
	( t o h e	{ TOhE }
	/ j e h a	{ JEhA }
	/ n a h e	{ NAhE }
	/ n o h e	{ NOhE } )
	&post_word

_NAhU :: () = &cmavo n a h u &post_word	{ () }
_NIhE :: () = &cmavo n i h e &post_word	{ () }

_NIhO :: NIhO = &cmavo
	( n i h o	{ NIhO }
	/ n o h i	{ NOhI } )
	&post_word

_NOI :: NOI = &cmavo
	( v o i		{ VOI }
	/ n o i		{ NOI }
	/ p o i		{ POI } )
	&post_word

_NU :: NU = &cmavo
	( n i		{ NI }
	/ d u h u	{ DUhU }
	/ s i h o	{ SIhO }
	/ n u		{ NU }
	/ l i h i	{ LIhI }
	/ k a		{ KA }
	/ j e i		{ JEI }
	/ s u h u	{ SUhU }
	/ z u h o	{ ZUhO }
	/ m u h e	{ MUhE }
	/ p u h u	{ PUhU }
	/ z a h i	{ ZAhI } )
	&post_word

_NUhA :: () = &cmavo n u h a &post_word		{ () }
_NUhI :: () = &cmavo n u h i &post_word		{ () }
_NUhU :: () = &cmavo n u h u &post_word		{ () }

_PA :: PA = &cmavo
	( d a u		{ DAU }
	/ f e i		{ FEI }
	/ j a u		{ JAU }
	/ r e i		{ REI }
	/ v a i		{ VAI }
	/ p i h e	{ PIhE }
	/ p i		{ PI }
	/ f i h u	{ FIhU }
	/ z a h u	{ ZAhU }
	/ m e h i	{ MEhI }
	/ n i h u	{ NIhU }
	/ k i h o	{ KIhO }
	/ c e h i	{ CEhI }
	/ m a h u	{ MAhU }
	/ r a h e	{ RAhE }
	/ d a h a	{ DAhA }
	/ s o h a	{ SOhA }
	/ j i h i	{ JIhI }
	/ s u h o	{ SUhO }
	/ s u h e	{ SUhE }
	/ r o		{ RO }
	/ r a u		{ RAU }
	/ s o h u	{ SOhU }
	/ s o h i	{ SOhI }
	/ s o h e	{ SOhE }
	/ s o h o	{ SOhO }
	/ m o h a	{ MOhA }
	/ d u h e	{ DUhE }
	/ t e h o	{ TEhO }
	/ k a h o	{ KAhO }
	/ c i h i	{ CIhI }
	/ t u h o	{ TUhO }
	/ x o		{ XO }
	/ p a i		{ PAI }
	/ n o h o	{ NOhO }
	/ n o		{ NO }
	/ p a		{ PA }
	/ r e		{ RE }
	/ c i		{ CI }
	/ v o		{ VO }
	/ m u		{ MU }
	/ x a		{ XA }
	/ z e		{ ZE }
	/ b i		{ BI }
	/ s o		{ SO }
	/ digit		{ case $1 of
				'0' -> NO
				'1' -> PA
				'2' -> RE
				'3' -> CI
				'4' -> VO
				'5' -> MU
				'6' -> XA
				'7' -> ZE
				'8' -> BI
				'9' -> SO } )
	&post_word

_PEhE :: () = &cmavo p e h e &post_word		{ () }
_PEhO :: () = &cmavo p e h o &post_word		{ () }

_PU :: PU = &cmavo
	( b a		{ BA }
	/ p u		{ PU }
	/ c a		{ CA } )
	&post_word

_RAhO :: () = &cmavo r a h o &post_word		{ () }

_ROI :: ROI = &cmavo
	( r e h u	{ REhU }
	/ r o i		{ ROI } )
	&post_word

_SA :: () = &cmavo s a &post_word			{ () }

_SE :: SE = &cmavo
	( s e		{ SE }
	/ t e		{ TE }
	/ v e		{ VE }
	/ x e		{ XE } )
	&post_word

_SEI :: SEI = &cmavo
	( s e i		{ SEI }
	/ t i h o	{ TIhO } )
	&post_word

_SEhU :: () = &cmavo s e h u &post_word		{ () }
_SI :: () = &cmavo s i &post_word			{ () }
_SOI :: () = &cmavo s o i &post_word			{ () }
_SU :: () = &cmavo s u &post_word			{ () }

_TAhE :: TAhE = &cmavo
	( r u h i	{ RUhI }
	/ t a h e	{ TAhE }
	/ d i h i	{ DIhI }
	/ n a h o	{ NAhO } )
	&post_word

_TEhU :: () = &cmavo t e h u &post_word		{ () }
_TEI :: () = &cmavo t e i &post_word			{ () }

_TO :: TO = &cmavo
	( t o h i	{ TOhI }
	/ t o		{ TO } )
	&post_word

_TOI :: () = &cmavo t o i &post_word			{ () }
_TUhE :: () = &cmavo t u h e &post_word		{ () }
_TUhU :: () = &cmavo t u h u &post_word		{ () }

_UI :: UI = &cmavo
	( i h a		{ IhA }
	/ i e		{ IE }
	/ a h e		{ AhE }
	/ u h i		{ UhI }
	/ i h o		{ IhO }
	/ i h e		{ IhE }
	/ a h a		{ AhA }
	/ i a		{ IA }
	/ o h i		{ OhI }
	/ o h e		{ OhE }
	/ e h e		{ EhE }
	/ o i		{ OI }
	/ u o		{ UO }
	/ e h i		{ EhI }
	/ u h o		{ UhO }
	/ a u		{ AU }
	/ u a		{ UA }
	/ a h i		{ AhI }
	/ i h u		{ IhU }
	/ i i		{ II }
	/ u h a		{ UhA }
	/ u i		{ UI }
	/ a h o		{ AhO }
	/ a i		{ AI }
	/ a h u		{ AhU }
	/ i u		{ IU }
	/ e i		{ EI }
	/ o h o		{ OhO }
	/ e h a		{ EhA }
	/ u u		{ UU }
	/ o h a		{ OhA }
	/ o h u		{ OhU }
	/ u h u		{ UhU }
	/ e h o		{ EhO }
	/ i o		{ IO }
	/ e h u		{ EhU }
	/ u e		{ UE }
	/ i h i		{ IhI }
	/ u h e		{ UhE }
	/ b a h a	{ BAhA }
	/ j a h o	{ JAhO }
	/ c a h e	{ CAhE }
	/ s u h a	{ SUhA }
	/ t i h e	{ TIhE }
	/ k a h u	{ KAhU }
	/ s e h o	{ SEhO }
	/ z a h a	{ ZAhA }
	/ p e h i	{ PEhI }
	/ r u h a	{ RUhA }
	/ j u h a	{ JUhA }
	/ t a h o	{ TAhO }
	/ r a h u	{ RAhU }
	/ l i h a	{ LIhA }
	/ b a h u	{ BAhU }
	/ m u h a	{ MUhA }
	/ d o h a	{ DOhA }
	/ t o h u	{ TOhU }
	/ v a h i	{ VAhI }
	/ p a h e	{ PAhE }
	/ z u h u	{ ZUhU }
	/ s a h e	{ SAhE }
	/ l a h a	{ LAhA }
	/ k e h u	{ KEhU }
	/ s a h u	{ SAhU }
	/ d a h i	{ DAhI }
	/ j e h u	{ JEhU }
	/ s a h a	{ SAhA }
	/ k a u		{ KAU }
	/ t a h u	{ TAhU }
	/ n a h i	{ NAhI }
	/ j o h a	{ JOhA }
	/ b i h u	{ BIhU }
	/ l i h o	{ LIhO }
	/ p a u		{ PAU }
	/ m i h u	{ MIhU }
	/ k u h i	{ KUhI }
	/ j i h a	{ JIhA }
	/ s i h a	{ SIhA }
	/ p o h o	{ POhO }
	/ p e h a	{ PEhA }
	/ r o h i	{ ROhI }
	/ r o h e	{ ROhE }
	/ r o h o	{ ROhO }
	/ r o h u	{ ROhU }
	/ r o h a	{ ROhA }
	/ r e h e	{ REhE }
	/ l e h o	{ LEhO }
	/ j u h o	{ JUhO }
	/ f u h i	{ FUhI }
	/ d a i		{ DAI }
	/ g a h i	{ GAhI }
	/ z o h o	{ ZOhO }
	/ b e h u	{ BEhU }
	/ r i h e	{ RIhE }
	/ s e h i	{ SEhI }
	/ s e h a	{ SEhA }
	/ v u h e	{ VUhE }
	/ k i h a	{ KIhA }
	/ x u		{ XU }
	/ g e h e	{ GEhE }
	/ b u h o	{ BUhO } )
	&post_word

_VA :: VA = &cmavo
	( v i		{ VI }
	/ v a		{ VA }
	/ v u		{ VU } )
	&post_word

_VAU :: () = &cmavo v a u &post_word		{ () }
_VEI :: () = &cmavo v e i &post_word		{ () }
_VEhO :: () = &cmavo v e h o &post_word	{ () }

_VUhU :: VUhU = &cmavo
	( g e h a	{ GEhA }
	/ f u h u	{ FUhU }
	/ p i h i	{ PIhI }
	/ f e h i	{ FEhI }
	/ v u h u	{ VUhU }
	/ s u h i	{ SUhI }
	/ j u h u	{ JUhU }
	/ g e i		{ GEI }
	/ p a h i	{ PAhI }
	/ f a h i	{ FAhI }
	/ t e h a	{ TEhA }
	/ c u h a	{ CUhA }
	/ v a h a	{ VAhA }
	/ n e h o	{ NEhO }
	/ d e h o	{ DEhO }
	/ f e h a	{ FEhA }
	/ s a h o	{ SAhO }
	/ r e h a	{ REhA }
	/ r i h o	{ RIhO }
	/ s a h i	{ SAhI }
	/ p i h a	{ PIhA }
	/ s i h i	{ SIhI } )
	&post_word

_VEhA :: VEhA = &cmavo
	( v e h u	{ VEhU }
	/ v e h a	{ VEhA }
	/ v e h i	{ VEhI }
	/ v e h e	{ VEhE } )
	&post_word

_VIhA :: VIhA = &cmavo
	( v i h i	{ VIhI }
	/ v i h a	{ VIhA }
	/ v i h u	{ VIhU }
	/ v i h e	{ VIhE } )
	&post_word

_VUhO :: () = &cmavo v u h o &post_word		{ () }
_XI :: () = &cmavo x i &post_word			{ () }
_Y :: () = &cmavo y+ &post_word			{ () }

_ZAhO :: ZAhO = &cmavo
	( c o h i	{ COhI }
	/ p u h o	{ PUhO }
	/ c o h u	{ COhU }
	/ m o h u	{ MOhU }
	/ c a h o	{ CAhO }
	/ c o h a	{ COhA }
	/ d e h a	{ DEhA }
	/ b a h o	{ BAhO }
	/ d i h a	{ DIhA }
	/ z a h o	{ ZAhO } )
	&post_word

_ZEhA :: ZEhA = &cmavo
	( z e h u	{ ZEhU }
	/ z e h a	{ ZEhA }
	/ z e h i	{ ZEhI }
	/ z e h e	{ ZEhE } )
	&post_word

_ZEI :: () = &cmavo z e i &post_word		{ () }

_ZI :: ZI = &cmavo
	( z u		{ ZU }
	/ z a		{ ZA }
	/ z i		{ ZI } )
	&post_word

_ZIhE :: () = &cmavo z i h e &post_word	{ () }
_ZO :: () = &cmavo z o &post_word		{ () }

_ZOI :: ZOI = &cmavo
	( z o i		{ ZOI }
	/ l a h o	{ LAhO } )
	&post_word

_ZOhU :: () = &cmavo z o h u &post_word	{ () }

|]

data Prenex = Prenex Term (Clause ())
	deriving Show

data Term
	= TSumti Sumti
	| TTag (Either Tag (AddFree (Clause FA)))
		(Either Sumti (AddFree (Maybe (Clause Unit))))
	| Term
	deriving Show

data Ek	= Ek (Maybe (Clause NA)) (Maybe (Clause SE))
		(Clause A) (Maybe (Clause Unit))
	deriving Show

data Gihek = Gihek (Maybe (Clause NA)) (Maybe (Clause SE))
	(Clause GIhA) (Maybe (Clause Unit))
	deriving Show

data Jek
	= Jek (Maybe (Clause NA)) (Maybe (Clause SE)) (Clause JA)
		(Maybe (Clause Unit))
	deriving Show

data Joik
	= Joik (Maybe (Clause SE)) (Clause JOI) (Maybe (Clause Unit))
	| JoikInterval (Maybe (Clause GAhO))
		(Maybe (Clause SE), Clause Unit, Maybe (Clause Unit))
		(Maybe (Clause GAhO))
	deriving Show

data Gek
	= GekGA (Maybe (Clause SE)) (Clause GA) (Maybe (Clause Unit))
	| Gek
	deriving Show

data Guhek
	= Guhek (Maybe (Clause SE)) (Clause GUhA) (Maybe (Clause Unit))
	deriving Show

data Tag
	= Stag TenseModal [(Either Joik Jek, TenseModal)]
	| Tag (AddFree TenseModal)
		[(AddFree (Either Joik Jek), (AddFree TenseModal))]
	deriving Show

data TenseModal
	= TMBAI (Maybe (Clause NAhE)) (Maybe (Clause SE)) (Clause BAI)
		(Maybe (Clause Unit)) (Maybe (Clause Unit))
	| TMTense (Maybe (Clause NAhE)) (Maybe TimeSpace, Maybe (Clause CAhA))
		(Maybe (Clause Unit))
	| TMKI (Clause Unit)
	| TMCUhE (Clause CUhE)
	| TMFIhO Selbri
	deriving Show

data TimeSpace
	= TimeSpace Time (Maybe Space_)
	| SpaceTime Space_ (Maybe Time)
	deriving Show

data Time = Time (Maybe (Clause ZI)) [TimeOffset]
	(Maybe (Clause ZEhA, Maybe (Clause PU, Maybe (Clause Unit))))
	[Interval]
	deriving Show

data TimeOffset
	= TimeOffset (Clause PU) (Maybe (Clause Unit)) (Maybe (Clause ZI))
	deriving Show

data Space_
	= Space_ (Maybe (Clause VA))
		[(Clause FAhA, Maybe (Clause Unit), Maybe (Clause VA))]
		(Maybe SpaceInterval)
		(Maybe (Clause Unit,
			(Clause FAhA, Maybe (Clause Unit), Maybe (Clause VA))))
	deriving Show

data SpaceInterval
	= SIVVSIP (Maybe (Clause VEhA), Maybe (Clause VIhA))
		(Maybe (Clause FAhA, Maybe (Clause Unit)))
		[(Clause Unit, Interval)]
	| SIVV (Maybe (Clause VEhA), Maybe (Clause VIhA))
		(Maybe (Clause FAhA, Maybe (Clause Unit)))
	| SISIP [(Clause Unit, Interval)]
	deriving Show

data Interval
	= IROI [Either (Clause Lerfu) (Clause PA)] (Clause ROI)
	| ITAhE (Clause TAhE) (Maybe (Clause Unit))
	| IZAhO (Clause ZAhO) (Maybe (Clause Unit))
	deriving Show

data Free
	= Free
	| FSEI [Free] (Maybe (Term, Maybe (Clause Unit), [Free])) Selbri
	| FSOI [Free] Sumti (Maybe Sumti)
	| FVocativeSelbri Vocative (Maybe Relative) Selbri (Maybe Relative)
	| FVocativeCMENE Vocative (Maybe Relative) [Clause CMENE] [Free]
		(Maybe Relative)
	| FVocativeSumti Vocative (Maybe Sumti)
	| FMAI [Either (Clause Lerfu) (Clause PA)] (Clause MAI)
	| FXI (AddFree XIString)
	deriving Show

data Sumti
	= SQuote (Clause Quote)
	| SKOhA (AddFree (Clause KOhA))
	| SLA (Clause LA) (AddFree [Clause CMENE])
	| SLE Selbri
	| SRelative Sumti Relative
	| SLerfuStr [Either (Clause Lerfu) (Clause PA)]
	deriving Show

data Relative
	= RelativePhrase (Clause GOI) Term
	| RMany Relative [(Clause (), Relative)]
	deriving Show

data Selbri = STanruUnit TanruUnit
	deriving Show

data TanruUnit = TUBRIVLA (AddFree (Clause BRIVLA))
	deriving Show

data Quote
	= SingleWordQuote String
	| DelimitedQuote ZOI String
	deriving Show

data Mex
	= Mex (Clause PA)
	deriving Show

data AddFree a
	= NoF a
	| AddFree a [Free]
	deriving Show

data Vocative
	= Vocative [(Clause COI, Maybe (Clause Unit))] (Maybe (Clause Unit))
	deriving Show

data Indicators
	= Ind [Indicator]
	| IFUhE [Indicator]
	deriving Show

data Indicator
	= IUI (Clause UI)
	| IUINAI (Clause UI) (Clause ())
	| ICAI (Clause CAI)
	| ICAINAI (Clause CAI) (Clause ())
	| IDAhO (Clause ())
	| IFUhO (Clause ())
	deriving Show

data Clause a
	= Raw a
	| Pre [BAhE] a
	| Post a [Indicators]
	| PrePost [BAhE] a [Indicators]
	deriving Show

data Word
	= WBRIVLA BRIVLA
	| WCMENE CMENE
	| WA A       | WBAI BAI   | WBAhE BAhE | WBE        | WBEI
	| WBEhO      | WBIhE      | WBIhI      | WBO        | WBOI
	| WBU        | WBY Lerfu  | WCAhA CAhA | WCAI CAI   | WCEI
	| WCEhE      | WCO        | WCOI COI   | WCU        | WCUhE CUhE
	| WDAhO      | WDOI       | WDOhU      | WFA FA     | WFAhA FAhA
	| WFAhO      | WFEhE      | WFEhU      | WFIhO      | WFOI
	| WFUhA      | WFUhE      | WFUhO      | WGA GA     | WGAhO GAhO
	| WGEhU      | WGI        | WGIhA GIhA | WGOI GOI   | WGOhA GOhA
	| WGUhA GUhA | WI         | WJA JA     | WJAI       | WJOhI
	| WJOI JOI   | WKE        | WKEhE      | WKEI       | WKI
	| WKOhA KOhA | WKU        | WKUhE      | WKUhO      | WLA LA
	| WLAU LAU   | WLAhE LAhE | WLE LE     | WLEhU      | WLI
	| WLIhU      | WLOhO      | WLOhU      | WLU        | WLUhU
	| WMAhO      | WMAI MAI   | WME        | WMEhU      | WMOhE
	| WMOhI      | WMOI MOI   | WNA NA     | WNAI       | WNAhE NAhE
	| WNAhU      | WNIhE      | WNIhO NIhO | WNOI NOI   | WNU NU
	| WNUhA      | WNUhI      | WNUhU      | WPA PA     | WPEhE
	| WPEhO      | WPU PU     | WRAhO      | WROI ROI   | WSA
	| WSE SE     | WSEI SEI   | WSEhU      | WSI        | WSOI
	| WSU        | WTAhE TAhE | WTEhU      | WTEI       | WTO TO
	| WTOI       | WTUhE      | WTUhU      | WUI UI     | WVA VA
	| WVAU       | WVEI       | WVEhO      | WVUhU VUhU | WVEhA VEhA
	| WVIhA VIhA | WVUhO      | WXI        | WZAhO ZAhO | WZEhA ZEhA
	| WZEI       | WZI ZI     | WZIhE      | WZO        | WZOI ZOI
	| WZOhU      | WCMAVO String
	deriving Show

data BRIVLA
	= BRIVLA String
	| ZEI Word [[String]] [String]
	deriving Show
data CMENE = CMENE String deriving Show

data A = A | E | JI | O | U deriving Show
data BAI
	= DUhO | SIhU | ZAU  | KIhI | DUhI | CUhU | TUhI | TIhU | DIhO | JIhU
	| RIhA | NIhI | MUhI | KIhU | VAhU | KOI  | CAhI | TAhI | PUhE | JAhI
	| KAI  | BAI  | FIhE | DEhI | CIhO | MAU  | MUhU | RIhI | RAhI | KAhA
	| PAhU | PAhA | LEhA | KUhU | TAI  | BAU  | MAhI | CIhE | FAU  | POhI
	| CAU  | MAhE | CIhU | RAhA | PUhA | LIhE | LAhU | BAhI | KAhI | SAU
	| FAhE | BEhI | TIhI | JAhE | GAhA | VAhO | JIhO | MEhA | DOhE | JIhE
	| PIhO | GAU  | ZUhE | MEhE | RAI
	deriving Show
data BAhE = BAhE | ZAhE deriving Show

data XIString
	= XIString [Either (Clause Lerfu) (Clause PA)] | XIMex Mex
	deriving Show

data Lerfu
	= Lerfu Char | JOhO | RUhO | JEhO | LOhA | NAhA | SEhE | GEhO | TOhA
	| GAhE
	| LBU Word [(Bool, [String])]
	deriving Show

data CAhA = CAhA | PUhI | NUhO | KAhE deriving Show
data CAI = PEI | CAI | CUhI | SAI | RUhE deriving Show

data COI
	= JUhI | COI  | FIhI | TAhA | MUhO | FEhO | COhO | PEhU | KEhO | NUhE
	| REhI | BEhE | JEhE | MIhE | KIhE | VIhO
	deriving Show

data CUhE = CUhE | NAU deriving Show
data FA = FAI | FA | FE | FO | FU | FIhA | FI deriving Show

data FAhA
	= DUhA | BEhA | NEhU | VUhA | GAhU | TIhA | NIhA | CAhU | ZUhA | RIhU
	| RUhU | REhO | TEhE | BUhU | NEhA | PAhO | NEhI | TOhO | ZOhI | ZEhO
	| ZOhA | FAhA
	deriving Show

data GA = GEhI | GE | GO | GA | GU deriving Show
data GAhO = KEhI | GAhO deriving Show
data GIhA = GIhE | GIhI | GIhO | GIhA | GIhU deriving Show
data GOI = NOhU | NE | GOI | POhU | PE | POhE | PO deriving Show

data GOhA
	= MO | NEI | GOhU | GOhO | GOhI | NOhA | GOhE | GOhA | DU | BUhA | BUhE
	| BUhI | COhE deriving Show

data GUhA = GUhE | GUhI | GUhO | GUhA | GUhU deriving Show
data JA = JEhI | JE | JO | JA | JU deriving Show
data JOI = FAhU | PIhU | JOI | CEhO | CE | JOhU | KUhA | JOhE | JUhE deriving Show

data KOhA
	= DAhU | DAhE | DIhU | DIhE | DEhU | DEhE | DEI  | DOhI | MIhO | MAhA
	| MIhA | DOhO | KOhA | FOhU | KOhE | KOhI | KOhO | KOhU | FOhA | FOhE
	| FOhI | FOhO | VOhA | VOhE | VOhI | VOhO | VOhU | RU   | RI   | RA
	| TA   | TU   | TI   | ZIhO | KEhA | MA   | ZUhI | ZOhE | CEhU | DA
	| DE   | DI   | KO   | MI   | DO
	deriving Show

data LA = LAI | LAhI | LA deriving Show
data LAU = CEhA | LAU | ZAI | TAU deriving Show
data LAhE = TUhA | LUhA | LUhO | LAhE | VUhI | LUhI | LUhE deriving Show
data LE = LEI | LOI | LEhI | LOhI | LEhE | LOhE | LO | LE deriving Show
data MAI = MOhO | MAI deriving Show
data MOI = MEI | MOI | SIhE | CUhO | VAhE deriving Show
data NA = JAhA | NA deriving Show
data NAhE = TOhE | JEhA | NAhE | NOhE deriving Show
data NIhO = NIhO | NOhI deriving Show
data NOI = VOI | NOI | POI deriving Show

data NU	= NI | DUhU | SIhO | NU | LIhI | KA | JEI | SUhU | ZUhO | MUhE | PUhU
	| ZAhI
	deriving Show

data PA	= DAU  | FEI  | GAI  | JAU  | REI  | VAI  | PIhE | PI   | FIhU | ZAhU
	| MEhI | NIhU | KIhO | CEhI | MAhU | RAhE | DAhA | SOhA | JIhI | SUhO
	| SUhE | RO   | RAU  | SOhU | SOhI | SOhE | SOhO | MOhA | DUhE | TEhO
	| KAhO | CIhI | TUhO | XO   | PAI  | NOhO | NO   | PA   | RE   | CI
	| VO   | MU   | XA   | ZE   | BI   | SO
	deriving Show

data PU = BA | PU | CA deriving Show
data ROI = REhU | ROI deriving Show
data SE = SE | TE | VE | XE deriving Show
data SEI = SEI | TIhO deriving Show
data TAhE = RUhI | TAhE | DIhI | NAhO deriving Show
data TO = TOhI | TO deriving Show

data UI	= IhA  | IE   | AhE  | UhI  | IhO  | IhE  | AhA  | IA   | OhI  | OhE
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
	deriving Show

data VA = VI | VA | VU deriving Show

data VUhU
	= GEhA | FUhU | PIhI | FEhI | VUhU | SUhI | JUhU | GEI  | PAhI | FAhI
	| TEhA | CUhA | VAhA | NEhO | DEhO | FEhA | SAhO | REhA | RIhO | SAhI
	| PIhA | SIhI
	deriving Show

data VEhA = VEhU | VEhA | VEhI | VEhE deriving Show
data VIhA = VIhI | VIhA | VIhU | VIhE deriving Show
data ZAhO = COhI | PUhO | COhU | MOhU | CAhO | COhA | DEhA | BAhO | DIhA | ZAhO
	deriving Show
data ZEhA = ZEhU | ZEhA | ZEhI | ZEhE deriving Show
data ZI = ZU | ZA | ZI deriving Show
data ZOI = ZOI | LAhO deriving Show

main :: IO ()
main = interact $ (++ "\n") . either show show .
	parseString test_parser "<stdin>" . preprocess

type Unit = ()

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

prePost :: a -> [BAhE] -> [Indicators] -> Clause a
prePost x pre post = case (pre, post) of
	([], []) -> Raw x
	(_, []) -> Pre pre x
	([], _) -> Post x post
	_ -> PrePost pre x post
