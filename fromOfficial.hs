{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Prelude hiding (words)

import Text.Peggy
import Data.List
import Data.Maybe
import Data.Char
import Control.Arrow

[peggy|

test_parser :: (Maybe Prenex, (Maybe Term, Selbri)) = statement eof	{ $1 }

--* GRAMMAR *************************************************************** 23

statement :: (Maybe Prenex, (Maybe Term, Selbri))
	= prenex? sentence

prenex :: Prenex
	= terms _ZOhU_clause		{ Prenex $1 $2 }

sentence :: (Maybe Term, Selbri)
	= terms? selbri

terms :: Term
	= term

term :: Term
	= term_1

term_1 :: Term
	= sumti		{ TSumti $1 }

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
	/ lerfu_string			{ SLerfuStr (fst $1) (snd $1) }
	/ _KOhA_clause free*		{ SKOhA $1 (Free $2) }
	/ _LA_clause _CMENE_clause+	{ SLA $1 $2 }

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
	= _BRIVLA_clause	{ TUBRIVLA $1 }

-- 245

-- 355
number :: (Clause PA, [Either (Clause PA) (Clause Lerfu)]) = _PA_clause
	( _PA_clause	{ Left $1 }
	/ lerfu_word	{ Right $1 } )*

lerfu_string :: (Clause Lerfu, [Either (Clause PA) (Clause Lerfu)]) = lerfu_word
	( _PA_clause	{ Left $1 }
	/ lerfu_word	{ Right $1 } )*

lerfu_word :: Clause Lerfu
	= _BY_clause
--	/ _LAU_clause lerfu_word
--	/ _TEI_clause lerfu_string _FOI_clause

free :: (Clause Lerfu, [Either (Clause PA) (Clause Lerfu)]) = xi_clause

-- 459
xi_clause :: (Clause Lerfu, [Either (Clause PA) (Clause Lerfu)])
	= _XI_clause lerfu_string	{ $2 }

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

zei_tail :: [String] = (_ZEI_clause (lojban_word spaces?))+
	{ map (fst . snd) $1 }
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
		!_SU_clause (lojban_word spaces?) si_clause?	{ () }

-- General Morphology Issues 498

-- 1. Spaces (including '.y') and UI are eaten *after* a word.

-- 3. BAhE is eaten *before* a word.

-- Handling of what can go after a cmavo
post_clause :: [Indicators]
	= spaces? si_clause? !_ZEI_clause !_BU_clause indicators*
	{ $3 }
post_clause_ind :: ()
	= spaces? si_clause? !_ZEI_clause !_BU_clause	{ () }
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

-- Handling of SI and interactions with zo and lo'u...le'u

si_clause :: ()
	= ((erasable_clause / si_word { () } / _SA_clause { () })
		si_clause? _SI_clause)+				{ () }

erasable_clause :: ()
	= bu_clause_no_pre !_ZEI_clause !_BU_clause		{ () }
	/ zei_clause_no_pre !_ZEI_clause !_BU_clause		{ () }

si_word :: ([BAhE], Word) = pre_zei_bu

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
_CAhA_pre :: ([BAhE], CAhA) = pre_clause _CAhA spaces?		{ ($1, $2) }

--	*** CAI: afterthought intensity marker ***
_CAI_clause :: Clause CAI = _CAI_pre _CAI_post	{ prePost (snd $1) (fst $1) [] }
_CAI_pre :: ([BAhE], CAI) = pre_clause _CAI spaces?	{ ($1, $2) }
_CAI_post :: () = post_clause_ind
_CAI_no_SA_handling :: () = pre_clause _CAI post_clause_ind	{ () }

--	*** CEI: pro-bridi assignment operator ***
_CEI_pre :: [BAhE] = pre_clause _CEI spaces?	{ $1 }

--	*** CEhE: afterthought term list connective ***
_CEhE_pre :: [BAhE] = pre_clause _CEhE spaces?	{ $1 }

---	*** CO: tanru inversion ***
_CO_pre :: [BAhE] = pre_clause _CO spaces?	{ $1 }

---	*** COI: vocative marker permitted inside name ***
_COI_pre :: ([BAhE], COI) = pre_clause _COI spaces?	{ ($1, $2) }

---	*** CU: separator between head sumti and selbri ***
_CU_pre :: [BAhE] = pre_clause _CU spaces?	{ $1 }

---	*** CUhE: tensemodal question ***
_CUhE_pre :: ([BAhE], CUhE) = pre_clause _CUhE spaces?	{ ($1, $2) }

--	*** DAhO: cancel anaphoracataphora assignments ***
_DAhO_clause :: Clause Unit = _DAhO_pre _DAhO_post	{ prePost () $1 $2 }
_DAhO_pre :: [BAhE] = pre_clause _DAhO spaces?		{ $1 }
_DAhO_post :: [Indicators] = post_clause
_DAhO_no_SA_handling :: () = pre_clause _DAhO post_clause	{ () }

--	*** DOI: vocative marker ***
_DOI_pre :: [BAhE] = pre_clause _DOI spaces?		{ $1 }

--	*** DOhU: terminator for DOI_marked vocatives ***
_DOhU_pre :: [BAhE] = pre_clause _DOhU spaces?		{ $1 }

--	*** FA: modifier head generic case tag ***
_FA_pre :: ([BAhE], FA) = pre_clause _FA spaces?	{ ($1, $2) }

--	*** FAhA: superdirections in space ***
_FAhA_pre :: ([BAhE], FAhA) = pre_clause _FAhA spaces?	{ ($1, $2) }

--	*** FAhO: normally elided 'done pause' to indicate end of utterance string ***
_FAhO_clause :: () = pre_clause _FAhO spaces?	{ () }

--	*** FEhE: space interval mod flag ***
_FEhE_pre :: [BAhE] = pre_clause _FEhE spaces?	{ $1 }

--	*** FEhU: ends bridi to modal conversion ***
_FEhU_pre :: [BAhE] = pre_clause _FEhU spaces?	{ $1 }

--	*** FIhO: marks bridi to modal conversion ***
_FIhO_pre :: [BAhE] = pre_clause _FIhO spaces?	{ $1 }

--	*** FOI: end compound lerfu ***
_FOI_pre :: [BAhE] = pre_clause _FOI spaces?	{ $1 }

--	*** FUhA: reverse Polish flag ***
_FUhA_pre :: [BAhE] = pre_clause _FUhA spaces?	{ $1 }

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
_GA_pre :: ([BAhE], GA) = pre_clause _GA spaces?	{ ($1, $2) }

--	*** GAhO: openclosed interval markers for BIhI ***
_GAhO_pre :: ([BAhE], GAhO) = pre_clause _GAhO spaces?	{ ($1, $2) }

---	*** GEhU: marker ending GOI relative clauses ***
_GEhU_pre :: [BAhE] = pre_clause _GAhO spaces?		{ $1 }

---	*** GI: forethought medial marker ***
_GI_pre :: [BAhE] = pre_clause _GI spaces?		{ $1 }

--	*** GIhA: logical connective for bridi-tails ***
_GIhA_pre :: ([BAhE], GIhA) = pre_clause _GIhA spaces?	{ ($1, $2) }

--	*** GOI: attaches a sumti modifier to a sumti ***
_GOI_clause :: Clause GOI = _GOI_pre _GOI_post	{ prePost (snd $1) (fst $1) $2 }
_GOI_pre :: ([BAhE], GOI) = pre_clause _GOI spaces?	{ ($1, $2) }
_GOI_post :: [Indicators] = post_clause
_GOI_no_SA_handling :: () = pre_clause _GOI post_clause	{ () }

--	*** GOhA: pro-bridi ***
_GOhA_pre :: ([BAhE], GOhA) = pre_clause _GOhA spaces?	{ ($1, $2) }

--	*** GUhA: GEK for tanru units, corresponds to JEKs ***
_GUhA_pre :: ([BAhE], GUhA) = pre_clause _GUhA spaces?	{ ($1, $2) }

--	*** I: sentence link ***
_I_pre :: [BAhE] = pre_clause _I spaces?		{ $1 }

--	*** JA: jeks; logical connectives within tanru ***
_JA_pre :: ([BAhE], JA) = pre_clause _JA spaces?	{ ($1, $2) }

--	*** JAI: modal conversion flag ***
_JAI_pre :: [BAhE] = pre_clause _JAI spaces?		{ $1 }

--	*** JOhI: flags an array operand ***
_JOhI_pre :: [BAhE] = pre_clause _JOhI spaces?		{ $1 }

--	*** JOI: non-logical connectives ***
_JOI_pre :: ([BAhE], JOI) = pre_clause _JOI spaces?	{ ($1, $2) }

--	*** KE: left long scope marker ***
_KE_pre :: [BAhE] = pre_clause _KE spaces?		{ $1 }

--	*** KEhE: right terminator for KE groups ***
_KEhE_pre :: [BAhE] = pre_clause _KEhE spaces?		{ $1 }

--	*** KEI: right terminator, NU abstractions ***
_KEI_pre :: [BAhE] = pre_clause _KEI spaces?		{ $1 }

--	*** KI: multiple utterance scope for tenses ***
_KI_pre :: [BAhE] = pre_clause _KI spaces?		{ $1 }

--	*** KOhA: sumti anaphora ***
_KOhA_clause :: Clause KOhA = _KOhA_pre _KOhA_post
	{ prePost (snd $1) (fst $1) $2 }
_KOhA_pre :: ([BAhE], KOhA) = pre_clause _KOhA spaces?		{ ($1, $2) }
_KOhA_post :: [Indicators] = post_clause
_KOhA_no_SA_handling :: () = pre_clause _KOhA spaces?		{ () }

--	*** KU: right terminator for descritions, etc. ***
_KU_pre :: [BAhE] = pre_clause _KU spaces?			{ $1 }

--	*** KUhE: MEX forethought delimiter ***
_KUhE_pre :: [BAhE] = pre_clause _KUhE spaces?			{ $1 }

--	*** KUhO: right terminator, NOI relative clauses ***
_KUhO_pre :: [BAhE] = pre_clause _KUhO spaces?			{ $1 }

--	*** LA: name descriptors ***
_LA_clause :: Clause LA = _LA_pre _LA_post	{ prePost (snd $1) (fst $1) $2 }
_LA_pre :: ([BAhE], LA) = pre_clause _LA spaces?		{ ($1, $2) }
_LA_post :: [Indicators] = post_clause
_LA_no_SA_handling :: () = pre_clause _LA post_clause		{ () }

--	*** LAU: lerfu prefixes ***
_LAU_pre :: ([BAhE], LAU) = pre_clause _LAU spaces?		{ ($1, $2) }

--	*** LAhE: sumti qualifiers ***
_LAhE_pre :: ([BAhE], LAhE) = pre_clause _LAhE spaces?		{ ($1, $2) }

--	*** LE: sumti descriptors ***
_LE_pre :: ([BAhE], LE) = pre_clause _LE spaces?		{ ($1, $2) }

--	*** LEhU: posibbly ungrammatical text right quote ***
_LEhU_pre :: [BAhE] = pre_clause _LEhU spaces?			{ $1 }

--	*** LI: convert number to sumti ***
_LI_pre :: [BAhE] = pre_clause _LI spaces?			{ $1 }

--	*** LIhU: grammatical text right quote ***
_LIhU_pre :: [BAhE] = pre_clause _LIhU spaces?			{ $1 }

--	*** LOhO: elidable terminator for LI ***
_LOhO_pre :: [BAhE] = pre_clause _LOhO spaces?			{ $1 }

--	*** LOhU: possibly ungrammatical text left quote ***
_LOhU_pre :: [BAhE] = pre_clause _LOhU spaces?			{ $1 }

--	*** LU: grammatical text left quote ***
_LU_pre :: [BAhE] = pre_clause _LU spaces?			{ $1 }

--	*** LUhU: LAhE close delimiter ***
_LUhU_pre :: [BAhE] = pre_clause _LUhU spaces?			{ $1 }

--	*** MAhO: change MEX expressions to MEX operators ***
_MAhO_pre :: [BAhE] = pre_clause _MAhO spaces?			{ $1 }

--	*** MAI: change numbers to utterance ordinals ***
_MAI_pre :: ([BAhE], MAI) = pre_clause _MAI spaces?		{ ($1, $2) }

--	*** ME: change numbers to utterance ordinals ***
_ME_pre :: [BAhE] = pre_clause _ME spaces?			{ $1 }

--	*** MEhU: terminator for ME ***
_MEhU_pre :: [BAhE] = pre_clause _MEhU spaces?			{ $1 }

--	*** MOhE: change sumti to operand, inverse of LI ***
_MOhE_pre :: [BAhE] = pre_clause _MOhE spaces?			{ $1 }

--	*** MOhI: motion tense marker ***
_MOhI_pre :: [BAhE] = pre_clause _MOhI spaces?			{ $1 }

--	*** MOI: change number to selbri ***
_MOI_pre :: ([BAhE], MOI) = pre_clause _MOI spaces?		{ ($1, $2) }

--	*** NA: bridi negation ***
_NA_pre :: ([BAhE], NA) = pre_clause _NA spaces?		{ ($1, $2) }

--	*** NAI: attached to words to negate them ***
_NAI_clause :: Clause Unit = _NAI_pre _NAI_post		{ prePost () $1 [] }
_NAI_pre :: [BAhE] = pre_clause _NAI spaces?			{ $1 }
_NAI_post :: () = post_clause_ind
_NAI_no_SA_handling :: () = pre_clause _NAI post_clause_ind	{ () }

--	*** NAhE: scalar negation ***
_NAhE_pre :: ([BAhE], NAhE) = pre_clause _NAhE spaces?		{ ($1, $2) }

--	*** NAhU: change a selbri into an operator ***
_NAhU_pre :: [BAhE] = pre_clause _NAhU spaces?			{ $1 }

--	*** NIhE: change selbri to operand; inverse of MOI ***
_NIhE_pre :: [BAhE] = pre_clause _NIhE spaces?			{ $1 }

--	*** NIhO: new paragraph; change of subject ***
_NIhO_pre :: ([BAhE], NIhO) = pre_clause _NIhO spaces?		{ ($1, $2) }

--	*** NOI: attaches a subordinate clause to a sumti ***
_NOI_pre :: ([BAhE], NOI) = pre_clause _NOI spaces?		{ ($1, $2) }

--	*** NU: abstraction ***
_NU_pre :: ([BAhE], NU) = pre_clause _NU spaces?		{ ($1, $2) }

--	*** NUhA: change operator to selbri; inverse of MOhE ***
_NUhA_pre :: [BAhE] = pre_clause _NUhA spaces?			{ $1 }

--	*** NUhI: marks the start of a termset ***
_NUhI_pre :: [BAhE] = pre_clause _NUhI spaces?			{ $1 }

--	*** NUhU: marks the middle and end of a termset ***
_NUhU_pre :: [BAhE] = pre_clause _NUhU spaces?			{ $1 }

--	*** PA: numbers and numeric punctuation ***
_PA_clause :: Clause PA = _PA_pre _PA_post	{ prePost (snd $1) (fst $1) $2 }
_PA_pre :: ([BAhE], PA) = pre_clause _PA spaces?{ ($1, $2) }
_PA_post :: [Indicators] = post_clause
_PA_no_SA_handling :: () = pre_clause _PA post_clause	{ () }

--	*** PEhE: afterthought termset connective prefix ***
_PEhE_pre :: [BAhE] = pre_clause _PEhE spaces?			{ $1 }

--	*** PEhO: forethought (Polish) flag ***
_PEhO_pre :: [BAhE] = pre_clause _PEhO spaces?			{ $1 }

--	*** PU: directions in time ***
_PU_pre :: ([BAhE], PU) = pre_clause _PU spaces?		{ ($1, $2) }

--	*** RAhO: flag for modified interpretation of GOhI ***
_RAhO_pre :: [BAhE] = pre_clause _RAhO spaces?			{ $1 }

--	*** ROI: converts number to extensinal tense ***
_ROI_pre :: ([BAhE], ROI) = pre_clause _ROI spaces?		{ ($1, $2) }

--	*** SA: metalinguistic eraser to the beginning of the current utterance ***
_SA_clause :: Clause Unit = _SA_pre _SA_post	{ prePost () $1 [] }
_SA_pre :: [BAhE] = pre_clause _SA spaces?	{ $1 }
_SA_post :: () = spaces?			{ () }

--	*** SE: conversions ***
_SE_pre :: ([BAhE], SE) = pre_clause _SE spaces?	{ ($1, $2) }

--	*** SEI: metalinguistic bridi insert marker ***
_SEI_pre :: ([BAhE], SEI) = pre_clause _SEI spaces?	{ ($1, $2) }

--	*** SEhU: metalinguistic bridi end marker ***
_SEhU_pre :: [BAhE] = pre_clause _SEhU spaces?		{ $1 }

--	*** SI: metalinguistic single word eraser ***
_SI_clause :: () = spaces? _SI spaces?		{ () }

--	*** SOI: reciprocal sumti marker ***
_SOI_pre :: [BAhE] = pre_clause _SOI spaces?	{ $1 }

--	*** SU: metalinguistic eraser of the entire text ***
_SU_clause :: Clause Unit = _SU_pre _SU_post	{ prePost () $1 $2 }
_SU_pre :: [BAhE] = pre_clause _SU spaces?	{ $1 }
_SU_post :: [Indicators] = post_clause

--	*** TAhE: tense interval properties ***
_TAhE_pre :: ([BAhE], TAhE) = pre_clause _TAhE spaces?	{ ($1, $2) }

--	*** TEhU: closing gap for MEX constructs ***
_TEhU_pre :: [BAhE] = pre_clause _TEhU spaces?		{ $1 }

--	*** TEI: start compound lerfu ***
_TEI_pre :: [BAhE] = pre_clause _TEI spaces?		{ $1 }

--	*** TO: left discursive parenthesis ***
_TO_pre :: ([BAhE], TO) = pre_clause _TO spaces?	{ ($1, $2) }

--	*** TOI: right discursive parenthesis ***
_TOI_pre :: [BAhE] = pre_clause _TOI spaces?		{ $1 }

--	*** TUhE: multiple utterance scope mark ***
_TUhE_pre :: [BAhE] = pre_clause _TUhE spaces?		{ $1 }

--	*** TUhU: multiple utterance end scope mark ***
_TUhU_pre :: [BAhE] = pre_clause _TUhU spaces?		{ $1 }

--	*** UI: attitudinals, observationals, discursives ***
_UI_clause :: Clause UI = _UI_pre _UI_post	{ prePost (snd $1) (fst $1) [] }
_UI_pre :: ([BAhE], UI) = pre_clause _UI spaces?{ ($1, $2) }
_UI_post :: () = post_clause_ind
_UI_no_SA_handling :: () = pre_clause _UI post_clause_ind		{ () }

--	*** VA: distance in space-time ***
_VA_pre :: ([BAhE], VA) = pre_clause _VA spaces?	{ ($1, $2) }

--	*** VAU: end simple bridi or bridi-tail ***
_VAU_pre :: [BAhE] = pre_clause _VAU spaces?		{ $1 }

--	*** VEI: left MEX bracket ***
_VEI_pre :: [BAhE] = pre_clause _VEI spaces?		{ $1 }

--	*** VEhO: right MEX bracket ***
_VEhO_pre :: [BAhE] = pre_clause _VEhO spaces?		{ $1 }

--	*** VUhU: MEX operator
_VUhU_pre :: ([BAhE], VUhU) = pre_clause _VUhU spaces?	{ ($1, $2) }

--	*** VEhA: space-time interval size
_VEhA_pre :: ([BAhE], VEhA) = pre_clause _VEhA spaces?	{ ($1, $2) }

--	*** VIhA: space-time dimensionality marker
_VIhA_pre :: ([BAhE], VIhA) = pre_clause _VIhA spaces?	{ ($1, $2) }

--	*** VUhO: glue between logically connected sumti and relative clauses
_VUhO_pre :: [BAhE] = pre_clause _VUhO spaces?		{ $1 }

--	*** XI: subscripting operator
_XI_clause :: Clause Unit = _XI_pre _XI_post	{ prePost () $1 $2 }
_XI_pre :: [BAhE] = pre_clause _XI spaces?	{ $1 }
_XI_post :: [Indicators] = post_clause
_XI_no_SA_handling :: () = pre_clause _XI post_clause	{ () }

--	*** Y: hesitation

--	*** ZAhO: event properties - inchoative, etc. ***
_ZAhO_clause :: Clause ZAhO = _ZAhO_pre _ZAhO_post	
	{ prePost (snd $1) (fst $1) $2 }
_ZAhO_pre :: ([BAhE], ZAhO) = pre_clause _ZAhO spaces?		{ ($1, $2) }
_ZAhO_post :: [Indicators] = post_clause
_ZAhO_no_SA_handling :: () = pre_clause _ZAhO post_clause	{ () }

--	*** ZEhA: time interval size tense ***
_ZEhA_clause :: Clause ZEhA = _ZEhA_pre _ZEhA_post
	{ prePost (snd $1) (fst $1) $2 }
_ZEhA_pre :: ([BAhE], ZEhA) = pre_clause _ZEhA spaces?	{ ($1, $2) }
_ZEhA_post :: [Indicators] = post_clause
_ZEhA_no_SA_handling :: () = pre_clause _ZEhA post_clause{ () }

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
_ZIhE_no_SA_handling :: () = pre_clause _ZIhE post_clause		{ () }

--	*** ZO: single word metalinguistic quote marker ***
_ZO_clause :: Clause Quote = _ZO_pre _ZO_post	{ prePost (snd $1) (fst $1) $2 }
_ZO_pre :: ([BAhE], Quote) = pre_clause _ZO spaces? lojban_word spaces?
						{ ($1, SingleWordQuote $4) }
_ZO_post :: [Indicators] = post_clause
_ZO_no_SA_handling :: () = pre_clause _ZO spaces? lojban_word spaces?	{ () }

--	*** ZOI: delimited quote marker ***

_ZOI_clause :: Clause Quote = _ZOI_pre _ZOI_post
					{ prePost (snd $1) (fst $ fst $1) $2 }
_ZOI_pre :: (([BAhE], ZOI), Quote)
	= pre_clause _ZOI spaces? "\x00" zoi_word* "\x00" spaces?
					{ (($1, $2), DelimitedQuote $2 $4) }
_ZOI_post :: [Indicators] = post_clause
_ZOI_no_SA_handling :: ()
	= pre_clause _ZOI spaces? "\x00" zoi_word* "\x00" spaces?	{ () }
zoi_word :: Char = !"\x00" .

--	*** ZOhU: prenex terminator (not elidable) ***

_ZOhU_clause :: Clause Unit = _ZOhU_pre _ZOhU_post	{ prePost () $1 $2 }
_ZOhU_pre :: [BAhE] = pre_clause _ZOhU spaces?		{ $1 }
_ZOhU_post :: [Indicators] = post_clause
_ZOhU_no_SA_handling :: () = pre_clause _ZOhU post_clause
							{ () }

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

data Term = TSumti Sumti
	deriving Show

data Free = Free [(Clause Lerfu, [Either (Clause PA) (Clause Lerfu)])]
	deriving Show

data Sumti
	= SQuote (Clause Quote)
	| SKOhA (Clause KOhA) Free
	| SLA (Clause LA) [Clause CMENE]
	| SRelative Sumti Relative
	| SLerfuStr (Clause Lerfu) [Either (Clause PA) (Clause Lerfu)]
	deriving Show

data Relative
	= RelativePhrase (Clause GOI) Term
	| RMany Relative [(Clause (), Relative)]
	deriving Show

data Selbri = STanruUnit TanruUnit
	deriving Show

data TanruUnit = TUBRIVLA (Clause BRIVLA)
	deriving Show

data Quote
	= SingleWordQuote String
	| DelimitedQuote ZOI String
	deriving Show

data Clause a
	= Raw a
	| Pre [BAhE] a
	| Post a [Indicators]
	| PrePost [BAhE] a [Indicators]
	deriving Show

-- type Indicators = (Bool, [Indicator])
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
