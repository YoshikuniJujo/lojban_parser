module CmavoList(
	CMAVO(..),
	digitToPA,
	cmavo_list
)where

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
	| CMAVO String
	deriving (Show, Eq)

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
	(VA  ,[	("vi"  , VI  ), ("va"  , VA  ), ("vu"  , VU  ) ]),
	(VAU ,[	("vau" , VAU ) ]),
	(VEI ,[	("vei" , VEI ) ]),
	(VEhO,[	("veho", VEhO) ]),
	(VUhU,[	("geha", GEhA), ("fuhu", FUhU), ("pihi", PIhI), ("fehi", FEhI),
		("vuhu", VUhU), ("suhi", SUhI), ("juhu", JUhU), ("gei" , GEI ),
		("pahi", PAhI), ("fahi", FAhI), ("teha", TEhA), ("cuha", CUhA),
		("vaha", VAhA), ("neho", NEhO), ("deho", DEhO), ("feha", FEhA),
		("saho", SAhO), ("reha", REhA), ("riho", RIhO), ("sahi", SAhI),
		("piha", PIhA), ("sihi", SIhI) ]),
	(VEhA,[	("vehu", VEhU), ("veha", VEhA), ("vehi", VEhI), ("vehe", VEhE) ]),
	(VIhA,[	("vihi", VIhI), ("veha", VIhA), ("vihu", VIhU), ("vihe", VIhE) ]),
	(VUhO,[	("vuho", VUhO) ]),
	(XI  ,[ ("xi"  , XI  ) ]),
	(Y   ,[	("y"   , Y   ) ]),
	(ZAhO,[	("cohi", COhI), ("puho", PUhO), ("cohu", COhU), ("mohu", MOhU),
		("caho", CAhO), ("coha", COhA), ("deha", DEhA), ("baho", BAhO),
		("diha", DIhA), ("zaho", ZAhO) ]),
	(ZEhA,[	("zehu", ZEhU), ("zeha", ZEhA), ("zehi", ZEhI), ("zehe", ZEhE) ]),
	(ZEI ,[	("zei" , ZEI ) ]),
	(ZI  ,[	("zu"  , ZU  ), ("za"  , ZA  ), ("zi"  , ZI  ) ]),
	(ZIhE,[	("zihe", ZIhE) ]),
	(ZO  ,[	("zo"  , ZO  ) ]),
	(ZOI ,[	("zoi" , ZOI ), ("laho", LAhO) ]),
	(ZOhU,[	("zohu", ZOhU) ])
 ]
