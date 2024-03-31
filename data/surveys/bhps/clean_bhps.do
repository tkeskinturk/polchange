
********************************************************************************
*** Cleaning the BHPS Data
*** Turgut Keskintürk, Duke University, Department of Sociology
********************************************************************************
* This file uses Philipp M. Lersch's replication code [p.m.lersch@hu-berlin.de]
********************************************************************************

global workdir ///
	"."
global datadir ///
	"./source_files"

********************************************************************************
********************************************************************************
	
*** Personal Culture Items Used in the Analyses
	
/// Group 01
foreach w in a b c d e f g h i j k l m n o p q r {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'doim `w'nchild /// identifiers
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp1", replace
	}
foreach w in a b c d e f g h i j k l m n o p q {
	append using "$workdir/temp_`w'indresp1"
	}
sort pidp svyyear
save "$workdir/temp_indresp1", replace

/// Group 02
foreach w in a c e g i k m o q {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'opfamf ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp2", replace
	}
foreach w in a c e g i k m o {
	append using "$workdir/temp_`w'indresp2"
	}
sort pidp svyyear
save "$workdir/temp_indresp2", replace

/// Group 03
foreach w in a c e g j n q {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'opsoca `w'opsocb `w'opsocc `w'opsocd `w'opsoce `w'opsocf ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp3", replace
	}
foreach w in a c e g j n  {
	append using "$workdir/temp_`w'indresp3"
	}
sort pidp svyyear
save "$workdir/temp_indresp3", replace

/// Group 04
foreach w in h j l n p r {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'opfamr ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp4", replace
	}
foreach w in h j l n p {
	append using "$workdir/temp_`w'indresp4"
	}
sort pidp svyyear
save "$workdir/temp_indresp4", replace

/// Group 05
foreach w in b d f h k m p {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'oppolb `w'oppolc ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp5", replace
	}
foreach w in b d f h k m {
	append using "$workdir/temp_`w'indresp5"
	}
sort pidp svyyear
save "$workdir/temp_indresp5", replace

/// Group 06
foreach w in a f j o {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'opcls3 ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp6", replace
	}

foreach w in a f j  {
	append using "$workdir/temp_`w'indresp6"
	}
sort pidp svyyear
save "$workdir/temp_indresp6", replace

/// Group 07
foreach w in a c d e g j {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'ophla ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp7", replace
	}

foreach w in a c d e g  {
	append using "$workdir/temp_`w'indresp7"
	}
sort pidp svyyear
save "$workdir/temp_indresp7", replace
	
/// Group 08
foreach w in h j m o q r {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp `w'trust ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp8", replace
	}

foreach w in h j m o q {
	append using "$workdir/temp_`w'indresp8"
	}
sort pidp svyyear
save "$workdir/temp_indresp8", replace	

/// Group 09
foreach w in a b c d e f k l m n o p q r {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp `w'vote6 ///
		using "$datadir/`w'indresp", clear
	rename `w'* *
	capture rename id pidp
	gen svyyear = `waveno' + 1990
	sort pidp
	save "$workdir/temp_`w'indresp9", replace
	}

foreach w in a b c d e f k l m n o p q {
	append using "$workdir/temp_`w'indresp9"
	}
sort pidp svyyear
save "$workdir/temp_indresp9", replace

********************************************************************************
********************************************************************************

* Parenthood

use pp childbirth1 fertility last_int using "$datadir/family", clear
rename pp pid

save "$workdir/temp_family",replace

* Administrative Information

use pidp pid ?ivfho ?hid ?ivfio ?pno using "$datadir/xwaveid", clear

local year = 1991 
foreach w in a b c d e f g h i j k l m n o p q r {
	renvars `w'ivfho `w'hid `w'ivfio `w'pno, postfix(`year')
	renvars `w'ivfho`year' `w'hid`year' `w'ivfio`year' `w'pno`year++', ///
	predrop(1)
	}

reshape long ivfho hid ivfio pno, i(pidp) j(svyyear)

sort pidp svyyear
save "$workdir/temp_xwaveid", replace

* Merge Temp Files Into One `dta' File

use pidp memorig ///
	using "$datadir/xwavedat", clear
expand 18
sort pidp
by pidp: gen svyyear = 1990 + _n

merge 1:1 pidp svyyear using "$workdir/temp_xwaveid", nogen keep(1 3)
mvdecode hid, mv(-8)

forvalues n = 1 / 9 {
	merge 1:1 pidp svyyear using "$workdir/temp_indresp`n'", nogen keep(1 3)
	}
	
merge m:1 pid using "$workdir/temp_family", nogen keep(1 3)

* Some Housekeeping

mvdecode _all, ///
	mv(-1 -2 -3 -4 -7 -9 = .a \ -8 = .b)
keep if ivfio < 4 //only keep full

drop pid
rename pidp pid

xtset pid svyyear

lab var svyyear "Survey Year"

numlabel, add
compress
aorder

********************************************************************************
********************************************************************************

* Cleaning Up the Variables

lab def yesno 0 "no" 1 "yes"

** Parenthood
gen interviewdate = (svyyear - 1900) * 12 + doim
gen kids_any = 0 if fertility == 0
replace kids_any = 0 ///
	if childbirth1 > interviewdate & ~missing(childbirth1)
replace kids_any = 1 ///
	if childbirth1 <= interviewdate & ~missing(interviewdate)
replace kids_any = 1 ///
	if nchild > 0 & ~missing(nchild)
by pid (svyyear): replace kids_any = 1 ///
	if kids_any[_n-1] == 1 & missing(kids_any)

drop interviewdate fertility childbirth1 doim nchild last_int

** Recode Binary Variables
findname, all(inlist(@, . , .a, .b , 1, 2)) local(binary)
foreach var of local binary{
	recode `var' (2 = 0)
	lab val `var' yesno
	}

** Recode Order of Categories
recode opcls3 (5 = .)
recode trust (3 = 2) (2 = 3)
lab def rtrust  1 "most people can be trusted" ///
				2 "depends" ///
				3 "can't be too careful", modify
 
** Some Drops
drop hid ivfho ivfio memorig pno
order pid svyyear kids_any

********************************************************************************
********************************************************************************

save "$workdir/bhps", replace
shell rm temp_*
