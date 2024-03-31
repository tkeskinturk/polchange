
********************************************************************************
*** Cleaning the SOEP Data
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
	
use pid syear ///
	plj0046 plj0047 ///
	plh0192 plh0193 plh0194 plh0195 plh0196 ///
	plh0004 plh0007 ///
	using "$datadir/pl", clear

save "$workdir/temp_pl", replace   

********************************************************************************
********************************************************************************

* Administrative Information

** Timing
use pid syear pgmonth using "$datadir/pgen", clear	
save "$workdir/temp_pgen", replace

** PPATHL Backbone
use pid syear netto using "$datadir/ppathl", clear	
save "$workdir/temp_ppathl", replace

** Fertility
use pid kidgeb01 kidmon01 sumkids bioage bioyear biovalid ///
	using "$datadir/biobirth", clear	
save "$workdir/temp_biobirth", replace

* Merge Temp Files Into One `dta' File

use  "$workdir/temp_ppathl",clear

merge 1:1 pid syear using "$workdir/temp_pl" 		, nogen keep(3)
merge 1:1 pid syear using "$workdir/temp_pgen" 		, nogen keep(3)
merge m:1 pid using "$workdir/temp_biobirth" 		, nogen keep(1 3)

* Some Housekeeping

keep if netto >= 10 & netto <= 19 // only full interviews

gen svyyear = syear

numlabel, add
compress
aorder
sort pid syear
order pid syear

mvdecode _all, mv(-7 -6 -5 -4 -3 -1=. \ -2=.b \ -8=.c)

xtset pid svyyear
label language EN 	//switch to English labels

********************************************************************************
********************************************************************************

* Cleaning Up the Variables

lab def yesno 0 "no" 1 "yes"

** Parenthood
gen interviewdate = svyyear * 12 + pgmonth
gen childbirth1 = kidgeb01 * 12 + kidmon01
replace childbirth1 = kidgeb01 * 12 + 6 if missing(kidmon01)
gen kids_any = 0 if sumkids == 0
replace kids_any = 0 ///
	if childbirth1 >  interviewdate & ~missing(childbirth1)
replace kids_any = 1 ///
	if childbirth1 <= interviewdate & ~missing(interviewdate)
by pid (svyyear): replace kids_any = 1 ///
	if kids_any[_n-1] == 1 & missing(kids_any)
drop pgmonth interviewdate childbirth1 kidgeb01 kidmon01 sumkids ///
	 bioage bioyear biovalid

** Recode Binary Variables
findname, all(inlist(@, . , .a, .b , .c , 1, 2)) local(binary)
foreach var of local binary{
	recode `var' (2 = 0)
	lab val `var' yesno
	}
	
** Some Drops
drop syear netto

order pid svyyear kids_any

********************************************************************************
********************************************************************************

lab var svyyear "Survey year"
lab var plh0007 "Interest in politics"

save "$workdir/soep", replace
shell rm temp_*
