
********************************************************************************
*** Cleaning the UKHLS Data
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
foreach w in b c f i {
	local waveno = strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'_demorient ///
		using "$datadir/`w'_indresp", clear
	rename `w'_* * 
	gen wave = `waveno'
	save "$workdir/temp_`w'_indresp1", replace
	}
foreach w in b c f {
	append using "$workdir/temp_`w'_indresp1"
	}
save "$workdir/temp_indresp1", replace

/// Group 02
foreach w in a d j {
	local waveno = strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'_scenv_ftst `w'_scenv_grn ///
		using "$datadir/`w'_indresp", clear
	rename `w'_* * 
	gen wave = `waveno'
	save "$workdir/temp_`w'_indresp2", replace
	}
foreach w in a d {
	append using "$workdir/temp_`w'_indresp2"
	}
save "$workdir/temp_indresp2", replace

/// Group 03
foreach w in b d j {
	local waveno = strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'_scopfamf ///
		using "$datadir/`w'_indresp", clear
	rename `w'_* * 
	gen wave = `waveno'
	save "$workdir/temp_`w'_indresp3", replace
	}
foreach w in b d {
	append using "$workdir/temp_`w'_indresp3"
	}
save "$workdir/temp_indresp3", replace

/// Group 04
foreach w in a b c d e f g i j {
	local waveno = strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp ///
		`w'_vote6  ///
		using "$datadir/`w'_indresp", clear
	rename `w'_* * 
	gen wave = `waveno'
	save "$workdir/temp_`w'_indresp4", replace
	}
foreach w in a b c d e f g i {
	append using "$workdir/temp_`w'_indresp4"
	}
save "$workdir/temp_indresp4", replace

/// Group 05 (Survey Year)
foreach w in a b c d e f g h i j k {
	local waveno = strpos("abcdefghijklmnopqrstuvwxyz", "`w'")
	use pidp `w'_intdaty_dv /// 
		using "$datadir/`w'_indresp", clear
	rename `w'_* * 
	gen wave = `waveno'
	save "$workdir/temp_`w'_indresp5", replace
	}
foreach w in a b c d e f g h i j {
	append using "$workdir/temp_`w'_indresp5"
	}
save "$workdir/temp_indresp5", replace

********************************************************************************
********************************************************************************

* Administrative Information

** Waves
use pidp using "$datadir/xwavedat", clear
save "$workdir/temp_xwavedat", replace

** Some Basic Information
use pidp ?_ivfho ?_hidp ?_ivfio ?_pno using "$datadir/xwaveid", clear

foreach w in a b c d e f g h i j k {
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz", "`w'")		
	rename `w'_* *`waveno'
	}

reshape long ivfho hidp ivfio pno, i(pidp) j(wave)

lab var hidp "Household Identification Number"

* Merge Temp Files Into One `dta' File

forvalues i = 1 / 5 {
	merge 1:1 pidp wave using "$workdir/temp_indresp`i'", nogen keep(1 3)
	}
merge m:1 pidp using "$workdir/temp_xwavedat", nogen keep(1 3)

* Some Housekeeping

keep if ivfio == 1
rename intdaty_dv svyyear
drop if svyyear == 2021
rename wave ukhls_wave
lab var svyyear "Survey Year"
rename pidp pid
rename hidp hid

mvdecode _all, ///
	mv(-1=.a \ -2=.b \ -7=.c \ -8=.d \ -9=.e \ -10 -11 -20 =.f  \ -21 = .g )

numlabel, add
compress
drop if missing(pid)
xtset pid ukhls_wave

********************************************************************************
********************************************************************************

* Cleaning Up the Variables

lab def yesno 0 "no" 1 "yes"

** Recode Binary Variables
findname, all(inlist(@, . , .a, .b , .c , .d, .e , .f , 1, 2)) local(binary)
foreach var of local binary{
	recode `var' (2 = 0)
	lab val `var' yesno
	}
order _all, alphabetic
order pid hid svyyear ukhls_wave

** Some Drops
drop hid ivfho ivfio pno l_hidp l_ivfho l_ivfio l_pno

********************************************************************************
********************************************************************************

save "$workdir/ukhls", replace
shell rm temp_*
