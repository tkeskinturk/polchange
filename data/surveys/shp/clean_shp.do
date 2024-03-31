
********************************************************************************
*** Cleaning the SHP Data
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
local ws 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21
local ys 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
foreach w of local ws {
	gettoken y ys:ys
	use idpers ///
		idhous`w' /// 
		using "$datadir/shp`w'_p_user", clear   
	rename *`w' *
	gen svyyear=`y'
	sort idpers
	save "$workdir/temp_shp`w'_p1", replace
	}
foreach w in 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 {
	append using "$workdir/temp_shp`w'_p1"
	}
sort idpers svyyear
save "$workdir/temp_shp_p1", replace

/// Group 02
local ws 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21
local ys 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
foreach w of local ws {
	gettoken y ys:ys
	use idpers ///
		p`w'p01 p`w'p10 ///
		using "$datadir/shp`w'_p_user", clear   
	rename p`w'* p*
	gen svyyear=`y'
	sort idpers
	save "$workdir/temp_shp`w'_p2", replace
	}
foreach w in 99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 {
	append using "$workdir/temp_shp`w'_p2"
	}
sort idpers svyyear
save "$workdir/temp_shp_p2", replace

/// Group 03
local ws 11 14 17
local ys 2011 2014 2017 
foreach w of local ws {
	gettoken y ys:ys
	use idpers ///
		p`w'p53 p`w'p54 p`w'p55 p`w'p56 p`w'p57 p`w'p58 ///
		p`w'p59 p`w'p60 p`w'p61 p`w'p62 p`w'p63 ///
		using "$datadir/shp`w'_p_user", clear 
	rename p`w'* p*
	gen svyyear=`y'
	sort idpers
	save "$workdir/temp_shp`w'_p3", replace
	}
foreach w in 11 14 {
	append using "$workdir/temp_shp`w'_p3"
	}
sort idpers svyyear
save "$workdir/temp_shp_p3", replace

/// Group 04
local ws 99 00 01 02 03 04 05 06 07 08 09 11 14 17 20
local ys 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2011 2014 2017 2020
foreach w of local ws {
	gettoken y ys:ys
	use idpers ///
		p`w'p02 p`w'p03 p`w'p04 /// 
		p`w'p13 p`w'p15 p`w'p16 p`w'p17 p`w'p18 ///
		using "$datadir/shp`w'_p_user", clear   
	rename p`w'* p*
	gen svyyear=`y'
	sort idpers
	save "$workdir/temp_shp`w'_p4", replace
	}
foreach w in 99 00 01 02 03 04 05 06 07 08 09 11 14 17 {
	append using "$workdir/temp_shp`w'_p4"
	}
sort idpers svyyear
save "$workdir/temp_shp_p4", replace

/// Group 05
local ws 99 00 01 02 03 04 05 06 07 08 11 14 17 20
local ys 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2011 2014 2017 2020
foreach w of local ws {
	gettoken y ys:ys
	use idpers p`w'p12 ///
		using "$datadir/shp`w'_p_user", clear   
	rename p`w'* p*
	gen svyyear=`y'
	sort idpers
	save "$workdir/temp_shp`w'_p5", replace
	}
foreach w in 99 00 01 02 03 04 05 06 07 08 11 14 17 {
	append using "$workdir/temp_shp`w'_p5"
	}
sort idpers svyyear
save "$workdir/temp_shp_p5", replace

/// Group 06
local ws 00 01 02 03 04 05 06 07 08 09 11 14 17 20
local ys 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2011 2014 2017 2020
foreach w of local ws {
	gettoken y ys:ys
	use idpers ///
		p`w'p20 p`w'p22 ///
		using "$datadir/shp`w'_p_user", clear 
	rename p`w'* p*
	gen svyyear=`y'
	sort idpers
	save "$workdir/temp_shp`w'_p6", replace
	}
foreach w in 00 01 02 03 04 05 06 07 08 09 11 14 17 {
	append using "$workdir/temp_shp`w'_p6"
	}
sort idpers svyyear
save "$workdir/temp_shp_p6", replace

/// Group 07
local ws 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21
local ys 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
foreach w of local ws {
	gettoken y ys:ys
	use idpers p`w'p45 ///
		using "$datadir/shp`w'_p_user", clear   
	rename p`w'* p*
	gen svyyear=`y'
	sort idpers
	save "$workdir/temp_shp`w'_p7", replace
	}
foreach w in 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 {
	append using "$workdir/temp_shp`w'_p7"
	}
sort idpers svyyear
save "$workdir/temp_shp_p7", replace

********************************************************************************
********************************************************************************

* Merge Temp Files Into One `dta' File

use /// 
	idpers idhous* status* rnpx* using "$datadir/shp_mp", clear

local year = 1999
foreach wave in ///
	99 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 {
	renvars idhous`wave' status`wave' rnpx`wave', postsub(`wave')
	renvars idhous status rnpx, postfix(`year')
	local year=`year'+1
	}

reshape long idhous status rnpx, i(idpers) j(svyyear)		

lab var idhous "Identification Number of Household"

*merge temp files
forvalues i=1/7 {
	merge 1:1 idpers svyyear using "$workdir/temp_shp_p`i'", nogen keep(1 3)
	}

* Administrative Information

keep if rnpx == 0 // only full interviews

lab var svyyear "Survey Year"
rename idpers pid
rename idhous hid

mvdecode _all, mv(-1=.a \ -2=.b \ -3=.c \ -4 -5 -6=.d \ -7=.e \ -8=.f)

numlabel, add
compress
order _all, alphabetic
order pid svyyear status rnpx

xtset pid svyyear

drop if missing(pid)

********************************************************************************
********************************************************************************

* Cleaning Up the Variables

lab def yesno 0 "no" 1 "yes"

** Recode Binary Variables
findname, all(inlist(@, . , .a, .b , .c , 1, 2)) local(binary)
foreach var of local binary{
	recode `var' (2 = 0)
	lab val `var' yesno
	}

** Some Drops
drop status rnpx statuscovid hid

********************************************************************************
********************************************************************************

save "$workdir/shp", replace
shell rm temp_*
