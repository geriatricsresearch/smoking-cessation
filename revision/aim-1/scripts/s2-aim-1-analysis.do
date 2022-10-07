* Edie Espejo
* 2022-01-16
* 2022-03-16
* 2022-10-07

clear
use "V:\Health and Retirement Study\edie\projects\smoking-cessation\aim-1.dta"
svyset SECU [pweight=WGTR], strata(STRATUM) singleunit(centered)

gen AGE_CAT=.
replace AGE_CAT=1 if AGE<65
replace AGE_CAT=2 if AGE>64 & AGE<75
replace AGE_CAT=3 if AGE>74 & AGE<85
replace AGE_CAT=4 if AGE>84

tab YEAR if SUBPOP, matcell(freq) matrow(names)
putexcel set "../tables/rr-aim1-sample-size.xlsx", sheet(overall) replace
putexcel A1=("Year") B1=("n")
putexcel A2=matrix(names) B2=matrix(freq)

tab YEAR AGE_CAT if SUBPOP, matcell(freq) matrow(names)
putexcel set "../tables/rr-aim1-sample-size.xlsx", sheet(categorical_age) modify
putexcel A1=("Year") B1=("<65") C1=("65 to 74") D1=("75 to 84") E1=("85+")
putexcel A2=matrix(names) B2=matrix(freq)

* Collect Tables ---------------------------------------------------------------
collect clear
svy, subpop(SUBPOP): logit SMOKE i.YEAR i.AGE_CAT
collect _r_b _r_se _r_lb _r_ub _r_p: margins i.YEAR, subpop(SUBPOP)
collect layout (colname) (result)
collect export "../tables/rr-prevalence-estimates.xlsx", sheet(categorical_age) modify

* Prevalence -------------------------------------------------------------------
svy: logit SMOKE i.YEAR i.AGE_CAT if SUBPOP==1
log using "../tables/rr-catMarginsAge.log"
margins AGE_CAT#i.YEAR
log close


svy: logit SMOKE i.YEAR i.GENDER if SUBPOP==1
log using "../tables/rr-catMarginsGender.log"
margins i.GENDER#i.YEAR
log close