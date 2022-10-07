* Edie Espejo
* 2022-01-31
* 2022-03-13
* 2022-06-03
* 2022-10-06

clear
use "V:/Health and Retirement Study/edie/projects/smoking-cessation/rr-aim-2.dta"

drop if SUBPOP_SA!=1
* Fix the one person who had a missing value.
replace RACEETH=3 if RACEETH==.


tab AGE_CAT OUTCOME if SUBPOP_SA==1
tab AGE_CAT OUTCOME if SUBPOP==1
tabout OUTCOME if SUBPOP_SA==1 using "../tables/rr-count-outcomes-2.txt", format(0) replace
tabout OUTCOME if SUBPOP==1 using "../tables/rr-count-outcomes-1.txt", format(0) replace


* TABLE 1 ----------------------------------------------------------------------

table (var) (SUBPOP) [pweight=WGTR], statistic(frequency) statistic(mean AGE_1998) statistic(sd AGE_1998) statistic(fvrawfrequency AGE_CAT GENDER RACEETH MARRIED EDUCATION WEALTH_CAT LIVEALONE URBAN PAIN) statistic(fvpercent AGE_CAT GENDER RACEETH MARRIED EDUCATION WEALTH_CAT LIVEALONE URBAN PAIN) statistic(mean COMORBIDITIES_N) statistic(sd COMORBIDITIES_N) statistic(fvrawfrequency COMORBIDITIES_CAT) statistic(fvpercent COMORBIDITIES_CAT) statistic(fvrawfrequency ADL_CAT IADL_MONEY IADL_WALKSA SMOKE_CAT DRINK_CAT DEPRESSION COGFUNCTION) statistic(fvpercent ADL_CAT IADL_MONEY IADL_WALKSA SMOKE_CAT DRINK_CAT DEPRESSION COGFUNCTION) style(table-1) nformat(%6.2f mean sd) sformat("(%s)" sd) sformat("%s%%" fvpercent)


collect label list result, all

collect recode result fvrawfrequency=column1 fvpercent=column2 mean=column1 sd=column2

collect layout (var) (SUBPOP#result[column1 column2])

collect style cell var[AGE_CAT GENDER RACEETH MARRIED EDUCATION ADL_CAT SMOKE_CAT COMORBIDITIES_CAT LIVEALONE URBAN PAIN IADL_MONEY IADL_WALKSA WEALTH_CAT DRINK_CAT DEPRESSION COGFUNCTION]#result[column1], nformat(%6.0fc)
collect style cell var[AGE_CAT GENDER RACEETH MARRIED EDUCATION ADL_CAT SMOKE_CAT COMORBIDITIES_CAT LIVEALONE URBAN PAIN IADL_MONEY IADL_WALKSA WEALTH_CAT DRINK_CAT DEPRESSION COGFUNCTION]#result[column2], nformat(%6.1f) sformat("(%s%%)")

collect style cell var[AGE_1998 COMORBIDITIES_N]#result[column1], nformat(%6.1f)
collect style cell var[AGE_1998 COMORBIDITIES_N]#result[column2], nformat(%6.1f) sformat("(%s)")


collect preview

collect export "../tables/rr-smoking-cessation-table1.xlsx", sheet("table_1") replace

collect clear


* MODELING ---------------------------------------------------------------------

gen AGE_ORIGIN=55
global TABLE1VARS GENDER RACEETH EDUCATION WEALTH_CAT MARRIED LIVEALONE URBAN COMORBIDITIES_CAT DEPRESSION PAIN IADL_MONEY IADL_WALKSA ADL_CAT COGFUNCTION DRINK_CAT SMOKE_CAT

*stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)
*stcrreg i.AGE_CAT i.GENDER i.RACEETH i.EDUCATION i.WEALTH_CAT if SUBPOP, compete(OUTCOME==2)
*stcrreg i.AGE_CAT i.GENDER i.RACEETH if SUBPOP, compete(OUTCOME==2)


collect clear
stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)

* Single Predictor Models -----------------------------------------------------
foreach V of varlist $TABLE1VARS {
		collect _r_b _r_se _r_lb _r_ub _r_p: stcrreg i.`V' if SUBPOP, compete(OUTCOME==2)
}

collect style row split, dups(repeat)
collect layout (colname) (result)

collect export "../tables/rr-survival-estimates.xlsx", sheet(unadjusted) modify



* Saturated Model -------------------------------------------------------------

collect clear
stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)
collect _r_b _r_se _r_lb _r_ub _r_p: stcrreg i.($TABLE1VARS) if SUBPOP, compete(OUTCOME==2)

collect style row split, dups(repeat)
collect layout (colname) (result)

collect export "../tables/rr-survival-estimates.xlsx", sheet(saturated) modify



* Plots -----------------------------------------------------------------------

*gen AGE_ORIGIN=55
*global TABLE1VARS GENDER RACEETH EDUCATION WEALTH_CAT MARRIED LIVEALONE URBAN COMORBIDITIES_CAT DEPRESSION PAIN IADL_MONEY IADL_WALKSA ADL_CAT COGFUNCTION DRINK_CAT SMOKE_CAT

* Parametric plot
stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)
stcrreg i.($TABLE1VARS) if SUBPOP, compete(OUTCOME==2)

stcurve, cif at(SMOKE_CAT=(0 1 2)) outfile("V:/Health and Retirement Study/edie/projects/smoking-cessation/2022-10-06 data/rr-cif-stcurve.dta")

stcurve, cif outfile("V:/Health and Retirement Study/edie/projects/smoking-cessation/2022-10-06 data/rr-stcurve-overall.dta")


* Non-parametric plot
stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)
stcompet cif0=ci, compet1(2) by(SMOKE_CAT)

gen cif_cig0=cif0 if OUTCOME==1 & SMOKE_CAT==0 & SUBPOP==1
gen cif_cig1=cif0 if OUTCOME==1 & SMOKE_CAT==1 & SUBPOP==1
gen cif_cig2=cif0 if OUTCOME==1 & SMOKE_CAT==2 & SUBPOP==1

twoway line cif_cig* _t, connect(step step) sort

outsheet using "V:\Health and Retirement Study\edie\projects\smoking-cessation\2022-10-06 data\rr-cif-stcompet-data.csv", comma nolabel



* Non-parametric plot overall
stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)
stcompet cif1=ci, compet1(2)

gen cif_overall=cif1 if OUTCOME==1 & (SMOKE_CAT==1 | SMOKE_CAT==2 | SMOKE_CAT==0) & SUBPOP==1
twoway line cif_overall _t, sort

outsheet using "V:\Health and Retirement Study\edie\projects\smoking-cessation\2022-10-06 data\rr-cif-stcompet-overall.csv", comma nolabel




* MICE -------------------------------------------------------------------------
mi set mlong
mi register imputed DEPRESSION
mi misstable summarize, all

mi impute logit DEPRESSION AGE_1998 GENDER RACEETH ATOTA, add(5) rseed(1)

*gen AGE_ORIGIN=55
mi stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)



mi estimate: stcrreg i.($TABLE1VARS), compete(OUTCOME==2)

collect get _r_b _r_se _r_lb _r_ub

collect style row split, dups(repeat)
collect layout (colname) (result)

collect export "../tables/rr-survival-estimates.xlsx", sheet(mice) modify













* Summary Statistics ----------------------------------------------------------

clear
use "V:/Health and Retirement Study/edie/projects/smoking-cessation/rr-aim-2.dta"
tabstat AGE_1998 ATOTA if SUBPOP, stats(n mean sd min p25 median p75 max) columns(statistics) save
mat T = r(StatTotal)'

* Table 1 ----------------------------------------------------------------------
svyset SECU [pweight=WGTR], strata(STRATUM) singleunit(centered)

tab AGE_CAT if SUBPOP
svy, subpop(SUBPOP): tab AGE_CAT

tab GENDER if SUBPOP
svy, subpop(SUBPOP): tab GENDER

tab RACEETH if SUBPOP
svy, subpop(SUBPOP): tab RACEETH

* Models -----------------------------------------------------------------------
tab OUTCOME if SUBPOP
tab OUTCOME if SUBPOP_SA

gen AGE_ORIGIN=55
stset AGE_OUTCOME [pweight=WGTR], failure(OUTCOME==1) origin(AGE_ORIGIN) enter(AGE_1998)



stcrreg i.AGE_CAT i.GENDER i.RACEETH i.EDUCATION i.WEALTH_CAT if SUBPOP, compete(OUTCOME==2)

stcrreg i.AGE_CAT i.GENDER i.RACEETH if SUBPOP, compete(OUTCOME==2)