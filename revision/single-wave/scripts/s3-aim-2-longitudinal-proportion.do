clear
use "V:/Health and Retirement Study/edie/projects/smoking-cessation/rr-aim-2-long-v2.dta"

svyset SECU [pweight=WGTR], strata(STRATUM) singleunit(centered)
*svy: tab YEAR Group, row format(%10.3g)

tabout YEAR Group using "../tables/rr-cohort-plot-proportion-1.xls", svy cells(row) format(4) style(xlsx) replace
tabout YEAR Group if AGE_1998<65 using "../tables/rr-cohort-plot-proportion-2.xls", svy cells(row) format(4) replace


*tab YEAR Group if AGE_1998<65