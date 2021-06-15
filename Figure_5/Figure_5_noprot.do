
use "../Data/all_protests.dta", clear

sort city t 

lab var lcoef_var1 "Coef. Variation (1)"
lab var lcoef_var2 "Coef. Variation (2)"
lab var lcoef_var3 "Coef. Variation (3)"
lab var lcoef_var4 "Coef. Variation (4)"
lab var lcoef_var5 "Coef. Variation (5)"
lab var lcoef_var24 "Coef. Variation (24)"
lab var lcoef_var48 "Coef. Variation (48)"
lab var lcoef_var72 "Coef. Variation (72)"
lab var lcoef_var96 "Coef. Variation (96)"
lab var lcoef_var120 "Coef. Variation (120)"

encode city, gen(cities)

keep if type==0

foreach v of varlist lcoef_var1-lcoef_var168 {
  replace `v' = `v' / 100
}

reg protest lcoef_var1 lcoef_var2  lcoef_var3  lcoef_var4  lcoef_var5 ///
	coef_var24 lcoef_var48 lcoef_var72 lcoef_var96 lcoef_var120 i.cities
estimates save "../Data/no_protest_est", replace

predict pred

keep cities t pred

reshape wide pred, i(t) j(cities) 

saveold "../Data/pred_noprot", replace







