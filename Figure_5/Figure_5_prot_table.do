
estimates use "../Data/protest_est"
eststo: regress

estimates use "../Data/no_protest_est"
eststo: regress


** TABLE

esttab using "../Figure_5/tableA1.tex", replace f alignment(S) ///
	label booktabs b(3) p(3) eqlabels(none) drop(*cities*) ///
	star(* 0.1 ** 0.05 *** 0.01) collabels("") ///
	cells("b(fmt(3)star)" "se(fmt(3)par)") nonumbers mtitles("Protest" "No Protest") ///
	stats(N, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") ///
	labels(`"City Fixed Effects"' `"Observations"'))

	
