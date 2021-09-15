clear all
cap log close
log using lasso_between.log, text replace
import delimited using SR_df_attitude_c.csv, delim(",") case(lower)


local xlist = "zln_gni zinequality zmeanage zschooling zcorruption"

local i=0
foreach x1 in `xlist' {
	local j=0
	gen xx_`i' = `x1'
	foreach x2 in `xlist' {
		if `j'>=`i' {
			gen xx_`i'_`j' = `x1'*`x2'
		}
		local j=`j'+1
	}
	local i=`i'+1
}

xporegress inequality_unfair mean_bs, cont( xx* ) rseed(4132412)
xporegress gov_should_reduce_inequality mean_bs, cont( xx* ) rseed (912438)


log close
