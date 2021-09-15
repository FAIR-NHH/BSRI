cap log close
log using lasso_within.log, text replace 

use WPsr2, clear
keep inequality_unfair gov_should_reduce_inequality more_selfish /// 	
  inc_rank_oecd1 binary_education_high d_female z_age d_married ///
  z_n_children d_immigrant d_working d_urban iso_a3 psuid

egen country = group(iso_a3)
local xlist = "inc_rank_oecd1 binary_education_high d_female z_age d_married z_n_children d_immigrant d_working d_urban"

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

// Adding vce(cluster psuid) fails with r(134), "too many values"
xporegress inequality_unfair more_selfish, cont((i.country) xx* )  rseed(4132412)
xporegress gov_should_reduce_inequality more_selfish, cont((i.country) xx* )  rseed (912438)
log close
