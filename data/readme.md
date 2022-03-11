# Global evidence on the selfish rich inequality hypothesis

Data files to support the paper **Global evidence on the selfish rich inequality hypothesis** by Ingvild Almås, Alexander W. Cappelen,
Erik Ø. Sørensen and Bertil Tungodden.

## The data files

There are five different data files relevant for partial or complete replication.

1. `BSRI_external_data.{dta,csv}` Aggregate data taken from sources outside Gallup World Poll.
2. `WP_selfishness_public.{dta,csv}` Gallup World Poll 2018 data -- publicly available subset.
3. `WP_selfishness_confidential.rds` Gallup World Poll 2018, the full dataset. Not publicly available per 
   Gallup policy, without licensing,but "Access to the World Poll Respondent Level Data will be granted to individuals
representing academic review boards where necessary for the purposes of
verification of research undertaken using the Client commissioned items and
World Poll Respondent Level Data. All reasonable alternatives must first be
exhausted with the reviewing body, such as the sharing of topline data. The
researcher and Client’s Project Manager must notify Gallup in writing of such a
requirement including a copy of the correspondence from the reviewing body.
Upon such a request Gallup will grant the reviewer access to the data for a
limited duration for the sole purpose of review." There is an intermediate possibility of getting access to 
a slightly larger set of background variables (WP12259, WP1230, HHSIZE, EMP_2010, WP4657, and WP119) upon
request and agreement to restricted use.
4. `Durante2017.csv` Data from Durante et al (2017) with added manual coding by us of whether the rich are colder than the poor in relevant sample.
5. `WP_countryids.csv` Coding from World Poll numbering of countries to ISO-3 countrycodes.

Below is a full description of the variables used in the different subsets. 


## Variables in external data, `BSRI_external_data`
 
- iso_a3: Three-letter country codes defined in ISO 3166-1.
- GNI: World Development Indicators for Gross National Income per capita (source: The World Bank). Numbers are given in purchasing power parity units, ($2011). We use the last year available for each country:
  - 2018: Argentina, Australia, Bangladesh, Bolivia, Brazil, Canada, Chile, China, Cameroon, Colombia, Czech Republic, Germany, Ecuador, Egypt, Spain, France, United Kingdom, Croatia, Hungary, Indonesia, India, Israel, Italy, Jordan, Kazakhstan, Kenya, Cambodia, South Korea, Sri Lanka, Morocco, Mexico, Nigeria, Netherlands, Norway, Pakistan, Peru, Philippines, Portugal, Russia, Rwanda, Thailand, Turkey, Uganda, Ukraine, United States, Vietnam, South Africa, Zimbabwe
  - 2017: Algeria, Estonia, Greece, Malawi, Tanzania, Iran, Japan, Venezuela        
  - 2016: Switzerland  
  - 2010: Afghanistan, Ethiopia, Zambia
- WYD: World Income Distribution (Milanovic and Yitzhaki, 2001). We use last year available for each country:
    - 2012: Tanzania, China, 
    - 2011: Iran, Czech Republic, Ethiopia, Rwanda, Japan, France, Turkey, Hungary, India, 
    - 2010: Bangladesh, Cambodia, Indonesia, Zambia, Jordan, Egypt, Uganda, Malawi, Nigeria
    - 2009: Thailand, Sri Lanka, Philippines  
    - 2008: Switzerland, Afghanistan, South Korea, United States, Pakistan, South Africa, Canada, Israel
    - 2007: Cameroon, Kenya, Morocco
    - 2006: Colombia, Chile, Venezuela, Vietnam, Ecuador
    - 2005: Netherlands, Spain, Germany, Portugal, Russia, Bolivia, Peru, Mexico, Kazakhstan, Argentina, Ukraine, Brazil, Greece, 
    - 2004: Norway, United Kingdom, Italy, Croatia, Estonia
    - 2003: Australia 
    - 1995: Zimbabwe, Algeria
- Schooling: Mean years of schooling for each country reported in Barro and Lee (2013) for 2010 (all countries except Ethiopia and Nigeria) 
- Six governance indicators from the Worldwide Governance Indicators of the World Bank ---
  - corruption: negative of the indicator "Control of corruption"
  - government_ineff: negative of "Government Effectiveness"
  - instability: negative of "Political Stability and Absence of Violence/Terrorism"
  - poor_reg: negative of "Regulatory Quality"
  - law: negative of "Rule of Law"
  - voice_acc: negative of "Voice and accountability"
- shadow_ec:  Size of the shadow economy as percentage of GDP, 2015, as measured by : Medina and Schneider (2018), extracted from https://www.theglobaleconomy.com/rankings/shadow_economy/ 
- org_crime: Organized crime from the Quality of the government database. Scale 1-7. Question about to what extent organized crime impose costs on businesses. [1 = to a great extent-imposes huge costs; 7 = not at all-imposes no costs]. Original sources: World Economic Forum, Executive Opinion Survey.
- HDI: United Nations Development Program, Human Development Index. Data from 2017 extracted from Human Development Index 2020 Statistical Annex, Table 2. "Human Development Index trends, 1990-2019".
- gpi_gpi: Global Peace Index 2018. From Global Peace Index 2018: Measuring Peace in a Complex World. Scale 1-5 with 5 being the highest level of conflict.

## Variables taken from Gallup World Poll 2018, `WP_selfishness_public` and `WP_selfishness_confidential`
All variables included in `WP_selfishness_confidential`. 
An asterisk indicator indicates that the variable is also included in the publicly available subset `WP_selfishness_public`.

- WPID_RANDOM*: identifier allowing linking to licensed Gallu World Poll data.
- iso_a3*: Three-letter country codes defined in ISO 3166-1.
- wgt*: Population weight
- more_selfish*: "Do you generally agree, disagree, or neither agree nor disagree with
 this statement: "In (country), one of the main reasons for the rich being
 richer than the poor is that the rich have been more selfish in life
 than the poor." Coded from 1: Disagree strongly,
 through 3: Neither agree nor disagree to 5: Agree strongly. From "Fairness Across the World" module.
 - more_criminal*: "Do you generally agree, disagree, or neither agree nor disagree with
this statement: In Norway, one of the main reasons for the rich being
richer than the poor is that the rich have been more involved in
illegal activities than the poor. Coded from 1: Disagree strongly,
 through 3: Neither agree nor disagree to 5: Agree strongly. From "Fairness Across the World" module.
- inequality_unfair*: "Do you generally agree, disagree, or neither agree nor disagree with
this statement: In Norway, the economic differences between the rich
and poor are unfair."  Coded from 1: Disagree strongly,
 through 3: Neither agree nor disagree to 5: Agree strongly. From "Fairness Across the World" module.
- gov_should_reduce_inequality*: "Do you generally agree, disagree, or neither agree nor disagree with
this statement: In Norway, the national government should aim to
reduce the economic differences between the rich and the poor." 
- WP1219*: Gender of respondent. Coded 1: male, 2: female. 
- WP1220*: Age. Coded as years, but topcoded at 99. 100: refused.
- WP3117*: Education. Coded 1: Completed elementary education or less, 2: Secondary - 3 year Tertiary secondary education, 3: Completed four years of education beyond secondary education. 4: Don't know, 5: refused.
- income_2*: Annual household income in international dollars.
- WP14*: Urban rural. Coded 1: A rural area or on a farm, 2:  A small town or village, 3: A large city, 4: Don't know, 5: Refused, 6:  A suburb of a large city.
- psuid: Unique identifier of primary sampling unit. Codes phone interviews as singleton psu's. Based on Gallup WP12259.
- d_male: Convenience 0/1 indicator recoding of WP1219.
- z_age: Globally standardized version of age (WP1220).
- d_married: Convenience 0/1 indicator for an individual being married (recoded from Gallup WP1223).
- z_n_children: Globally standardized measure of number of children in household.
- d_midedu: Convenience 0/1 indicator for medium education. Based on WP3117.
- d_highedu: Convenience 0/1 indicator for high education. Based on WP3117.
- d_immigrant: Convenience 0/1 indicator for respondent being an immigrant, based on Gallup WP4657.
- d_working: Convenience 0/1 indicator for the respondent being employed. Based on Gallup EMP_2010.
- d_religionimp: "Is religion an important part of your daily life? (yes/no) " (WP119). Convenience 0/1 indicator for "yes".
- d_urban: Convenience 0/1 indicator for WP14 either 2,3, or 6.
- HHsize: Household size.
- inc_oecd1: Convenience measure: Adult equivalent household income (income_2 divided by square root of HHsize).
- inc_rank_oecd1: Convenience measure: Rank of income within country.
- corruption_business: Convenience recoding of Gallup WP145: "Is corruption widespread within businesses located in (country), or not? (yes/no)". Coded 0/1, 1 for yes.
- corruption_government: Convenience recoding of Gallup WP146: "Is corruption widespread throughout the government in (country), or not?
 (yes/no)". Coded 0/1, 1 for yes.
- donated_money: Convenience 0/1 recoding of Gallup WP108: "Have you done any of the following in the past month? How about *Donated Money*. Coded 0/1, 1 for yes.
- get_ahead: Convenience 0/1 recoding of Gallup WP128: "Can people in this country get ahead by working hard, or not? (yes/no)". Coded 0/1, 1 for yes.

## Variables in Durante2017.csv

- country: Name of country
- sample_comment: If there is a particular subset of the national data (string).
- year: Year of data collection.
- WC_correlation: Reported warmth/competence correlation in Durante (2017).
- Rich_colder: A yes/no coding of whether the rich are colder than the poor (coded by visual inspection of scatter graphs).
- SR_comment: Our comment on the visual coding of `Rich_colder`. If any substitution of literal "rich" and "poor" groups was necessary, or if we could not find relevant comparison groups (or the scatter graphs themselves).

## Variables in WP_countryids.csv

- WP5: Gallup numbering of country
- countrynew: Gallup name of country
- iso_a3: Three-letter country codes defined in ISO 3166-1.
