The program files
=================

The program files are available on github:
`git@github.com:FAIR-NHH/BSRI.git`

The data files
==============

The analysis files relies on four different data files.

1.  `BSRI_external_data.{dta,csv}` Aggregate data taken from sources
    outside Gallup World Poll.
2.  `WP_selfishness_public.{dta,csv}` Gallup World Poll 2018 data --
    publicly available subset.
3.  `WP_selfishness_confidential.rds` Gallup World Poll 2018, the full
    dataset. Not publicly available per Gallup policy, but "Access to
    the World Poll Respondent Level Data will be granted to individuals
    representing academic review boards where necessary for the purposes
    of verification of research undertaken using the Client commissioned
    items and World Poll Respondent Level Data. All reasonable
    alternatives must first be exhausted with the reviewing body, such
    as the sharing of topline data. The researcher and Client's Project
    Manager must notify Gallup in writing of such a requirement
    including a copy of the correspondence from the reviewing body. Upon
    such a request Gallup will grant the reviewer access to the data for
    a limited duration for the sole purpose of review."
4.  `Durante2017.csv` Data from Durante et al (2017) with added manual
    coding by us of whether the rich are colder than the poor in
    relevant sample.

Below is a full description of the variables used in the different
subsets.

Variables in external data, `BSRI_external_data`
================================================

-   iso\_a3: The United Nations ISO 3 alpha country code
-   gpi\_gpi: Global Peace Index (2018), with values 1--5: 5 being the
    least peaceful.
-   GNI: World Development Indicators for Gross National Income per
    capita (source: The World Bank). Numbers are given in purchasing
    power parity units, (\$2011). We use the last year available for
    each country:
    -   2018: Argentina, Australia, Bangladesh, Bolivia, Brazil, Canada,
        Chile, China, Cameroon, Colombia, Czech Republic, Germany,
        Ecuador, Egypt, Spain, France, United Kingdom, Croatia, Hungary,
        Indonesia, India, Israel, Italy, Jordan, Kazakhstan, Kenya,
        Cambodia, South Korea, Sri Lanka, Morocco, Mexico, Nigeria,
        Netherlands, Norway, Pakistan, Peru, Philippines, Portugal,
        Russia, Rwanda, Thailand, Turkey, Uganda, Ukraine, United
        States, Vietnam, South Africa, Zimbabwe
    -   2017: Algeria, Estonia, Greece, Malawi, Tanzania, Iran, Japan,
        Venezuela\
    -   2016: Switzerland\
    -   2010: Afghanistan, Ethiopia, Zambia
-   WYD: World Income Distribution (Milanovic and Yitzhaki, 2001). We
    use last year available for each country:
    -   2012: Tanzania, China,
    -   2011: Iran, Czech Republic, Ethiopia, Rwanda, Japan, France,
        Turkey, Hungary, India,
    -   2010: Bangladesh, Cambodia, Indonesia, Zambia, Jordan, Egypt,
        Uganda, Malawi, Nigeria
    -   2009: Thailand, Sri Lanka, Philippines\
    -   2008: Switzerland, Afghanistan, South Korea, United States,
        Pakistan, South Africa, Canada, Israel
    -   2007: Cameroon, Kenya, Morocco
    -   2006: Colombia, Chile, Venezuela, Vietnam, Ecuador
    -   2005: Netherlands, Spain, Germany, Portugal, Russia, Bolivia,
        Peru, Mexico, Kazakhstan, Argentina, Ukraine, Brazil, Greece,
    -   2004: Norway, United Kingdom, Italy, Croatia, Estonia
    -   2003: Australia
    -   1995: Zimbabwe, Algeria
-   Schooling: Mean years of schooling for each country reported in
    Barro and Lee (2013) for 2010 (all countries except Ethiopia and
    Nigeria)
-   Six governance indicators from the Worldwide Governance Indicators
    of the World Bank ---
    -   corruption: negative of the indicator "Control of corruption"
    -   government\_ineff: negative of "Government Effectiveness"
    -   instability: negative of "Political Stability and Absence of
        Violence/Terrorism"
    -   poor\_reg: negative of "Regulatory Quality"
    -   law: negative of "Rule of Law"
    -   voice\_acc: negative of "Voice and accountability"
-   shadow\_ec: Size of the shadow economy as percentage of GDP, 2015,
    as measured by : Medina and Schneider (2018), extracted from
    https://www.theglobaleconomy.com/rankings/shadow\_economy/
-   org\_crime: Organized crime from the Quality of the goverment
    database. Scale 1-7. Question about to what extent organized crime
    impose costs on businesses. \[1 = to a great extent-imposes huge
    costs; 7 = not at all-imposes no costs\]. Original sources: World
    Economic Forum, Executive Opinion Survey.
-   HDI: United Nations Development Program, Human Development Index.
    Data from 2017 extracted from Human Development Index 2020
    Statistical Annex, Table 2. "Human Development Index trends,
    1990-2019".
-   gpi\_gpi: Global Peace Index 2018. From Global Peace Index 2018:
    Measuring Peace in a Complex World. Scale 1-5 with 5 being the
    highest level of conflict.

Variables taken from Gallup World Poll 2018
===========================================

A asterisk indicator indicates that the variable is also included in the
publicly available subset

-   iso\_a3\*: The United Nations ISO 3 alpha country code
-   wgt\*: Population weight
-   more\_selfish\*: "Do you generally agree, disagree, or neither agree
    nor disagree with this statement:"In (country), one of the main
    reasons for the rich being richer than the poor is that the rich
    have been more selfish in life than the poor.\" Coded from 1:
    Disagree strongly, through 3: Neither agree nor disagree to 5: Agree
    strongly. From "Fairness Across the World" module.
-   more\_criminal\*: "Do you generally agree, disagree, or neither
    agree nor disagree with this statement: In Norway, one of the main
    reasons for the rich being richer than the poor is that the rich
    have been more involved in illegal activities than the poor. Coded
    from 1: Disagree strongly, through 3: Neither agree nor disagree to
    5: Agree strongly. From"Fairness Across the World\" module.
-   inequality\_unfair\*: "Do you generally agree, disagree, or neither
    agree nor disagree with this statement: In Norway, the economic
    differences between the rich and poor are unfair." Coded from 1:
    Disagree strongly, through 3: Neither agree nor disagree to 5: Agree
    strongly. From "Fairness Across the World" module.
-   gov\_should\_reduce\_inequality\*: "Do you generally agree,
    disagree, or neither agree nor disagree with this statement: In
    Norway, the national government should aim to reduce the economic
    differences between the rich and the poor."
-   WP1219\*: Gender of respondent. Coded 1: mle, 2: female.
-   WP1220\*: Age. Coded as years, but topcoded at 99. 100: refused.
-   WP3117\*: Education. Coded 1: Completed elementary education or
    less, 2: Secondary - 3 year Tertiary secondary education, 3:
    Completed four years of education beyond secondary education. 4:
    Don't know, 5: refused.
-   income\_2\*: Annual household income in international dollars.
-   WP14\*: Urban rural. Coded 1: A rural area or on a farm, 2: A small
    town or village, 3: A large city, 4: Don't know, 5: Refused, 6: A
    suburb of a large city.
-   psuid: Unique identifier of primary sampling unit. Codes phone
    interviews as singleton psu's. Based on Gallup WP12259.
-   d\_male: Convenience 0/1 indicator recoding of WP1219.
-   z\_age: Globally standardized version of age (WP1220).
-   d\_married: Convenience 0/1 indicator for an individual being
    married (recoded from Gallup WP1223).
-   z\_n\_children: Globally standardized measure of number of children
    in household.
-   d\_midedu: Convenience 0/1 indicator for medium education. Based on
    WP3117.
-   d\_highedu: Convenience 0/1 indicator for high education. Based on
    WP3117.
-   d\_immigrant: Convenience 0/1 indicator for respondent being an
    immigrant, based on Gallup WP4657.
-   d\_working: Convenience 0/1 indicator for the respondent being
    employed. Based on Gallup EMP\_2010.
-   d\_religionimp: "Is religion an important part of your daily life?
    (yes/no)" (WP119). Convenience 0/1 indicator for "yes".
-   d\_urban: Convenience 0/1 indicator for WP14 either 2,3, or 6.
-   HHsize: Household size.
-   inc\_oecd1: Convenience measure: Adult equivalent household income
    (income\_2 divided by square root of HHsize).
-   inc\_rank\_oecd1: Convenience measure: Rank of income within
    country.
-   corruption\_business: Convenience recoding of Gallup WP145: "Is
    corruption widespread within businesses located in (country), or
    not? (yes/no)". Coded 0/1, 1 for yes.
-   corruption\_government: Convenience recoding of Gallup WP146: "Is
    corruption widespread throughout the government in (country), or
    not? (yes/no)". Coded 0/1, 1 for yes.
-   donated\_money: Convenience 0/1 recoding of Gallup WP108: \"Have you
    done any of the following in the past month? How about *Donated
    Money*. Coded 0/1, 1 for yes.
-   get\_ahead: Convenience 0/1 recoding of Gallup WP128: "Can people in
    this country get ahead by working hard, or not? (yes/no)". Coded
    0/1, 1 for yes.

Variables in Durante2017.csv
============================

-   country: Name of country
-   sample\_comment: If there is a particular subset of the national
    data (string).
-   year: Year of data collection.
-   WC\_correlation: Reported warmth/competence correlation in Durante
    (2017).
-   Rich\_colder: A yes/no coding of whether the rich are colder than
    the poor (coded by visual inspection of scatter graphs).
-   SR\_comment: Our comment on the visual coding of `Rich_colder`. If
    any substitution of literal "rich" and "poor" groups was necessary,
    or if we could not find relevant comparison groups (or the scatter
    graphs themselves).
