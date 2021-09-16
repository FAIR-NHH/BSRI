Recoding Gallup World Poll data for ‘Belief in Selfish Rich Inequality’
================
Erik Ø. Sørensen
16 september, 2021

This documents the recoding of World Poll data into the analytical
dataset for **Global evidence on the selfish rich hypothesis**, and
finally subsets the data that we actually use.

Se separate readme file for description of each variable that we use.

# Organization

## ISO-letter codes for countries

I want to merge in the iso3 country codes and an indicator for the
country being ni the experiment.

``` r
WP_countryids <- read_csv(here::here("data","WP_countryids.csv")) %>% 
  mutate(WP5int = as.integer(WP5)) %>% 
           dplyr::select(WP5int, iso_a3)
```

    ## Rows: 142 Columns: 3

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): countrynew, iso_a3
    ## dbl (1): WP5

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
WP <- WP %>% mutate(WP5int = as.integer(WP5),
         in_experiment = !is.na(WP20030)) %>% 
  left_join(WP_countryids)
```

    ## Joining, by = "WP5int"

## Unique PSU identifiers

The phone-countries have no valid PSU codes. Since we want to use these
for clustering in a way that works universally, we need to create new
PSU codes that are unique and which allow for single-individual psus.

``` r
psus <- tibble(WPID_RANDOM = numeric(), psu = numeric())
all_wp5s <- unique(WP$WP5)
all_wp5s <- all_wp5s[!is.na(all_wp5s)]
for (i in seq_along(all_wp5s)) {
  cnt <- WP %>% filter(WP5 == all_wp5s[i])
  psu <- cnt$WP12259
  if (is.na(psu[1])) psu = 1:length(psu)
  psus <- psus %>% rbind( tibble( WPID_RANDOM = cnt$WPID_RANDOM, psu=psu) ) 
}
WP <- WP %>% left_join(psus, by="WPID_RANDOM")
WP <- WP %>% group_by(WP5, psu) %>%
  mutate(psuid = group_indices()) %>%
  ungroup()
```

    ## Warning: `group_indices()` was deprecated in dplyr 1.0.0.
    ## Please use `cur_group_id()` instead.

# Belief and attitude questions

## Necessary processing

I write a function to translate from the belief/attitude questions
(which are distributed over three responses) into a cardinal scale
(1-5). If people answer “don’t know” to the first question, this is
coded to 3. If they answer “don’t know” within either the “agree” or
“disagree” branches, I assign the the moderate answers (either 2 or 4).

``` r
qbelief <- function(b1, b2, b3) {
  b2 <- ifelse(is.na(b2), 0, b2) # Since otherwise the missing will make the test outcome missing. 
  b3 <- ifelse(is.na(b3), 0, b3) # Since otherwise the missing will make the test outcome missing.
  
  b <- ifelse(b1==3, 3, NA) 

  b <- ifelse(b2==1, 5, b)
  b <- ifelse(b2==2, 4, b)
  b <- ifelse(b2==98, 4, b)
  b <- ifelse(b2==99, NA, b)
  
  b <- ifelse(b3==1, 1, b)
  b <- ifelse(b3==2, 2, b)
  b <- ifelse(b3==98, 2, b)
  b <- ifelse(b3==99, NA, b)

  b
}
```

## Applying processing steps

``` r
WP <- WP %>%
    mutate(more_selfish = qbelief(WP20081, WP20082, WP20083),
          more_criminal = qbelief(WP20093, WP20094, WP20095),
          inequality_unfair = qbelief(WP20096, WP20097, WP20098),
          gov_should_reduce_inequality = qbelief(WP20099, WP20100, WP20101))
```

# Demographics

The demographic variables of the World Poll have complicated names, and
in some cases we want to split them into a series of dummy indicators,
or we would like z-score variants of the variables. For income, z-scores
are very sensitive to outliers, and to start, we want the place in the
income distribution within country. I take the rank of income, and scale
the rank such that it is \[-0.5, 0.5\], with 0.5 being the highest level
of income within country The convention I aim for is that there are
prefixes to variables to indicate the type of variables: “n” for counts,
“z” for normalized variables with mean zero and unit variance, and “r”
for rank variables.

For some variables, it is necessary to account for the don’t know and
refused to answer categories, which I code as NAs.

``` r
xtoz <- function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
WP <- WP %>%  mutate( d_female = WP1219 ==2,
                      d_male = WP1219==1,
                      age = ifelse(WP1220<100, WP1220, NA),
                      z_age = xtoz(WP1220),
                      d_married = WP1223 == 2,
                      n_children = ifelse(WP1230<98, WP1230, NA),
                      z_n_children = xtoz(n_children),
                      d_midedu = WP3117 == 2,
                      d_highedu = WP3117 == 3,
                      d_immigrant = WP4657 == 2,
                      d_working = EMP_2010 %in% c(1,2,3,5),
                      d_religionimp = WP119 == 1,
                      d_urban = WP14 %in% c(2,3,6))
```

# Rich and poor

In the PAP, we committed to using household income and household size to
construct the income measures. For the main analysis, we said we would
focus on OECD practice of square-root equivalence scale, but that we
also would provide robustness tests using the OECD-modified sacle and a
per capita measure
<http://www.oecd.org/els/soc/OECD-Note-EquivalenceScales.pdf>. The OECD
modified scale “assigns a value of 1 to the household head, of 0.5 to
each additional adult member and of 0.3 to each child.”

``` r
WP <- WP %>% mutate( inc_oecd1 = income_2 / sqrt(HHsize),
                     inc_oecd2 = income_2 / ( 1 + 0.5*pmax(WP12-1, 0) + 0.3*n_children ),
                     inc_cap   = income_2 / HHsize)
```

From the PAP, "we will focus on the definition of relative poverty
applied by the EU, where someone is considered poor if they have 60% of
the national median equivalized disposable income. I use the post
stratification weights to calculate the quantiles. Some countries don’t
have income data (most important to us: Venezuela). I need to filter out
this country in order to calculate the rich/poor indicators before
merging the resulting indicators back into the dataset.

``` r
WP <- WP %>% group_by(WP5) %>%
  mutate( noinc = min(is.na(inc_oecd1))) %>%
  filter(noinc!=1) %>% 
  mutate( poor_EU_oecd1 = (inc_oecd1 < 0.6 * wtd.quantile(inc_oecd1, 0.5, weight=wgt)),
          poor_EU_cap = (inc_cap < 0.6 * wtd.quantile(inc_cap, 0.5, weight=wgt)),
          rich_05p_oecd1 = (inc_oecd1 > wtd.quantile(inc_oecd1, 0.95, weight=wgt)),
          rich_10p_oecd1 = (inc_oecd1 > wtd.quantile(inc_oecd1, 0.9, weight=wgt)),
          rich_15p_oecd1 = (inc_oecd1 > wtd.quantile(inc_oecd1, 0.85, weight=wgt)),
          rich_20p_oecd1 = (inc_oecd1 > wtd.quantile(inc_oecd1, 0,80, weight=wgt)),
          rich_50p_oecd1 = (inc_oecd1 > wtd.quantile(inc_oecd1, 0,50, weight=wgt)),
          rich_05p_cap = (inc_cap > wtd.quantile(inc_cap, 0.95, weight=wgt)),
          rich_10p_cap = (inc_cap > wtd.quantile(inc_cap, 0.9, weight=wgt)),
          rich_15p_cap = (inc_cap > wtd.quantile(inc_cap, 0.85, weight=wgt)),
          rich_20p_cap = (inc_cap > wtd.quantile(inc_cap, 0,80, weight=wgt)),
          rich_50p_cap = (inc_cap > wtd.quantile(inc_cap, 0,80, weight=wgt)),
          inc_rank_oecd1 = rank(inc_oecd1)/length(inc_oecd1),
          inc_rank_cap = rank(inc_cap)/length(inc_cap)) %>%
    ungroup() %>% 
  select(c(WPID_RANDOM, poor_EU_oecd1, rich_05p_oecd1, rich_10p_oecd1, rich_15p_oecd1, rich_20p_oecd1, rich_50p_oecd1,
           poor_EU_cap, rich_05p_cap, rich_10p_cap, rich_15p_cap, rich_20p_cap, rich_50p_cap, inc_rank_oecd1, inc_rank_cap)) %>%
  full_join( WP, by="WPID_RANDOM")
```

# Recoding other variables

Other variables used in the robustness analysis.

``` r
WPs <- WP %>% filter(in_experiment) %>% mutate(
  corruption_business = case_when( WP145==1 ~ TRUE,
                                     WP145==2 ~ FALSE,
                                     WP145==3 ~ NA, # Don't know
                                     WP145==4 ~ NA), # Refused
  corruption_government = case_when( WP146==1 ~ TRUE,
                                     WP146==2 ~ FALSE,
                                     WP146==3 ~ NA, # Don't know
                                     WP146==4 ~ NA), # Refused
  donated_money = case_when( WP108==1 ~ TRUE,
                                     WP108==2 ~ FALSE,
                                     WP108==3 ~ NA, # Don't know
                                     WP108==4 ~ NA), # Refused
  volunteered_time = case_when( WP109==1 ~ TRUE,
                                     WP109==2 ~ FALSE,
                                     WP109==3 ~ NA, # Don't know
                                     WP109==4 ~ NA), # Refused
  helped_stranger = case_when( WP110==1 ~ TRUE,
                                     WP110==2 ~ FALSE,
                                     WP110==3 ~ NA, # Don't know
                                     WP110==4 ~ NA), # Refused
  get_ahead = case_when( WP128==1 ~ TRUE, 
                         WP128==2 ~ FALSE,
                         WP128==3 ~ NA, # Don't know
                         WP128==4 ~ NA), # Refused
  ) %>% rowwise() %>%
  mutate( helping_index = mean( c(donated_money, volunteered_time,
                                  helped_stranger), na.rm=TRUE)) %>%
  ungroup()
```

# Education reclassification

In light of the differences by country, it seems like a uniform way of
defining high/low education is not going to work. In Rwanda there is
only one person at the max education level, so for Rwanda, medium/high
needs to be one of the groups. For the United States, we have only 12
individuals at the lowest level, so the high group cannot contain the
middle group. I base the classification into high education relative to
each country, allocating the middle group to the low or high category
based aiming to maximize the size of the smallest group.

``` r
edu_classification <- WPs %>% group_by(iso_a3) %>% 
  summarize( size_lower = 1 - weighted.mean(d_highedu, wgt),
            size_upper = weighted.mean(d_highedu+d_midedu, wgt)) %>%
  mutate(where_to_allocate_middle_education = ifelse(size_upper>size_lower, "lower", "upper"))
edu_classification %>% knitr::kable(digits = 3)
```

| iso\_a3 | size\_lower | size\_upper | where\_to\_allocate\_middle\_education |
|:--------|------------:|------------:|:---------------------------------------|
| AFG     |       0.979 |       0.256 | upper                                  |
| ARG     |       0.946 |       0.629 | upper                                  |
| AUS     |       0.704 |       0.943 | lower                                  |
| BGD     |       0.970 |       0.588 | upper                                  |
| BOL     |       0.860 |       0.657 | upper                                  |
| BRA     |       0.907 |       0.550 | upper                                  |
| CAN     |       0.745 |       0.962 | lower                                  |
| CHE     |       0.618 |       0.691 | lower                                  |
| CHL     |       0.831 |       0.745 | upper                                  |
| CHN     |       0.880 |       0.319 | upper                                  |
| CMR     |       0.984 |       0.415 | upper                                  |
| COL     |       0.942 |       0.626 | upper                                  |
| CZE     |       0.868 |       0.811 | upper                                  |
| DEU     |       0.750 |       0.963 | lower                                  |
| DZA     |       0.930 |       0.460 | upper                                  |
| ECU     |       0.921 |       0.771 | upper                                  |
| EGY     |       0.889 |       0.648 | upper                                  |
| ESP     |       0.924 |       0.795 | upper                                  |
| EST     |       0.661 |       0.802 | lower                                  |
| ETH     |       0.979 |       0.211 | upper                                  |
| FRA     |       0.797 |       0.870 | lower                                  |
| GBR     |       0.711 |       0.815 | lower                                  |
| GRC     |       0.812 |       0.720 | upper                                  |
| HRV     |       0.829 |       0.709 | upper                                  |
| HUN     |       0.822 |       0.740 | upper                                  |
| IDN     |       0.954 |       0.529 | upper                                  |
| IND     |       0.955 |       0.460 | upper                                  |
| IRN     |       0.767 |       0.709 | upper                                  |
| ISR     |       0.681 |       0.951 | lower                                  |
| ITA     |       0.907 |       0.540 | upper                                  |
| JOR     |       0.879 |       0.761 | upper                                  |
| JPN     |       0.773 |       0.871 | lower                                  |
| KAZ     |       0.787 |       0.843 | lower                                  |
| KEN     |       0.905 |       0.568 | upper                                  |
| KHM     |       0.978 |       0.079 | upper                                  |
| KOR     |       0.542 |       0.830 | lower                                  |
| LKA     |       0.980 |       0.432 | upper                                  |
| MAR     |       0.951 |       0.369 | upper                                  |
| MEX     |       0.813 |       0.692 | upper                                  |
| MWI     |       0.984 |       0.269 | upper                                  |
| NGA     |       0.984 |       0.624 | upper                                  |
| NLD     |       0.688 |       0.952 | lower                                  |
| NOR     |       0.635 |       0.817 | lower                                  |
| PAK     |       0.938 |       0.286 | upper                                  |
| PER     |       0.911 |       0.741 | upper                                  |
| PHL     |       0.858 |       0.752 | upper                                  |
| PRT     |       0.830 |       0.654 | upper                                  |
| RUS     |       0.748 |       0.851 | lower                                  |
| RWA     |       1.000 |       0.150 | upper                                  |
| THA     |       0.888 |       0.314 | upper                                  |
| TUR     |       0.860 |       0.671 | upper                                  |
| TZA     |       0.987 |       0.202 | upper                                  |
| UGA     |       0.991 |       0.456 | upper                                  |
| UKR     |       0.790 |       0.838 | lower                                  |
| USA     |       0.669 |       0.964 | lower                                  |
| VEN     |       0.845 |       0.711 | upper                                  |
| VNM     |       0.901 |       0.600 | upper                                  |
| ZAF     |       0.955 |       0.738 | upper                                  |
| ZMB     |       0.958 |       0.590 | upper                                  |
| ZWE     |       0.968 |       0.638 | upper                                  |

``` r
edu_classification %>% with(table(where_to_allocate_middle_education))
```

    ## where_to_allocate_middle_education
    ## lower upper 
    ##    16    44

Now, we can merge this way of classifying education into groups back to
the individual level data set. Preparing the data:

``` r
WPs_edu_classified <- WPs %>% 
  mutate(country = countrycode(iso_a3, "iso3c", "country.name")) %>%
  left_join(edu_classification) %>%
  mutate(binary_education_high = ifelse(where_to_allocate_middle_education=="upper",
                                        d_highedu | d_midedu, d_highedu))
```

    ## Joining, by = "iso_a3"

``` r
# Carrying the new variable onwards
WPs <- WPs_edu_classified
```

# Weighting

We have population weights that are defined relative to each country,
and they work to create estimates that are weighted to be nationally
representative. When we mix micro data across countries, it would mean
that countries with many observations (China: 3649, India: 3000, Russia:
2000) have a larger impact on the estimates; for most countries we have
only 1000 observations, regardless of the country being large (the US,
Indonesia, Nigeria, …) or small (Estonia, Norway, …). To implement the
same idea of weighting each country the same, I create a scaled version
of the population weight that gives each country the same weight.

``` r
WPs <- WPs %>% group_by(country) %>%
  mutate(wgtw = ( wgt / mean(wgt) )  * ( 1000 / n())) %>%
  ungroup()
```

# Saving results

``` r
saveRDS(WPs, file=here::here("data","WP_selfishness_confidential.rds"))
```

# Session Info

``` r
sessionInfo()
```

    ## R version 4.1.1 (2021-08-10)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 18.04.5 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=nb_NO.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=nb_NO.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=nb_NO.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=nb_NO.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] reldist_1.6-6     readxl_1.3.1      countrycode_1.3.0 labelled_2.8.0   
    ##  [5] haven_2.4.3       forcats_0.5.1     stringr_1.4.0     dplyr_1.0.7      
    ##  [9] purrr_0.3.4       readr_2.0.1       tidyr_1.1.3       tibble_3.1.3     
    ## [13] ggplot2_3.3.5     tidyverse_1.3.1  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-152        fs_1.5.0            bit64_4.0.5        
    ##  [4] lubridate_1.7.10    RColorBrewer_1.1-2  httr_1.4.2         
    ##  [7] rprojroot_2.0.2     tools_4.1.1         backports_1.2.1    
    ## [10] utf8_1.2.2          R6_2.5.1            rpart_4.1-15       
    ## [13] Hmisc_4.5-0         DBI_1.1.1           mgcv_1.8-36        
    ## [16] colorspace_2.0-2    nnet_7.3-16         withr_2.4.2        
    ## [19] tidyselect_1.1.1    gridExtra_2.3       bit_4.0.4          
    ## [22] compiler_4.1.1      cli_3.0.1           rvest_1.0.1        
    ## [25] htmlTable_2.2.1     xml2_1.3.2          scales_1.1.1       
    ## [28] checkmate_2.0.0     digest_0.6.27       foreign_0.8-81     
    ## [31] rmarkdown_2.10      base64enc_0.1-3     jpeg_0.1-9         
    ## [34] pkgconfig_2.0.3     htmltools_0.5.1.1   highr_0.9          
    ## [37] dbplyr_2.1.1        htmlwidgets_1.5.4   rlang_0.4.11       
    ## [40] rstudioapi_0.13     generics_0.1.0      jsonlite_1.7.2     
    ## [43] vroom_1.5.4         magrittr_2.0.1      Formula_1.2-4      
    ## [46] Matrix_1.3-4        Rcpp_1.0.7          munsell_0.5.0      
    ## [49] fansi_0.5.0         lifecycle_1.0.0     stringi_1.7.3      
    ## [52] yaml_2.2.1          grid_4.1.1          parallel_4.1.1     
    ## [55] crayon_1.4.1        lattice_0.20-44     splines_4.1.1      
    ## [58] hms_1.1.0           knitr_1.33          pillar_1.6.2       
    ## [61] reprex_2.0.1        glue_1.4.2          evaluate_0.14      
    ## [64] latticeExtra_0.6-29 data.table_1.14.0   modelr_0.1.8       
    ## [67] png_0.1-7           vctrs_0.3.8         tzdb_0.1.2         
    ## [70] cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
    ## [73] xfun_0.25           broom_0.7.9         survival_3.2-11    
    ## [76] cluster_2.1.2       ellipsis_0.3.2      here_1.0.1
