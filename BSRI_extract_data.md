Extracting World Poll data for replication file we can share
================
Erik Ø. Sørensen
11 mai, 2022

# Data availability policy

There are no restrictions on the data used that were collected as part
of the Fairness-Across-the-World module; these outcomes will be publicly
released with a core set of individual background variables. The
remaining set of background variables will be made available upon
request (in order to secure the privacy of individual respondents).

## Gallup policy on what respondent level data can be shared openly:

From Zacc Ritter (Gallup), March 8 2021:

> For general public release, we ask our clients to limit the
> demographic/background variables to the following: Weights (wgt),
> country (WP5), gender (WP1219), age (WP1220), education (WP3117),
> income (INCOME_2), and urbanicity (WP14).
>
> We ask that when NHH makes its 2018 Fairness Around the World data set
> available to the general public, it sticks to this delimited set of
> variables.
>
> In the specific case that another researcher reaches out for a more
> comprehensive data set, Gallup is willing to reach a compromise. NHH
> may provide that specific researcher the more extensive list of
> variables specified below. We ask that NHH not release any of other
> respondent-level items to that researcher not specified below.
>
> 1.  SURVEY STRUCTURE: Weights (wgt), PSUs (WP12259), and country
>     (WP5).
> 2.  BACKGROUND VARIABLES ON INDIVIDUALS/HOUSEHOLDS: gender (WP1219),
>     age (WP1220), marital status (WP1223), number of children under 15
>     years of age (WP1230), education (WP3117), income (INCOME_2 and
>     HHSIZE), employment status (EMP_2010), whether religion is
>     important (WP119), urbanicity (WP14), and whether they are
>     first-generation immigrants (WP4657)

## Update about World Poll identifiers

In communication with Zacc Ritter at Gallup, 2022-03-11, following up on
a contact from a party interested in linking the openly available data
with privately licensed World Poll data

> > Would it be ok to also update the publicly available subset by
> > including WPID_RANDOM such that everyone with a licensed dataset can
> > download and use it without going through me?

> Definitely, that would be a good idea to include WPID.

Conclusion: Code updated to extract WPID_RANDOM as part of the
`WP_selfishness_public` data.

## Gallup policy on access to World Poll respondent level data for verification purposes

From communication with Zacc Ritter (Gallup), March 9, 2021:

> Access to the World Poll Respondent Level Data will be granted to
> individuals representing academic review boards where necessary for
> the purposes of verification of research undertaken using the Client
> commissioned items and World Poll Respondent Level Data. All
> reasonable alternatives must first be exhausted with the reviewing
> body, such as the sharing of topline data. The researcher and Client’s
> Project Manager must notify Gallup in writing of such a requirement
> including a copy of the correspondence from the reviewing body. Upon
> such a request Gallup will grant the reviewer access to the data for a
> limited duration for the sole purpose of review.

# Extracting public subset

``` r
WP_selfishness_public <- readRDS(here::here("data","WP_selfishness_confidential.rds")) %>%
  filter(in_experiment==TRUE) %>%
  dplyr::select(WPID_RANDOM, # Identifying link to the World Poll.
                iso_a3, # Recoding of WP5 
                wgt, # Population weights
                more_selfish, # From Fairness Across the World module
                more_criminal, # From Fairness Across the World module
                gov_should_reduce_inequality, #From Fairness Across the World module
                inequality_unfair, # From Fairness Across the World module
                WP1219, WP1220, WP3117, income_2, WP14) # Core Gallup World Poll variables
saveRDS(WP_selfishness_public, file=here::here("data", "WP_selfishness_public.rds"))
write_dta(WP_selfishness_public, here::here("data", "WP_selfishness_public.dta"))
write_csv(WP_selfishness_public, file=here::here("data", "WP_selfishness_public.csv"))
```

# Extracting non-public but sharable subset

This is the data we are allowed to share with people that are interested
in replicating our data if they show interest and agree that the data
should only be used

``` r
WP_selfishness_request <- readRDS(here::here("data","WP_selfishness_confidential.rds")) %>%
  filter(in_experiment==TRUE) %>%
  dplyr::select(WPID_RANDOM, # Identifying link to the World Poll.
                iso_a3, # Recoding of WP5 
                wgt, # Population weights
                more_selfish, # From Fairness Across the World module
                more_criminal, # From Fairness Across the World module
                gov_should_reduce_inequality, #From Fairness Across the World module
                inequality_unfair, # From Fairness Across the World module
                WP1219, WP1220, WP3117, income_2, WP14,
                WP12259, WP1223, WP1230, HHsize, EMP_2010, WP119, WP4657
                ) # Core Gallup World Poll variables
saveRDS(WP_selfishness_request, file=here::here("data", "WP_selfishness_request.rds"))
write_dta(WP_selfishness_request, here::here("data", "WP_selfishness_request.dta"))
```

# Session Info

``` r
sessionInfo()
```

    ## R version 4.2.0 (2022-04-22)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 20.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3
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
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## other attached packages:
    ##  [1] haven_2.5.0     forcats_0.5.1   stringr_1.4.0   dplyr_1.0.9    
    ##  [5] purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.7   
    ##  [9] ggplot2_3.3.6   tidyverse_1.3.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.2 xfun_0.31        colorspace_2.0-3 vctrs_0.4.1     
    ##  [5] generics_0.1.2   htmltools_0.5.2  yaml_2.3.5       utf8_1.2.2      
    ##  [9] rlang_1.0.2      pillar_1.7.0     withr_2.5.0      glue_1.6.2      
    ## [13] DBI_1.1.2        bit64_4.0.5      dbplyr_2.1.1     modelr_0.1.8    
    ## [17] readxl_1.4.0     lifecycle_1.0.1  munsell_0.5.0    gtable_0.3.0    
    ## [21] cellranger_1.1.0 rvest_1.0.2      evaluate_0.15    knitr_1.39      
    ## [25] tzdb_0.3.0       fastmap_1.1.0    parallel_4.2.0   fansi_1.0.3     
    ## [29] broom_0.8.0      renv_0.14.0      backports_1.4.1  scales_1.2.0    
    ## [33] vroom_1.5.7      jsonlite_1.8.0   bit_4.0.4        fs_1.5.2        
    ## [37] hms_1.1.1        digest_0.6.29    stringi_1.7.6    rprojroot_2.0.3 
    ## [41] grid_4.2.0       here_1.0.1       cli_3.3.0        tools_4.2.0     
    ## [45] magrittr_2.0.3   crayon_1.5.1     pkgconfig_2.0.3  ellipsis_0.3.2  
    ## [49] xml2_1.3.3       reprex_2.0.1     lubridate_1.8.0  assertthat_0.2.1
    ## [53] rmarkdown_2.14   httr_1.4.3       rstudioapi_0.13  R6_2.5.1        
    ## [57] compiler_4.2.0
