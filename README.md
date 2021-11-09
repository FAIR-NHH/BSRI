# Global evidence on the selfish rich inequality hypothesis

Analysis to support the paper **Global evidence on the selfish rich inequality hypothesis** by Ingvild Almås, Alexander W. Cappelen,
Erik Ø. Sørensen and Bertil Tungodden.

## Data

Data are not part of this analysis replication file. The publicly available data are available
from Harvard Dataverse:

- Almås, Ingvild; Cappelen, Alexander W.; Sørensen, Erik Ø.; Tungodden, Bertil, 2021, "Global evidence on the selfish rich inequality hypothesis", https://doi.org/10.7910/DVN/ZEGFIT, Harvard Dataverse, V2, UNF:6:n88HFUBLvRkZYyDkT33PHQ== [fileUNF]

The files to be downloaded from here are, in the original format:

- `BSRI_external_data.dta`: External country level information.
- `Durante2017.csv`: Visual coding of warmth data in Durante et al (2017), (https://doi.org/10.1073/pnas.1611874114).
- `WP_countryids.csv`: Coding from World Poll numbering of countries to ISO-3 countrycodes.
- `readme.html`: A description of each variable in the files and details on how to access the full Gallup World Poll data for replication purposes.

These files should be saved in the `data/` subdirectory.

One more file is available from the Harvard Dataverse site: 
The file `WP_selfishness_public.tab` contains the freely available variables collected as part of the Fairness-Across-the-World module of the Gallup
World Poll 2018. This file contains also a restricted set of background data. The current replication package requires the presence of `WP2018.dta`,
the full Gallup World Poll 2018 dataset with the Fairness-Across-the-World module linked in. The relevant variables are described in the `readme.txt` file, but
the licensing agreement with Gallup precludes us from publicly sharing this. Access to a somewhat broader set of background variables can be made
available upon request, other Gallup World Poll variables are subject to licensing from Gallup or special request from academic review boards.

## Running the analysis

1. `BSRI_extract_data.Rmd` extracts data from the original World Poll file for public use. Can be run independently of the others. Uses R.
2. `BSRI_recode_World-Poll.Rmd` recodes the original World Poll data and saves analytical data in the `data/` directory. Uses R.
3. `BSRI_analysis.Rmd` creates all the graphs for both paper and SI and saves input files for use by the lasso code. Uses R.
4. `lasso_within.do` runs the within-country lasso regressions in Table~1. Uses Stata version 16.1. 
5. `lasso_between.do` runs the between-country lasso regressions in Table~1. Uses Stata version 16.1.

The R-configuration used for running the analysis is controlled by the R `renv` package (v 0.14.0), 
with the configuration of all packages documented
in `renv.lock`, and the relevant configuration automatically loaded by the `.Rprofile`.

The output from the analysis is saved in the repository in `graphs/` and `tables/`
