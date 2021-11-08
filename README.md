# Global evidence on the selfish rich inequality hypothesis

Analysis to support the paper **Global evidence on the selfish rich inequality hypothesis** by Ingvild Almås, Alexander W. Cappelen,
Erik Ø. Sørensen and Bertil Tungodden.

## Data

Data are not part of this analysis replication file. The publicly available data are available
from Harvard Dataverse:

- Sørensen, Erik Ø.; Almås, Ingvild; Cappelen, Alexander W.; Tungodden, Bertil, 2021, "Global evidence on people's belief in the selfish rich inequality hypothesis", (https://doi.org/10.7910/DVN/ZEGFIT), Harvard Dataverse, V1, UNF:6:Y4kMU39xY3LTKD1DLR4J2A== [fileUNF]

The files to be downloaded from here are, in the original format,

- `BSRI_external_data.dta`: External country level information.
- `Durante2017.csv`: Visual coding of warmth data in Durante et al (2017), (https://doi.org/10.1073/pnas.1611874114).
- `readme.txt`: A description of each variable in the files and details on how to access the full Gallup World Poll data for replication purposes.

These files should be saved in the `data/` subdirectory.

The file `WP_selfishness_public.tab` contains the freely available variables collected as part of the Fairness-Across-the-World module of the Gallup
World Poll 2018. This file contains also a restricted set of background data. The current replication package requires the presence of `WP2018.dta`,
the full Gallup World Poll 2018 dataset with the Fairness-Across-the-World module linked in. The variables are described in the `readme.txt` file, but
the licensing agreement with Gallup precludes us from publicly sharing this.

## Running the analysis

1. `BSRI_extract_data.Rmd` extracts data from the original World Poll file for public use. Can be run independently of the others. Uses R.
2. `BSRI_recode_World-Poll.Rmd` recodes the original World Poll data and saves analytical data in the `data/` directory. Uses R.
3. `BSRI_analysis.Rmd` creates all the graphs for both paper and SI and saves input files for use by the lasso code. Uses R.
4. `lasso_within.do` runs the within-country lasso regressions in Table~1. Uses Stata version 17. 
5. `lasso_between.do` runs the between-country lasso regressions in Table~1. Uses Stata version 17.

The R-configuration used for running the analysis is controlled by the R `renv` package (v 0.14.0), 
with the configuration of all packages documented
in `renv.lock`, and the relevant configuration automatically loaded by the `.Rprofile`.
