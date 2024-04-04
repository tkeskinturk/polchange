## Replication: Life-Course Transitions and Political Orientations

This repository provides the replication files for the paper *Life-Course Transitions and Political Orientations.*

The pre-print version of the paper is stored at [SocArXiv](https://osf.io/preprints/socarxiv/rk47w).

### Notes on Replication

1) *Data Sources*. This study uses data from BHPS, SHP, SOEP, and UKHLS, as documented in the manuscript.

   Note that all raw data files are organized as `source_files` within the path `data/DATASET/source_files`. Since these data require permissions from the respective organizations, the folders currently only contain the list of files needed to run the do-files for data cleaning and storage.

2) *CPF*. This study uses [Comparative Panel File](https://www.cpfdata.com/) for variable harmonization.

   The folder `data/cpf` contains the outputs from the CPF harmonization code applied to the raw source files. These outputs include `shp_cpf.dta`, `soep_cpf.dta`, and `ukhls_cpf.dta`.

   Please follow the CPF instructions to generate these outputs and rename the files accordingly.

3) *The Main Dataframe*.

   `scripts/01_dataPreps.R` prepares an analysis file, stored in `data/data.rds`

If you have the permissions from these organizations, I would be happy to share the final `data.rds`.

### The Steps for Replication

1) Insert the source files into respective `data/DATASET/source_files` folders,
2) Run the do-files to generate the outputs. There is no hard-coded path, but be sure to check your code,
3) Insert the CPF-generated files to `data/cpf`. Adhere to naming conventions mentioned above,
4) Run the scripts stored in `scripts/` in consecutive order. Note that you are advised to use `renv.lock` and the related [{{renv}}](https://rstudio.github.io/renv/articles/renv.html) conventions to make sure that you use the R packages that I used in analyses.
