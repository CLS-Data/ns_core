# Next Steps Longitudinal Dataset \[work in progress\]

[Centre for Longitudinal Studies](https://cls.ucl.ac.uk/)

------------------------------------------------------------------------

## Overview

-   This repository provides **R scripts** that harmonise the multiple sweeps of [**Next Steps**](https://cls.ucl.ac.uk/cls-studies/next-steps/) into a single tidy dataset, so analysts can get straight to research rather than recoding.
-   The variables are given consistent names which relate to the content and age of participants (e.g., `educ25` for education attainment at age 25).

------------------------------------------------------------------------

## Included variable domains

| Domain | Examples |
|-----------------|-------------------------------------------------------|
| Demographics | sex, ethnicity, language, sexuality, partnership, region, |
| Education | qualifications, parents' qualifications. |
| Socioeconomic | own economic activity, parental economic activity, own NSSEC, parental NSSEC, home ownership, household income, IMD |
| Physical and mental health | psychological distress, life satisfaction, self-harm,anxiety, depression, loneliness, long-term illness, self‑rated health |
| Anthropometrics | weight, height, BMI |
| Behaviours | smoking, alcohol, drug, exercise, school absence, police contact, bullying victimisation |

*See `ns_mseu_userguide.docx` for full details.*

## Repository structure

`build-core-dataset.R` is the main R script used to create this dataset. Running this script recreates the derived dataset in `data/derived/`. The main script calls other scripts in the `R/` subfolder, which derive domain-specific variables. It then merges all derived variables into a single dataset. 

```
.
├── build-core-dataset.R        # Run this to produce the derived dataset from start to finish.
├── R/                          # Domain-specific scripts, sourced by the main script
│   ├── 00-load-raw-data.R      # Loads the raw .dta files
│   ├── 01-demographic.R
│   ├── 02-education.R
│   ├── 03-socioeconomic.R
│   ├── 04-health.R
│   ├── 05-anthropometric.R
│   ├── 06-behaviour.R
│   └── helpers.R               # Shared recoding helper functions.
├── data/
│   ├── UKDA-5545-stata/.../safeguarded_eul/   # Place the raw .dta files from UKDS here
│   └── derived/                               # The derived dataset is exported here.
├── renv.lock                   # Tracks R package versions (see "Reproducibility" below)
└── .Rprofile                   # Auto-activates renv when an R session starts here
```

## Code and data availability

The source datasets are available to download from the [**UK Data Service**](https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000030). The derived variable dataset will be deposited once completed.

### Setting up the raw data

Download Study 5545 (Next Steps, End User Licence) from the [**UK Data Service**](https://datacatalogue.ukdataservice.ac.uk/studies/study/5545#details) in Stata 13 (`.dta`) format, then unzip it.

Unzipping the file should give you the `UKDA-5545-stata` folder with the datasets. You can either place this folder in `data/` (which will replace the empty `UKDA-5545-stata` counterpart from this repo). Alternatively, find the `.dta` files and place them directly in the relevant subfolder (that is, `data/UKDA-5545-stata/stata/stata13/safeguarded_eul/`).

## Version control

The code was run with R v4.5.2. R package versions are tracked using [`renv`](https://rstudio.github.io/renv/). If you wish to rerun the code using the same package versions, make sure the `renv` package is installed (`install.packages("renv")`), and run `renv::restore()` the first time you open R in the project root. This will install those package versions locally and link them with the project. Please note that depending on your machine and setup, this may take time, and additional work might be required if some package installations fail.

------------------------------------------------------------------------

## User feedback and future plans

We welcome user feedback and plan to expand this dataset in future releases. Please email clsdata\@ucl.ac.uk.

## Licence

Code: MIT Licence (see `LICENSE`). Derived datasets remain subject to the Next Steps **End‑User Licence** terms.

------------------------------------------------------------------------

© 2025 UCL Centre for Longitudinal Studies