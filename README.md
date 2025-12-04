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
| Behaviours | smoking, alcohol, drug, exercise, school absence, policy contact, bullying victimisation |

*See `ns_mseu_userguide.docx` for full details.*

## Code and data availability

`build-core-dataset.R` is the main R script used to create this dataset. Running this script recreates the derived dataset in `data/derived/`. The main script calls other scripts in the `R/` subfolder, which derive domain-specific variables. It then merges all derived variables into a single dataset.

The source datasets are available to download from the [**UK Data Service**](https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000030). This derived variable dataset will be despotied once completed.

------------------------------------------------------------------------

## User feedback and future plans

We welcome user feedback and plan to expand this dataset in future releases. Please email clsdata\@ucl.ac.uk.

## Licence

Code: MIT Licence (see `LICENSE`). Derived datasets remain subject to the Next Steps **End‑User Licence** terms.

------------------------------------------------------------------------

© 2025 UCL Centre for Longitudinal Studies