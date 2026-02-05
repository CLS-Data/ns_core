# Source scripts --------------------------------------------------------------------

# This part runs all scripts in the 'R' folder, which process and derive variables for each domain.
source("R/00-load-raw-data.R", echo = TRUE)
source("R/01-demographic.R", echo = TRUE)
source("R/02-education.R", echo = TRUE)
source("R/03-socioeconomic.R", echo = TRUE)
source("R/04-health.R", echo = TRUE)
source("R/05-anthropometric.R", echo = TRUE)
source("R/06-behaviour.R", echo = TRUE)

# Longitudinal Dataset --------------------------------------------------------------------

# prepare for the longitudinal dataset by merging all derived variables
long_vars <- read_dta(file.path(data_path, sweeps$longitudinal)) %>%
  select(
    NSID,
    SAMPPSU,
    SAMPSTRATUM,
    DESIGNWEIGHT,
    MAINBOOST,
    W1OUTCOME,
    W1FINWT,
    W2OUTCOME,
    W2FINWT,
    W3OUTCOME,
    W3FINWT,
    W2TOW3NRWT,
    W4OUTCOME,
    W4WEIGHT_MAIN_BOOST,
    W4WEIGHT_MAIN,
    W5OUTCOME,
    W5FINWT_CROSS,
    W6OUTCOME,
    W6FINWT_CROSS,
    W6LSYPEWT,
    W6LSYPEW4WT,
    W7OUTCOME,
    W7_LSYPE_WT,
    W7FINWT,
    W7_LSYPE_WT_SKIPONLY,
    W8OUTCOME,
    W8FINWT,
    W9OUTCOME,
    W9FINWT,
    DATA_AVAILABILITY
  ) %>%
  mutate(
    across(
      c(
        W1OUTCOME,
        W2OUTCOME,
        W3OUTCOME,
        W4OUTCOME,
        W5OUTCOME,
        W6OUTCOME,
        W7OUTCOME,
        W8OUTCOME,
        W9OUTCOME
      ),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, -1),
        labels = c(
          "Productive",
          "Refusal",
          "Non-contact and other unproductive",
          "Ineligible",
          "Untraced",
          "Not issued",
          "No contact"
        )
      )
    ),
    DATA_AVAILABILITY = factor(
      DATA_AVAILABILITY,
      levels = c(0, 1),
      labels = c("Not available", "Available for research")
    ),
    MAINBOOST = factor(MAINBOOST, levels = c(1, 2), labels = c("Main", "Boost"))
  )

# Merge All Datasets --------------------------------------------------------------------

derived_vars <- list(
  sex_all,
  eth_all,
  lang_all,
  sexuality_all,
  partnr_all,
  region_all,
  educaim_all,
  educ_all,
  parent_edu_all,
  ecoact_all,
  ecoactDT_parents_all,
  nssec_all,
  nssec_parents_all,
  hown_all,
  income_all,
  hh_income_all,
  imd_all,
  ghq_all,
  lsat_all,
  sharm_all,
  gad_all,
  phq_all,
  lon_all,
  wt_all,
  ht_all,
  bmi_all,
  health_all,
  lsi_all,
  smoking_all,
  alc_all_clean,
  drug_all_clean,
  spt_all,
  absence_all,
  suspend_expel_all,
  truancy_all,
  bully_all,
  police_all,
  long_vars
)

# Combine all datasets by NSID
derived_all <- reduce(derived_vars, full_join, by = "NSID")

# Replace any remaining NAs with -5 (missing)
# Identify variables not in longitudinal dataset
new_vars <- setdiff(names(derived_all), names(long_vars))

# Replace NAs accordingly
derived_all <- derived_all %>%
  mutate(
    # Numeric variables: NA → -5
    across(
      all_of(new_vars[sapply(derived_all[new_vars], is.numeric)]),
      ~ if_else(is.na(.x), -5, .x)
    ),

    # Factor variables: NA → "Data not available"
    across(
      all_of(new_vars[sapply(derived_all[new_vars], is.factor)]),
      ~ forcats::fct_expand(.x, "Data not available") |>
        tidyr::replace_na("Data not available")
    )
  )

# Append -5 label for labelled variables
derived_all <- derived_all |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(haven::is.labelled),
      ~ {
        # Only touch variables that actually contain -5
        if (any(.x == -5L, na.rm = TRUE)) {
          labelled::add_value_labels(.x, "Data not available" = -5L)
        } else {
          .x
        }
      }
    )
  )

# set multiple labels at once; returns the modified data frame
derived_all <- set_variable_labels(
  derived_all,
  sex = "Sex (age 32-14y)",
  eth = "ethnicity (age 14-32y)",
  lang = "Language spoken at home (age 17-14y)",
  sori19 = "Sexuality (age 19y)",
  sori20 = "Sexuality (age 20y)",
  sori25 = "Sexuality (age 25y)",
  sori32 = "Sexuality (age 32y)",
  partnr19 = "Legal marital status simple (age 19y)",
  partnr25 = "Legal marital status simple (age 25y)",
  partnr32 = "Legal marital status simple (age 32y)",
  partnradu25 = "Legal marital status complete (age 25y)",
  partnradu32 = "Legal marital status complete (age 32y)",
  regub15 = "Region urban (age 15y)",
  regub16 = "Region urban (age 16y)",
  regov15 = "Region government area (age 15y)",
  regov16 = "Region government area (age 16y)",
  regor25 = "Region government area (age 25y)",
  regor32 = "Region government area (age 32y)",
  regint32 = "Living abroad (age 32y)",
  educaim17 = "Current qualification studied level 3-category (age 17)",
  educaim19 = "Current qualification studied level 3-category  (age 19y)",
  educaim20 = "Current qualification studied level 3-category (age 20y)",
  educaim25 = "Current qualification studied level 3-category (age 25y)",
  educaim32 = "Current qualification studied level 3-category (age 32y)",
  educ25 = "Highest education level 3-category (age 25y)",
  educ32 = "Highest education level 3-category (age 32y)",
  educadtl32 = "Highest education detail - academic (age 32)",
  educvdtl32 = "Highest education detail - vocational (age 32)",
  educma = "Highest education level mother 3-category (CM age 17-14y)",
  educpa = "Highest education level father 3-category (CM age 17-14y)",
  educdtlpa = "Highest education detail father (CM age 14-17y)",
  educdtlma = "Highest education detail mother (CM age 14-17y)",
  ecoact17 = "Economic activity simple (age 17y)",
  ecoact18 = "Economic activity simple (age 18y)",
  ecoact19 = "Economic activity simple (age 19y)",
  ecoact20 = "Economic activity simple (age 20y)",
  ecoact25 = "Economic activity simple (age 25y)",
  ecoact32 = "Economic activity simple (age 32y)",
  ecoactadu25 = "Economic activity detailed (age 25y)",
  ecoactadu32 = "Economic activity detailed (age 32y)",
  ecoactdtma14 = "Economic activity mother (CM age 14y)",
  ecoactdtpa14 = "Economic activity father (CM age 14y)",
  ecoactdtma15 = "Economic activity mother (CM age 15y)",
  ecoactdtpa15 = "Economic activity father (CM age 15y)",
  ecoactdtma16 = "Economic activity mother (CM age 16y)",
  ecoactdtpa16 = "Economic activity father (CM age 16y)",
  ecoactdtma17 = "Economic activity mother (CM age 17y)",
  ecoactdtpa17 = "Economic activity father (CM age 17y)",
  nssec17 = "NS-SEC 17-category (age 17y)",
  nssec18 = "NS-SEC 17-category (age 18y)",
  nssec19 = "NS-SEC 17-category (age 19y)",
  nssec20 = "NS-SEC 17-category (age 20y)",
  nssec25 = "NS-SEC 17-category (age 25y)",
  nssec32 = "NS-SEC 17-category (age 32y)",
  nssecma14 = "NS-SEC mother 17-category (CM age 14y)",
  nssecpa14 = "NS-SEC father 17-category (CM age 14y)",
  nssecma15 = "NS-SEC mother 17-category (CM age 15y)",
  nssecpa15 = "NS-SEC father 17-category (CM age 15y)",
  nssecma16 = "NS-SEC mother 17-category (CM age 16y)",
  nssecpa16 = "NS-SEC father 17-category (CM age 16y)",
  nssecma17 = "NS-SEC mother 17-category (CM age 17y)",
  nssecpa17 = "NS-SEC father 17-category (CM age 17y)",
  nssecma18 = "NS-SEC mother 17-category (CM age 18y)",
  nssecpa18 = "NS-SEC father 17-category (CM age 18y)",
  hown14 = "Household ownership (age 14y)",
  hown15 = "Household ownership (age 15y)",
  hown16 = "Household ownership (age 16y)",
  hown17 = "Household ownership (age 17y)",
  hown18 = "Household ownership (age 18y)",
  hown19 = "Household ownership (age 19y)",
  hown20 = "Household ownership (age 20y)",
  hown25 = "Household ownership (age 25y)",
  hown32 = "Household ownership (age 32y)",
  hownteen14 = "Household ownership detailed (age 14y)",
  hownteen15 = "Household ownership detailed (age 15y)",
  hownteen16 = "Household ownership detailed (age 16y)",
  hownteen17 = "Household ownership detailed (age 17y)",
  hownteen18 = "Household ownership detailed (age 18y)",
  hownteen19 = "Household ownership detailed (age 19y)",
  hownteen20 = "Household ownership detailed (age 20y)",
  inc25 = "Banded weekly income of cohort member and partner (pound, age 25)",
  inc32 = "Banded weekly income of cohort member and partner (pound, age 32)",
  incwhh14 = "Banded weekly parents' gross salary (pound, CM age 14)",
  incwhh15 = "Banded weekly parents' gross salary (pound, CM age 15)",
  incwhhcnt14 = "Weekly parents' gross salary (pounds, continuous, CM age 14)",
  incwhhcnt15 = "Weekly parents' gross salary (pounds, continuous, CM age 15)",
  incwhh16 = "Banded weekly parents' gross salary (pound, CM age 16)",
  incwhh17 = "Banded weekly parents' gross salary (pound, CM age 17)",
  imd15 = "2004 Index of Multiple Deprivation (IMD) (age 15y)",
  imd16 = "2004 Index of Multiple Deprivation (IMD) (age 16y)",
  imd32 = "2019 Index of Multiple Deprivation (IMD) (age 32y)",
  ghq15 = "GHQ-12 12-item score (0-12) (age 15y)",
  ghq17 = "GHQ-12 12-item score (0-12) (age 17y)",
  ghq25 = "GHQ-12 12-item score (0-12) (age 25y)",
  ghq32 = "GHQ-12 12-item score (0-12) (age 32y)",
  ghqtl15 = "GHQ-12 sum score of the each item (12-48) (age 15y)",
  ghqtl17 = "GHQ-12 sum score of the each item (12-48) (age 17y)",
  ghqtl25 = "GHQ-12 sum score of the each item (12-48) (age 25y)",
  ghqtl32 = "GHQ-12 sum score of the each item (12-48) (age 32y)",
  lsat20 = "Life satisfaction (age 20y)",
  lsat25 = "Life satisfaction (age 25y)",
  lsat32 = "Life satisfaction (age 32y)",
  sharm25 = "Self-harm (age 25y)",
  gad32 = "GAD 2-item score (age 32y)",
  phq32 = "PHQ 2-item score (age 32y)",
  lon32 = "UCLA Loneliness 3-item (age 32y)",
  wt0 = "Weight (kilogram, parent reported, age 0y)",
  wt25 = "Weight (kilogram, age 25y)",
  wt32 = "Weight (kilogram, age 32y)",
  ht25 = "Height (metre, age 25y)",
  ht32 = "Height (metre, age 32y)",
  ht25_32 = "Height (metre, age 25y or 32y)",
  bmi25 = "BMI (age 25y)",
  bmi32 = "BMI (age 32y)",
  ghea15 = "Self-rated general health binary (age 15y)",
  ghea16 = "Self-rated general health binary (age 16y)",
  ghea17 = "Self-rated general health binary (age 17y)",
  ghea25 = "Self-rated general health binary (age 25y)",
  ghea32 = "Self-rated general health binary (age 32y)",
  gheateen15 = "Self-rated general health adolescent 4-point (age 15y)",
  gheateen16 = "Self-rated general health adolescent 4-point (age 16y)",
  gheateen17 = "Self-rated general health adolescent 4-point (age 17y)",
  gheaadu25 = "Self-rated general health adult 5-point (age 25y)",
  gheaadu32 = "Self-rated general health adult 5-point (age 32y)",
  lsi14_15 = "Long-term illness binary (age 14-15y)",
  lsi17 = "Long-term illness binary (age 17y)",
  lsi19 = "Long-term illness binary (age 19y)",
  lsi20 = "Long-term illness binary (age 20y)",
  lsi25 = "Long-term illness binary (age 25y)",
  lsi32 = "Long-term illness binary (age 32y)",
  smknw14 = "Smoking now binary (age 14y)",
  smknw15 = "Smoking now binary (age 15y)",
  smknw16 = "Smoking now binary (age 16y)",
  smknw25 = "Smoking now binary (age 25y)",
  smknw32 = "Smoking now binary (age 32y)",
  smk14 = "Smoking ever and frequency (age 14y)",
  smk15 = "Smoking ever and frequency (age 15y)",
  smk16 = "Smoking ever and frequency (age 16y)",
  smk25 = "Smoking ever and frequency (age 25y)",
  smk32 = "Smoking ever and frequency (age 32y)",
  alcfst = "First time mentioned having alcohol (age 14-32y)",
  alcfreq14 = "Alcohol frequency (age 14y)",
  alcfreq15 = "Alcohol frequency (age 15y)",
  alcfreq16 = "Alcohol frequency (age 16y)",
  alcfreq17 = "Alcohol frequency (age 17y)",
  alcfreq19 = "Alcohol frequency (age 19y)",
  alcfreq20 = "Alcohol frequency (age 20y)",
  audita25 = "AUDIT A (age 25y)",
  audita32 = "AUDIT A (age 32y)",
  auditb25 = "AUDIT B (age 25y)",
  auditb32 = "AUDIT B (age 32y)",
  auditc25 = "AUDIT C (age 25y)",
  auditc32 = "AUDIT C (age 32y)",
  drgcnbevr = "Cannabis ever used (age 14-32y)",
  drgcnbfst = "First time reported ever used cannabis (age 14-32y)",
  drgcnbnw19 = "Cannabis current use (age 19y)",
  drgcnbnw20 = "Cannabis current use (age 20y)",
  drgcnbnw25 = "Cannabis current use (age 25y)",
  drgcnbnw32 = "Cannabis current use (age 32y)",
  drgothevr = "Other drugs ever used (age 19-32y)",
  drgothfst = "First time reported ever used other drugs (age 19-32y)",
  drgothnw19 = "Other drugs current use (age 19y)",
  drgothnw20 = "Other drugs current use (age 20y)",
  drgothnw25 = "Other drugs current use (age 25y)",
  drgothnw32 = "Other drugs current use (age 32y)",
  spt14 = "Exercise frequency (age 14y)",
  spt15 = "Exercise frequency (age 15y)",
  spt17 = "Exercise frequency (age 17y)",
  spt19 = "Exercise frequency (age 19y)",
  spt20 = "Exercise frequency (age 20y)",
  spt25 = "Exercise frequency (age 25y)",
  spt32 = "Exercise frequency (age 32y)",
  abs1m14 = "Absence from school over 1 month in the past 12 months (age 14y)",
  abs1m15 = "Absence from school over 1 month in the past 12 months (age 15y)",
  abs1m16 = "Absence from school over 1 month in the past 12 months (age 16y)",
  susp14 = "Suspended from school (age 14y)",
  susp15 = "Suspended from school (age 15y)",
  susp16 = "Suspended from school (age 16y)",
  susp17 = "Suspended from school (age 17y)",
  expl14 = "Expelled from school (age 14y)",
  expl15 = "Expelled from school (age 15y)",
  expl16 = "Expelled from school (age 16y)",
  expl17 = "Expelled from school (age 17y)",
  trua14 = "Truancy within past three years (age 14y)",
  trua15 = "Truancy in the past year (age 15y)",
  trua16 = "Truancy in the past year (age 16y)",
  trua17 = "Truancy in the past year (age 17y)",
  pol14 = "Police contact binary (age 14y)",
  pol15 = "Police contact binary (age 15y)",
  pol16 = "Police contact binary (age 16y)",
  pol17 = "Police contact binary (age 17y, only boost sample)",
  polcnt14 = "Police contact count (age 14y)",
  polcnt15 = "Police contact count (age 15y)",
  polcnt16 = "Police contact count (age 16y)",
  polcnt17 = "Police contact count (age 17y, only boost sample)",
  polwrn25 = "Police warning (age 25y)",
  polars25 = "Police arrest (age 25y)",
  polcau25 = "Police caution (age 25y)",
  polglt25 = "Police found guilty (age 25y)",
  polpnd25 = "Given a Penalty Notice for Disorder (age 25y)",
  polwrn32 = "Police warning (age 32y)",
  polars32 = "Police arrest (age 32y)",
  polcau32 = "Police caution (age 32y)",
  polglt32 = "Police found guilty (age 32y)",
  polpnd32 = "Given a Penalty Notice for Disorder (age 32y)",
  bul14 = "Bullying victimisation (age 14y)",
  bul15 = "Bullying victimisation (age 15y)",
  bul16 = "Bullying victimisation (age 16y)",
  bul17 = "Bullying victimisation (age 17y)",
  bul20 = "Bullying victimisation (age 20y)",
  bul25 = "Bullying victimisation (age 25y)",
  MAINBOOST = "Whether main or boost sample",
  DATA_AVAILABILITY = "Whether the data is available for research use",
  W1OUTCOME = "Fieldwork outcome at Wave 1",
  W2OUTCOME = "Fieldwork outcome at Wave 2",
  W3OUTCOME = "Fieldwork outcome at Wave 3",
  W4OUTCOME = "Fieldwork outcome at Wave 4",
  W5OUTCOME = "Fieldwork outcome at Wave 5",
  W6OUTCOME = "Fieldwork outcome at Wave 6",
  W7OUTCOME = "Fieldwork outcome at Wave 7",
  W8OUTCOME = "Fieldwork outcome at Wave 8",
  W9OUTCOME = "Fieldwork outcome at Wave 9"
)
out <- here::here("data", "derived", "nextsteps_derived_core_v1.dta")
write_dta(derived_all, out)
