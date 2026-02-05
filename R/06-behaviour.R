# Prerequisite: 00-load-raw-data.R (this script also sources helpers.R)
#
# If you are running this script on its own, please run the following first
# from the project root:
#
# source(here::here("R", "00-load-raw-data.R"))
#
# or manually run 00-load-raw-data.R before this script.

# Smoke --------------------------------------------------------------------
# Load smoking data from relevant sweeps
smoking_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, smk14_ever = W1cignowYP, smk14_freq = W1cigfreqYP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, smk15_ever = W2cignowYP, smk15_freq = W2cigfreqYP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, smk16_ever = W3cignowYP, smk16_freq = W3cigfreqYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, smk25_ever_freq = W8SMOKING),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, smk32_ever_freq = W9SMOKING)
)

# Merge all sweeps
smoking_all <- reduce(smoking_vars, full_join, by = "NSID")


# Merge all sweeps
# smoking_all <- reduce(smoking_vars, full_join, by = "NSID") %>%
# Rename all smknw to smk_ever and smk to smk_freq for readability
#  rename_with(
#    ~ stringr::str_replace()
# Add '_raw' suffix to all 'smk*' variable names for simpler re-coding & cross-checks
#  rename_with(
#    ~ stringr::str_c(.x, "_raw"),
#    contains("smk")
#  )

# Note on smoking variables:
## In some sweeps, participants were first asked if they ever smoked [smknw_raw variables].
## If positive, they were then asked how often they smoke(d). [smk_raw variables]
## This means that if a person did not smoke, they would have frequency as missing ['Not applicable'].

## Standardise values --------------------------------------------------------------------

## The following code will convert missing values and responses to a common coding scheme.

# Recode if ever smoking for age 14-16
recode_smk_ever_14_16 <- function(x) {
  case_when(
    x == 1 ~ 1, # Yes
    x == 2 ~ 0, # No
    x == -96 ~ -3,
    x %in% c(-92, -97) ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    x == -99 ~ -3, # YP not interviewed
    TRUE ~ -3
  )
}

# Recode smoking frequency for age 14-16
recode_smk_freq_14_16 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 0, # Never
    x == 3 ~ 1, # used to, don’t at all now
    x %in% c(4, 5) ~ 2, #  smoke cigs occasionally – not every day
    x == 6 ~ 3, # smoke cigs almost every day
    x == -91 ~ -1,
    x == -96 ~ -3,
    x %in% c(-92, -97) ~ -9,
    x == -1 ~ -8,
    x == -99 ~ -3, # YP not interviewed
    TRUE ~ -3
  )
}

# Derive smoking frequency for age 25 and 32
recode_smk_25_32_to_freq <- function(x) {
  case_when(
    x > 0 ~ x - 1, # Convert 1-4 to 0-3
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ -3
  )
}

# Derive binary smoking status for age 25 and 32
recode_smk_25_32_to_ever <- function(x) {
  case_when(
    x %in% c(0, 1) ~ 0,
    x %in% c(2, 3) ~ 1,
    TRUE ~ x
  )
}

smoking_std <- smoking_all %>%
  mutate(
    # Smoking ever age 14-16
    across(
      c(smk14_ever, smk15_ever, smk16_ever),
      recode_smk_ever_14_16,
      .names = "{col}_std"
    ),
    # Smoking freq age 14-16
    across(
      c(smk14_freq, smk15_freq, smk16_freq),
      recode_smk_freq_14_16,
      .names = "{col}_std"
    ),
    # Smoking freq age 25-32 (derived from combined ever/freq fields)
    smk25_freq_std = recode_smk_25_32_to_freq(smk25_ever_freq),
    smk32_freq_std = recode_smk_25_32_to_freq(smk32_ever_freq),
    # Smoking ever age 25-32 (binary, derived from recoded freq)
    smk25_ever_std = recode_smk_25_32_to_ever(smk25_freq_std),
    smk32_ever_std = recode_smk_25_32_to_ever(smk32_freq_std)
  )

## Derive smoking status variables --------------------------------------------------------------------

# Helpers

# Derive binary current smoking status ages 14-16  ('smknw[age]')
# A person counts as 'not currently smoking' if either of the following conditions are met:
# i) indicated not smoking when asked 'Do you ever smoke cigarettes at all?' [EVER questions]
# ii) indicated they never smoke, they tried smoking only once, or they used to smoke but not anymore [FREQ questions]
derive_smk_now <- function(ever_var, freq_var) {
  case_when(
    ever_var == 0 ~ 0L, # If EVER: 'Not smoking' -> "No"
    freq_var %in% c(0, 1) ~ 0L, # otherwise if FREQ: 'Never smoked'/'Only once' or 'Used to smoke but never now' -> 'No'
    ever_var == 1 ~ 1L, # otherwise if EVER: 'Yes' to smoking -> 'Yes'
    # Missing values:
    freq_var == -9 | ever_var == -9 ~ -9L, # otherwise: if either refused -> 'Refusal'
    freq_var == -8 | ever_var == -8 ~ -8L, # otherwise: if either dk/insufficient info -> 'dk/insufficient info'
    freq_var == -1 | ever_var == -1 ~ -1L, # otherwise: if either not applicable -> not applicable,
    .default = -3L # everything else defaults to 'not interviewed/asked etc.'
  )
}

derive_smk_detailed <- function(ever_var, freq_var) {
  case_when(
    freq_var >= 0 ~ as.integer(freq_var),
    ever_var == 0 ~ 0, # Those who replied 'not ever smoke' -> 'never' (limitation: they were not asked if they never smoked)
    freq_var == -9 | ever_var == -9 ~ -9L,
    freq_var == -8 | ever_var == -8 ~ -8L,
    freq_var == -1 | ever_var == -1 ~ -1L,
    TRUE ~ -3L
  )
}


smoking_rec <- smoking_std %>%
  mutate(
    # Detailed smoking frequency
    smk14 = derive_smk_detailed(
      ever_var = smk14_ever_std,
      freq_var = smk14_freq_std
    ),
    smk15 = derive_smk_detailed(
      ever_var = smk15_ever_std,
      freq_var = smk15_freq_std
    ),
    smk16 = derive_smk_detailed(
      ever_var = smk16_ever_std,
      freq_var = smk16_freq_std
    ),
    smk25 = derive_smk_detailed(
      ever_var = smk25_ever_std,
      freq_var = smk25_freq_std
    ),
    smk32 = derive_smk_detailed(
      ever_var = smk32_ever_std,
      freq_var = smk32_freq_std
    ),

    # Binary current smoking status
    smknw14 = derive_smk_now(
      ever_var = smk14_ever_std,
      freq_var = smk14_freq_std
    ),
    smknw15 = derive_smk_now(
      ever_var = smk15_ever_std,
      freq_var = smk15_freq_std
    ),
    smknw16 = derive_smk_now(
      ever_var = smk16_ever_std,
      freq_var = smk16_freq_std
    ),
    smknw25 = derive_smk_now(
      ever_var = smk25_ever_std,
      freq_var = smk25_freq_std
    ),
    smknw32 = derive_smk_now(
      ever_var = smk32_ever_std,
      freq_var = smk32_freq_std
    )
  ) %>%
  mutate(
    across(
      c(smk14, smk15, smk16, smk25, smk32),
      ~ labelled(
        .x,
        labels = c(
          "Never" = 0,
          "Used to smoke, don’t at all now" = 1,
          "Smoke occasionally – not every day" = 2,
          "Smoke almost every day" = 3,
          common_missing_labels
        )
      )
    ),
    across(
      c(smknw14, smknw15, smknw16, smknw25, smknw32),
      ~ labelled(
        .x,
        labels = c(
          "No" = 0,
          "Yes" = 1,
          common_missing_labels
        )
      )
    )
  )

# Checks
smoking_rec %>%
  count(smk14_ever, smk14_freq, smk14, smknw14)

smoking_rec %>%
  count(smk15_ever, smk15_freq, smk15, smknw15)

smoking_rec %>%
  count(smk16_ever, smk16_freq, smk16, smknw16)

smoking_rec %>%
  count(smk25_ever_freq, smk25, smknw25)

smoking_rec %>%
  count(smk32_ever_freq, smk32, smknw32)

smoking_all <- smoking_rec %>%
  select(
    NSID,
    smknw14,
    smknw15,
    smknw16,
    smknw25,
    smknw32,
    smk14,
    smk15,
    smk16,
    smk25,
    smk32
  )

# Alcohol --------------------------------------------------------------------
# Load and Select Variables
alc_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(
      NSID,
      alcever_S1 = W1alceverYP,
      alcmon_S1 = W1alcmonYP,
      alcfreq_S1 = W1alcfreqYP
    ),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, alcever_S2 = W2alceverYP, alcfreq_S2 = W2alcfreqYP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, alcever_S3 = W3alceverYP, alcfreq_S3 = W3alcfreqYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, alcever_S4 = W4AlcEverYP, alcfreq_S4 = W4AlcFreqYP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, alcever_S6 = W6AlcEverYP, alcfreq_S6 = W6AlcFreqYP),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, alcever_S7 = W7AlcEverYP, alcfreq_S7 = W7AlcFreqYP),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, audita25 = W8AUDIT1, auditb25 = W8AUDIT2, auditc25 = W8AUDIT6),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, audita32 = W9AUDIT1, auditb32 = W9AUDIT2, auditc32 = W9AUDIT3)
)

# Merge all alcohol variables by NSID
alc_all <- reduce(alc_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'audit*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("audit")
  )


## First time had alcohol --------------------------------------------------------------------

# Helpers: Derive 'not drinking' from alcever and audita
# This will be used to derive never drinkers.
never_from_alcever <- function(x) {
  dplyr::case_when(
    x < 0 ~ NA_real_, # negative codes = missing
    x == 2 ~ 1, # "never"
    x == 1 ~ 0, # "ever"
    .default = NA_real_
  )
}

never_from_audita <- function(x) {
  dplyr::case_when(
    x < 0 ~ NA_real_, # negative codes = missing
    x == 1 ~ 1, # "never"
    x > 1 ~ 0, # "ever"
    .default = NA_real_
  )
}

alc_first_age_rec <- alc_all |>
  mutate(
    # Derive age first known drinking
    ever14 = if_else(alcever_S1 == 1 & alcmon_S1 == 1, 14, NA_real_),
    ever15 = if_else(alcever_S2 == 1, 15, NA_real_),
    ever16 = if_else(alcever_S3 == 1, 16, NA_real_),
    ever17 = if_else(alcever_S4 == 1, 17, NA_real_),
    ever19 = if_else(alcever_S6 == 1, 19, NA_real_),
    ever20 = if_else(alcever_S7 == 1, 20, NA_real_),
    ever25 = if_else(audita25_raw > 1, 25, NA_real_),
    ever32 = if_else(audita32_raw > 1, 32, NA_real_),

    first_age_raw = pmin(
      ever14,
      ever15,
      ever16,
      ever17,
      ever19,
      ever20,
      ever25,
      ever32,
      na.rm = TRUE
    ),

    # Derive known never drinking
    # recode to 1 = "never", 0 = "ever", NA = missing
    across(
      c(alcever_S1, alcever_S2, alcever_S3, alcever_S4, alcever_S6, alcever_S7),
      never_from_alcever,
      .names = "never_{.col}"
    ),
    across(
      c(audita25_raw, audita32_raw),
      never_from_audita,
      .names = "never_{.col}"
    ),
    # Derive never drinkers:
    # - 1 = all items observed & all never
    # - 0 = at least one drinker
    # - NA = no drinkers but some/all missing
    never_alc = case_when(
      # any 0 -> drinker
      if_any(starts_with("never_"), ~ dplyr::coalesce(.x == 0, FALSE)) ~ 0L,
      # all observed & 1
      if_all(starts_with("never_"), ~ !is.na(.x) & .x == 1) ~ 1L,
      # otherwise NA
      .default = NA
    ),
    # First age -> use the first age when not missing.
    # If never drinking, set to 99.
    # Anything else is missing.
    alcfst = case_when(
      !is.na(first_age_raw) ~ first_age_raw,
      never_alc == 1 ~ 99,
      .default = -8
    )
  ) |>
  select(-starts_with(c("ever", "never")), -first_age_raw)

# Add labels
alc_first_age_rec <- alc_first_age_rec %>%
  mutate(
    alcfst = labelled(
      alcfst,
      labels = c(
        "Age 14" = 14,
        "Age 15" = 15,
        "Age 16" = 16,
        "Age 17" = 17,
        "Age 19" = 19,
        "Age 20" = 20,
        "Age 25" = 25,
        "Age 32" = 32,
        "Never had alcohol" = 99,
        common_missing_labels
      )
    )
  )

## Alcohol frequency --------------------------------------------------------------------

# Helpers
recode_freq <- function(x, sweep, ever) {
  case_when(
    sweep %in% c("S2", "S3", "S4") ~ case_when(
      x == 1 ~ 4, # most days
      x == 2 ~ 3, # once or twice a week
      x %in% c(3, 4) ~ 2, # once to three times a month
      x == 5 ~ 1, # once every couple of month
      x == 6 ~ 0, # less often/not at all
      ever == 2 ~ 0, # less often/not at all
      # Missing  values:
      x %in% c(-97, -92) | ever %in% c(-97, -92) ~ -9, # refusal if either refused,
      x == -1 | ever == -1 ~ -8, # dk/missing info
      x == -91 | ever == -91 ~ -1, # not applicable
      .default = -3
    ),
    sweep %in% c("S6", "S7") ~ case_when(
      x %in% c(1, 2) ~ 4,
      x %in% c(3, 4) ~ 3,
      x == 5 ~ 2,
      x == 6 ~ 1,
      x %in% c(7, 8) ~ 0,
      ever == 2 ~ 0,
      x == -997 ~ -2,
      # Missing  values:
      x %in% c(-97, -92) | ever %in% c(-97, -92) ~ -9, # refusal if either refused,
      x == -1 | ever == -1 ~ -8, # dk/missing info
      x == -91 | ever == -91 ~ -1, # not applicable
      .default = -3
    )
  )
}

# Recode frequency variables
alc_freq_rec <- alc_first_age_rec %>%
  mutate(
    alcfreq14 = case_when(
      alcfreq_S1 == 1 ~ 4,
      alcfreq_S1 == 2 ~ 3,
      alcfreq_S1 == 3 ~ 2,
      alcfreq_S1 == 4 ~ 2,
      alcfreq_S1 == 5 ~ 1,
      alcfreq_S1 == 6 ~ 0,
      alcever_S1 == 2 ~ 0,
      alcmon_S1 == 2 ~ 0,
      # Missing  values:
      alcfreq_S1 %in%
        c(-97, -92) |
        alcmon_S1 %in% c(-97, -92) |
        alcever_S1 %in% c(-97, -92) ~ -9, # refusal if either refused,
      alcfreq_S1 == -1 | alcmon_S1 == -1 | alcever_S1 == -1 ~ -8, # dk/missing info
      alcfreq_S1 == -91 | alcmon_S1 == -91 | alcever_S1 == -91 ~ -1, # not applicable
      .default = -3
    ),
    alcfreq15 = recode_freq(alcfreq_S2, "S2", alcever_S2),
    alcfreq16 = recode_freq(alcfreq_S3, "S3", alcever_S3),
    alcfreq17 = recode_freq(alcfreq_S4, "S4", alcever_S4),
    alcfreq19 = recode_freq(alcfreq_S6, "S6", alcever_S6),
    alcfreq20 = recode_freq(alcfreq_S7, "S7", alcever_S7),
  ) %>%
  # Add labels
  mutate(
    across(
      c(alcfreq14, alcfreq15, alcfreq16, alcfreq17, alcfreq19, alcfreq20),
      ~ labelled(
        .x,
        labels = c(
          "Less often/not at all" = 0,
          "Once every couple of months" = 1,
          "Once to three times a month" = 2,
          "Once or twice a week" = 3,
          "Most days" = 4,
          common_missing_labels
        )
      )
    )
  )

# Check
## Cross-tabs for alcfreq
{
  # Build separate cross-tabs for each sweep with alcfreq_S*, alcever_S* first and alcfreq* last
  freq_map <- list(
    S1 = c("alcever_S1", "alcfreq_S1", "alcmon_S1", "alcfreq14"),
    S2 = c("alcever_S2", "alcfreq_S2", "alcfreq15"),
    S3 = c("alcever_S3", "alcfreq_S3", "alcfreq16"),
    S4 = c("alcever_S4", "alcfreq_S4", "alcfreq17"),
    S6 = c("alcever_S6", "alcfreq_S6", "alcfreq19"),
    S7 = c("alcever_S7", "alcfreq_S7", "alcfreq20")
  )

  alc_freq_crosstabs <- purrr::imap(freq_map, function(cols, sweep) {
    alc_freq_rec %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(cols))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
  })
}

## AUDIT-C --------------------------------------------------------------------

alc_all_clean <- alc_freq_rec %>%
  mutate(
    audita25 = case_when(
      audita25_raw > 0 ~ audita25_raw - 1,
      audita25_raw < 0 ~ audita25_raw,
      is.na(audita25_raw) ~ -3,
    ),
    audita32 = case_when(
      audita32_raw > 0 ~ audita32_raw - 1,
      audita32_raw < 0 ~ audita32_raw,
      is.na(audita32_raw) ~ -3,
    )
  ) %>%
  mutate(
    auditb25 = case_when(
      audita25 == 0 ~ 0,
      audita25 > 0 & auditb25_raw > 0 ~ auditb25_raw,
      auditb25_raw < 0 ~ auditb25_raw,
      is.na(auditb25_raw) ~ -3
    ),
    auditb32 = case_when(
      audita32 == 0 ~ 0,
      audita32 > 0 & auditb32_raw > 0 ~ auditb32_raw,
      auditb32_raw < 0 ~ auditb32_raw,
      is.na(auditb32_raw) ~ -3
    ),
    auditc25 = case_when(
      audita25 == 0 ~ 0,
      is.na(auditc25_raw) ~ -3,
      auditc25_raw < 0 ~ auditc25_raw,
      TRUE ~ auditc25_raw - 1
    ),
    auditc32 = case_when(
      audita32 == 0 ~ 0,
      is.na(auditc32_raw) ~ -3,
      auditc32_raw < 0 ~ auditc32_raw,
      TRUE ~ auditc32_raw - 1
    )
  ) %>%
  mutate(
    across(
      c(audita25, audita32),
      ~ labelled(
        .x,
        labels = c(
          "Never" = 0,
          "Monthly or less" = 1,
          "2–4 times a month" = 2,
          "2–3 times a week" = 3,
          "4 or more times a week" = 4,
          common_missing_labels
        )
      )
    ),
    across(
      c(auditb25, auditb32),
      ~ labelled(
        .x,
        labels = c(
          "0" = 0,
          "1–2 drinks" = 1,
          "3–4 drinks" = 2,
          "5–6 drinks" = 3,
          "7–9 drinks" = 4,
          "10+" = 5,
          common_missing_labels
        )
      )
    ),
    across(
      c(auditc25, auditc32),
      ~ labelled(
        .x,
        labels = c(
          "Never" = 0,
          "Less than monthly" = 1,
          "Monthly" = 2,
          "Weekly" = 3,
          "Daily or almost daily" = 4,
          common_missing_labels
        )
      )
    )
  )

alc_all_clean <- alc_all_clean %>%
  select(
    NSID,
    alcfst,
    alcfreq14,
    alcfreq15,
    alcfreq16,
    alcfreq17,
    alcfreq19,
    alcfreq20,
    audita25,
    audita32,
    auditb25,
    auditb32,
    auditc25,
    auditc32
  )

# Drug Use --------------------------------------------------------------------

# Load drug use data from relevant sweeps
## Naming convention:
## canevr - ever used cannabis
## now_cann - currently using cannabis
## yr_cann - used cannabis in past 12 months
## othevr - ever used other drugs
## now_oth - currently using other drugs
## yr_oth - used other drugs in past 12 months
drug_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, canevr14 = W1canntryYP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, canevr15 = W2canntryYP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, canevr16 = W3canntryYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, canevr17 = W4CannTryYP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(
      NSID,
      canevr19 = W6DrugYP0a,
      othevr19 = W6DrugYP0b,
      now_cann19 = W6DrugOftenYP0a,
      now_oth19 = W6DrugOftenYP0b
    ),
  S7 = ns_data[["S7youngperson"]] %>%
    select(
      NSID,
      canevr20 = W7DrugYP1YP0a,
      othevr20 = W7DrugYP1YP0b,
      now_cann20 = W7DrugOftenYP0a,
      now_oth20 = starts_with("W7DrugOftenYP0") & !any_of("W7DrugOftenYP0a")
    ),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(
      NSID,
      canevr25 = W8DRUGYP10A,
      othevr25 = starts_with("W8DRUGYP10") & !any_of("W8DRUGYP10A"),
      yr_cann25 = W8DRUGYP20A,
      yr_oth25 = starts_with("W8DRUGYP20") & !any_of("W8DRUGYP20A"),
      now_cann25 = W8DRUGOFTEN0A,
      now_oth25 = starts_with("W8DRUGOFTEN0") & !any_of("W8DRUGOFTEN0A")
    ),
  S9 = ns_data[["S9maininterview"]] %>%
    select(
      NSID,
      canevr32 = W9DRUGYP10A,
      othevr32 = starts_with("W9DRUGYP1") & !any_of("W9DRUGYP10A"),
      yr_cann32 = W9DRUGYP20A,
      yr_oth32 = starts_with("W9DRUGYP2") & !any_of("W9DRUGYP20A"),
      now_cann32 = W9DRUGOFTEN0A,
      now_oth32 = starts_with("W9DRUGOFTEN0") & !any_of("W9DRUGOFTEN0A")
    )
)

# Merge all datasets
drug_all <- reduce(drug_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all variable names for simpler re-coding & cross-checks
  rename_with(
    .fn = ~ stringr::str_c(.x, "_raw"),
    .cols = !contains("NSID")
  )

## Recode original response options --------------------------------------------------------------------

# functions: Recode function preserving missing values
# recode for whether using drug ever for each sweep
recode_drugbin1_7 <- function(x) {
  case_when(
    x %in% c(1) ~ 1, # yes
    x %in% c(2, 0) ~ 0, # no
    x %in% c(-97, -92) ~ -9,
    x %in% c(-1, -96) ~ -8,
    x %in% c(-997) ~ -2,
    is.na(x) | x == -99 ~ -3,
    TRUE ~ -2
  )
}

# recode for how sweep 7 about often using drug now to whether using drug now
recode_drugoft7 <- function(x) {
  case_when(
    x == 1 ~ 0,
    x %in% c(2, 3, 4) ~ 1,
    x %in% c(-97, -92) ~ -9,
    x %in% c(-1, -96) ~ -8,
    x %in% c(-997) ~ -2,
    is.na(x) | x == -99 ~ -3,
    TRUE ~ -2
  )
}

# recode for sweeps 8 and 9 about whether using drug ever/now
recode_drugbin89 <- function(x) {
  case_when(
    x %in% c(1) ~ 1,
    x %in% c(2, 0) ~ 0,
    x < 0 ~ x,
    is.na(x) ~ -3,
    TRUE ~ -2
  )
}

# Recode cannabis & other drug variables
drug_rec <- drug_all %>%
  mutate(
    across(
      c(
        canevr14_raw,
        canevr15_raw,
        canevr16_raw,
        canevr17_raw,
        canevr19_raw,
        canevr20_raw,
        now_cann19_raw,
        othevr19_raw,
        othevr20_raw,
        now_oth19_raw
      ),
      recode_drugbin1_7,
      # Remove '_raw' suffix from new variable names
      .names = "{stringr::str_remove(.col, '_raw$')}"
    ),
    across(
      c(now_cann20_raw, starts_with("now_oth20")),
      recode_drugoft7,
      # Remove '_raw' suffix from new variable names
      .names = "{stringr::str_remove(.col, '_raw$')}"
    ),
    across(
      c(
        canevr25_raw,
        canevr32_raw,
        yr_cann25_raw,
        yr_cann32_raw,
        now_cann25_raw,
        now_cann32_raw,
        starts_with("yr_oth25"),
        starts_with("yr_oth32"),
        starts_with("othevr25"),
        starts_with("othevr32"),
        starts_with("now_oth25"),
        starts_with("now_oth32")
      ),
      recode_drugbin89,
      # Remove '_raw' suffix from new variable names
      .names = "{stringr::str_remove(.col, '_raw$')}"
    )
  )

# Check: cross-tabs
drug_pairs <- tibble::tibble(
  raw_var = names(drug_rec) |>
    stringr::str_subset("_raw$")
) |>
  dplyr::mutate(
    rec_var = stringr::str_remove(raw_var, "_raw$"),
    has_rec = rec_var %in% names(drug_rec)
  ) |>
  dplyr::filter(has_rec) |>
  dplyr::select(raw_var, rec_var)

drug_crosstabs <- drug_pairs |>
  purrr::pmap(function(raw_var, rec_var) {
    drug_rec |>
      dplyr::count(
        raw = .data[[raw_var]],
        rec = .data[[rec_var]],
        name = "n"
      ) |>
      dplyr::mutate(
        raw_var = raw_var,
        rec_var = rec_var,
        .before = 1
      )
  })

## Derive 'Other' --------------------------------------------------------------------

# Helper function: Derive 'Other' drug use within a sweep.
## Used derive a single variable indicating if a person used drugs other than cannabis within a sweep.
## It uses the indicator for whether ANY drug was used (based on separate yes/no indicators for each drug).
## Coded as 1 = 'yes' if any drug was used.
## Else, coded as 0 = 'no' if all drugs were reported as not used (conservative).
## Else, missing values follow a hierarchy.
derive_drug_other_within <- function(cols) {
  dplyr::case_when(
    ## If any variable reported as yes -> 'yes' (1).
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == 1, FALSE)) ~ 1L,
    ## ELSE: If all drugs reported as NOT having used -> 'no' (0).
    dplyr::if_all({{ cols }}, \(x) !is.na(x) & x == 0) ~ 0L,
    ## ELSE: If any -9 (refusal) -> -9.
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == -9, FALSE)) ~ -9L,
    ## ELSE: If any -8 (dk/insufficient info) -> -8.
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == -8, FALSE)) ~ -8L,
    ## ELSE: If any -1 (not applicable) -> -1.
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == -1, FALSE)) ~ -1L,
    ## ELSE: -3 (not interviewed/asked etc.)
    TRUE ~ -3L
  )
}

drug_rec <- drug_rec |>
  dplyr::mutate(
    othevr25 = derive_drug_other_within(
      starts_with("othevr25") & !ends_with("raw")
    ),
    othevr32 = derive_drug_other_within(
      starts_with("othevr32") & !ends_with("raw")
    ),
    yr_oth25 = derive_drug_other_within(
      starts_with("yr_oth25") & !ends_with("raw")
    ),
    yr_oth32 = derive_drug_other_within(
      starts_with("yr_oth32") & !ends_with("raw")
    ),
    now_oth20 = derive_drug_other_within(
      starts_with("now_oth20") & !ends_with("raw")
    ),
    now_oth25 = derive_drug_other_within(
      starts_with("now_oth25") & !ends_with("raw")
    ),
    now_oth32 = derive_drug_other_within(
      starts_with("now_oth32") & !ends_with("raw")
    )
  )

## Derive EVER used --------------------------------------------------------------------

# Derive 'ever used' for cannabis and other drugs across sweeps.
# The function takes indicators from each sweep (1 = reported EVER using drug, 0 = reported not EVER using drug)
# and derives a single EVER indicator.
# Coded as 1 = 'yes' if drug was EVER used.
# Else, coded as 0 = 'no' if any sweep reported not EVER using drug (liberal).
# Otherwise, missing values follow a hierarchy.
derive_drug_ever_across <- function(cols) {
  dplyr::case_when(
    ## If any sweep reported as ever used -> 'yes' (1).
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == 1, FALSE)) ~ 1L,
    ## ELSE: If any sweep reported as NOT having ever used -> 'no' (0).
    dplyr::if_any({{ cols }}, \(x) x == 0) ~ 0L,
    ## ELSE: If any -9 (refusal) -> -9.
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == -9, FALSE)) ~ -9L,
    ## ELSE: If any -8 (dk/insufficient info) -> -8.
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == -8, FALSE)) ~ -8L,
    ## ELSE: If any -1 (not applicable) -> -1.
    dplyr::if_any({{ cols }}, \(x) dplyr::coalesce(x == -1, FALSE)) ~ -1L,
    ## ELSE: -3 (not interviewed/asked etc.)
    TRUE ~ -3L
  )
}

drug_rec_ever <- drug_rec %>%
  mutate(
    drgcnbevr = derive_drug_ever_across(
      starts_with("canevr") & !ends_with("raw")
    ),
    drgothevr = derive_drug_ever_across(c(
      othevr19,
      othevr20,
      othevr25,
      othevr32
    ))
  )

## Derive first time use --------------------------------------------------------------------

# This variable will record the first known age of using cannabis/other drugs.
first_wave_age <- c(14, 15, 16, 17, 19, 20, 25, 32)
cann_vars <- c(
  "canevr14",
  "canevr15",
  "canevr16",
  "canevr17",
  "canevr19",
  "canevr20",
  "canevr25",
  "canevr32"
)
oth_vars <- c("othevr19", "othevr20", "othevr25", "othevr32")

drug_rec_first <- drug_rec_ever |>
  mutate(
    drgcnbfst = case_when(
      canevr14 == 1 ~ 14L,
      canevr15 == 1 ~ 15L,
      canevr16 == 1 ~ 16L,
      canevr17 == 1 ~ 17L,
      canevr19 == 1 ~ 19L,
      canevr20 == 1 ~ 20L,
      canevr25 == 1 ~ 25L,
      canevr32 == 1 ~ 32L,

      # conservative "never": all included sweeps are exactly 0
      if_all(all_of(cann_vars), ~ .x == 0) ~ 99L,

      .default = -3L
    ),
    drgothfst = case_when(
      othevr19 == 1 ~ 19L,
      othevr20 == 1 ~ 20L,
      othevr25 == 1 ~ 25L,
      othevr32 == 1 ~ 32L,

      if_all(all_of(oth_vars), ~ .x == 0) ~ 99L,

      .default = -3L
    )
  )

## Derive current use --------------------------------------------------------------------

drug_rec_current <- drug_rec_first %>%
  mutate(
    drgcnbnw19 = case_when(
      canevr19 == 0 ~ 0,
      TRUE ~ now_cann19
    ),
    drgcnbnw20 = case_when(
      canevr20 == 0 ~ 0,
      TRUE ~ now_cann20
    ),
    drgcnbnw25 = case_when(
      canevr25 == 0 ~ 0,
      yr_cann25 == 0 ~ 0,
      TRUE ~ now_cann25
    ),
    drgcnbnw32 = case_when(
      canevr32 == 0 ~ 0,
      yr_cann32 == 0 ~ 0,
      TRUE ~ now_cann32
    ),

    drgothnw19 = case_when(
      othevr19 == 0 ~ 0,
      TRUE ~ now_oth19
    ),
    drgothnw20 = case_when(
      othevr20 == 0 ~ 0,
      TRUE ~ now_oth20
    ),
    drgothnw25 = case_when(
      othevr25 == 0 ~ 0,
      yr_oth25 == 0 ~ 0,
      TRUE ~ now_oth25
    ),
    drgothnw32 = case_when(
      othevr32 == 0 ~ 0,
      yr_oth32 == 0 ~ 0,
      TRUE ~ now_oth32
    )
  )

# Add labels and select variables
drug_all_clean <- drug_rec_current %>%
  select(-ends_with("raw")) %>%
  mutate(
    across(
      c(drgcnbevr, drgothevr, starts_with("drgcnbnw"), starts_with("drgothnw")),
      ~ labelled(
        .x,
        labels = c(
          "No" = 0,
          "Yes" = 1,
          common_missing_labels
        )
      )
    ),
    across(
      c(drgcnbfst, drgothfst),
      ~ labelled(
        .x,
        labels = c(
          "Age 14" = 14,
          "Age 15" = 15,
          "Age 16" = 16,
          "Age 17" = 17,
          "Age 19" = 19,
          "Age 20" = 20,
          "Age 25" = 25,
          "Age 32" = 32,
          "Never used" = 99,
          common_missing_labels
        )
      )
    )
  ) %>%
  select(
    NSID,
    drgcnbevr,
    drgcnbfst,
    starts_with("drgcnbnw"),
    drgothevr,
    drgothfst,
    starts_with("drgothnw")
  )

# Exercise --------------------------------------------------------------------
# Load relevant sweep files and select variables
exercise_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, spt14 = W1sportYP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, spt15 = W2sportYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, spt17 = W4SportYP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, spt19 = W6SportYP),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, spt20 = W7SportYP),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, spt25 = W8EXERCISE),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, spt32 = W9EXERCISEH)
)

# Merge all datasets
spt_all <- reduce(exercise_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all variable names for simpler re-coding & cross-checks
  rename_with(
    .fn = ~ stringr::str_c(.x, "_raw"),
    .cols = !contains("NSID")
  )

# Recode function
recode_exercise <- function(x) {
  case_when(
    x %in% c(1, 2, 3) ~ x - 1, # Keep as is
    x %in% c(4, 5, 6) ~ 3, # 4- 6 = less than once a week/hardly ever/never
    x == -92 ~ -9, # Refused
    x == -91 ~ -1, # Not applicable / insufficient info
    x == -99 ~ -3, # Not interviewed
    x == -1 ~ -8, # Don't know
    x %in% c(-998, -997, -995) ~ -2, # Script error/information lost
    is.na(x) ~ -3
  )
}

# Re-coding
## At sweeps 8-9, the question wording changed,
## asking about the number of days per week doing exercise for 30 mins or more.
## For these sweeps, re-coding was done as follows:
## - 5-7 days = "most days" (0)
## - 2-4 days = "more than once a week" (1)
## - 1 day = "once a week" (2)
## - 0 days = "less than once a week/hardly ever/never" (3)
spt_rec <- spt_all %>%
  mutate(
    spt14 = recode_exercise(spt14_raw),
    spt15 = recode_exercise(spt15_raw),
    spt17 = recode_exercise(spt17_raw),
    spt19 = recode_exercise(spt19_raw),
    spt20 = recode_exercise(spt20_raw),
    spt25 = case_when(
      # values from 0–7 days
      spt25_raw %in% c(5, 6, 7) ~ 0,
      spt25_raw %in% c(2, 3, 4) ~ 1,
      spt25_raw == 1 ~ 2,
      spt25_raw == 0 ~ 3,
      spt25_raw == -9 ~ -9,
      spt25_raw == -8 ~ -8,
      spt25_raw == -1 ~ -1,
      is.na(spt25_raw) ~ -3
    ),
    spt32 = case_when(
      spt32_raw %in% c(5, 6, 7) ~ 0,
      spt32_raw %in% c(2, 3, 4) ~ 1,
      spt32_raw == 1 ~ 2,
      spt32_raw == 0 ~ 3,
      spt32_raw == -9 ~ -9,
      spt32_raw == -8 ~ -8,
      spt32_raw == -1 ~ -1,
      is.na(spt32_raw) | spt32_raw == -3 ~ -3
    )
  ) %>%
  mutate(
    across(
      c(starts_with("spt") & !ends_with("raw")),
      ~ labelled(
        .x,
        labels = c(
          "Most days" = 0,
          "More than once a week" = 1,
          "Once a week" = 2,
          "Less than once a week/hardly ever/never" = 3,
          common_missing_labels
        )
      )
    )
  )

# Cross-tabs
spt_rec %>%
  count(spt14_raw, spt14)

spt_rec %>%
  count(spt15_raw, spt15)

spt_rec %>%
  count(spt17_raw, spt17)

spt_rec %>%
  count(spt19_raw, spt19)

spt_rec %>%
  count(spt20_raw, spt20)

spt_rec %>%
  count(spt25_raw, spt25)

spt_rec %>%
  count(spt32_raw, spt32)

spt_all <- spt_rec %>%
  select(NSID, spt14, spt15, spt17, spt19, spt20, spt25, spt32)

# School absence --------------------------------------------------------------------

# Load relevant sweep files and select variables
absence_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, abs1m14 = W1abs1myMP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, abs1m15 = W2abs1myMP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, abs1m16 = W3abs1myMP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID)
)

# Merge the datasets by NSID
absence_all <- reduce(absence_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all variable names for simpler re-coding & cross-checks
  rename_with(
    .fn = ~ stringr::str_c(.x, "_raw"),
    .cols = !contains("NSID")
  )

# Recode function for harmonised values
recode_absence <- function(x) {
  case_when(
    x == 1 ~ 1, # yes
    x == 2 ~ 0, # no
    x %in% c(-97, -92) ~ -9,
    x %in% c(-91) ~ -1,
    x %in% c(-1) ~ -8,
    x %in% c(-998, -997, -995) ~ -2,
    .default = -3
  )
}

# Apply recode to each sweep
absence_rec <- absence_all %>%
  mutate(
    abs1m14 = recode_absence(abs1m14_raw),
    abs1m15 = recode_absence(abs1m15_raw),
    abs1m16 = recode_absence(abs1m16_raw)
  ) %>%
  mutate(across(
    starts_with("abs1m") & !ends_with("raw"),
    ~ labelled(
      .x,
      labels = c(
        "No" = 0,
        "Yes" = 1,
        common_missing_labels
      )
    )
  ))

absence_rec %>%
  count(abs1m14_raw, abs1m14)

absence_rec %>%
  count(abs1m15_raw, abs1m15)

absence_rec %>%
  count(abs1m16_raw, abs1m16)

# Suspended/Expelled --------------------------------------------------------------------

# Load suspension and expulsion variables from each sweep
suspend_expel_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, susp14 = W1suspendMP, expl14 = W1expelMP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, susp15 = W2SuspendMP, expl15 = W2ExpelMP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, susp16 = W3suspendMP, expl16 = W3expelMP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, expl17 = W4Expel1YP, susp17 = W4Expel2YP)
)

# Merge all datasets by NSID
suspend_expel_all <- reduce(suspend_expel_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all variable names for simpler re-coding & cross-checks
  rename_with(
    .fn = ~ stringr::str_c(.x, "_raw"),
    .cols = !contains("NSID")
  )

# Recode function
recode_school_discipline <- function(x) {
  case_when(
    x == 1 ~ 1, # yes
    x == 2 ~ 0, # no
    x %in% c(-97, -92) ~ -9,
    x %in% c(-91) ~ -1,
    x %in% c(-1) ~ -8,
    x %in% c(-99) ~ -3,
    is.na(x) ~ -3,
    TRUE ~ -3
  )
}

# Apply recoding
suspend_expel_rec <- suspend_expel_all %>%
  mutate(
    susp14 = recode_school_discipline(susp14_raw),
    susp15 = recode_school_discipline(susp15_raw),
    susp16 = recode_school_discipline(susp16_raw),
    susp17 = recode_school_discipline(susp17_raw),
    expl14 = recode_school_discipline(expl14_raw),
    expl15 = recode_school_discipline(expl15_raw),
    expl16 = recode_school_discipline(expl16_raw),
    expl17 = recode_school_discipline(expl17_raw)
  ) %>%
  mutate(across(
    c(
      starts_with("susp") & !ends_with("raw"),
      starts_with("expl") & !ends_with("raw")
    ),
    ~ labelled(
      .x,
      labels = c(
        "No" = 0,
        "Yes" = 1,
        common_missing_labels
      )
    )
  ))

# Cross-tabs
suspend_expel_rec %>%
  count(susp14_raw, susp14)
suspend_expel_rec %>%
  count(susp15_raw, susp15)
suspend_expel_rec %>%
  count(susp16_raw, susp16)
suspend_expel_rec %>%
  count(susp17_raw, susp17)
suspend_expel_rec %>%
  count(expl14_raw, expl14)
suspend_expel_rec %>%
  count(expl15_raw, expl15)
suspend_expel_rec %>%
  count(expl16_raw, expl16)
suspend_expel_rec %>%
  count(expl17_raw, expl17)

suspend_expel_all <- suspend_expel_rec %>%
  select(NSID, starts_with("susp"), starts_with("expl"))

# Truancy --------------------------------------------------------------------
# Load original variables from S1–S4
truancy_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, trua14_ever = W1truantYP, trua14_type = W1truant1YP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, trua15_ever = W2truantYP, trua15_type = W2truant1YP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, trua16_ever = W3truantYP, trua16_type = W3truant1YP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, trua17_raw = W4TruantYP)
)

# Merge by NSID
truancy_all <- reduce(truancy_vars, full_join, by = "NSID")

# Recode S1–S3 type using combined ever-truanted info
recode_truancy_early <- function(ever, type) {
  case_when(
    ever == 2 ~ 0, # never truant
    type == 1 ~ 1, # for weeks at a time
    type == 2 ~ 2, # several days at a time
    type == 3 ~ 3, # particular days or lessons
    type == 4 ~ 4, # odd day or lesson
    # If missing, use hierarchy derived from both ever and type:
    # Else if either is refusal -> refusal (-9)
    ever %in% c(-92, -97) | type %in% c(-92, -97) ~ -9,
    # Else if either is don't know / insufficient info -> -8
    ever == -1 | type == -1 ~ -8,
    # Else is not interviewed/asked etc. -> -3
    .default = -3
  )
}

# Recode S4 directly
recode_truancy_s4 <- function(x) {
  case_when(
    x == 5 ~ 0,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x %in% c(-1) ~ -8,
    x %in% c(-97, -92) ~ -9,
    x == -99 ~ -3,
    x == -91 ~ -1,
    is.na(x) ~ -3,
    TRUE ~ -3
  )
}

# Apply recoding
truancy_rec <- truancy_all %>%
  mutate(
    trua14 = recode_truancy_early(trua14_ever, trua14_type),
    trua15 = recode_truancy_early(trua15_ever, trua15_type),
    trua16 = recode_truancy_early(trua16_ever, trua16_type),
    trua17 = recode_truancy_s4(trua17_raw)
  ) %>%
  mutate(across(
    c(trua14, trua15, trua16, trua17),
    ~ labelled(
      .x,
      labels = c(
        "Never played truant" = 0,
        "For weeks at a time" = 1,
        "Several days at a time" = 2,
        "Particular days or lessons" = 3,
        "Odd day or lesson" = 4,
        common_missing_labels
      )
    )
  ))

# Cross-tabs
truancy_rec %>%
  count(trua14_ever, trua14_type, trua14)

truancy_rec %>%
  count(trua15_ever, trua15_type, trua15)

truancy_rec %>%
  count(trua16_ever, trua16_type, trua16)

truancy_rec %>%
  count(trua17, trua17_raw)

truancy_all <- truancy_rec %>%
  select(NSID, trua14, trua15, trua16, trua17)

# Police Contact --------------------------------------------------------------------

# Load data for police contact
police_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, pol14 = W1Police1MP, polcnt14 = W1police2MP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, pol15 = W2police1MP, polcnt15 = W2Police2MP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, pol16 = W3police1MP, polcnt16 = W3police2MP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, pol17 = W4Police1MP, polcnt17 = W4Police2MP),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(
      NSID,
      polwrn25 = W8CJSCONTACT0A,
      polars25 = W8CJSCONTACT0B,
      polcau25 = W8CJSCONTACT0C,
      polglt25 = W8CJSCONTACT0D,
      polpnd25 = W8CJSCONTACT0E
    ),
  S9 = ns_data[["S9maininterview"]] %>%
    select(
      NSID,
      polwrn32 = W9CJSCONTACT0A,
      polars32 = W9CJSCONTACT0B,
      polcau32 = W9CJSCONTACT0C,
      polglt32 = W9CJSCONTACT0D,
      polpnd32 = W9CJSCONTACT0E
    )
)

# Merge datasets
police_all <- reduce(police_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all variable names for simpler re-coding & cross-checks
  rename_with(
    .fn = ~ stringr::str_c(.x, "_raw"),
    .cols = !contains("NSID")
  )

# Recode function for binary variables (pol15,16)
recode_pol <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    x %in% c(-97, -92) ~ -9,
    x == -91 ~ -1,
    x %in% c(-1) ~ -8,
    x %in% c(-998, -997, -995) ~ -2,
    x %in% c(-99) ~ -3,
    .default = -3
  )
}

# Recode function for contact counts
recode_cnt <- function(x, ever) {
  case_when(
    ever %in% c(2, 3) ~ 0,
    x >= 0 ~ x,
    # Use hierarchy for missing values:
    ever %in% c(-92, -97) | x %in% c(-92, -97) ~ -9,
    ever == -1 | x == -1 ~ -8,
    # Script errors/information lost:
    x %in% c(-998, -997, -995) ~ -2,
    # Else is not interviewed/asked etc. -> -3
    .default = -3
  )
}

## Police contact --------------------------------------------------------------------
police_rec_contact <- police_all %>%
  mutate(
    # Police contact - binary:
    pol14 = case_when(
      pol14_raw %in% c(1, 3) ~ 1,
      pol14_raw == 2 ~ 0,
      pol14_raw %in% c(-97, -92) ~ -9,
      pol14_raw == -91 ~ -1,
      pol14_raw %in% c(-1) ~ -8,
      pol14_raw %in% c(-99) ~ -3,
      .default = -3
    ),
    pol15 = recode_pol(pol15_raw),
    pol16 = recode_pol(pol16_raw),
    pol17 = case_when(
      pol17_raw %in% c(1, 3) ~ 1,
      pol17_raw == 2 ~ 0,
      pol17_raw %in% c(-97, -92) ~ -9,
      pol17_raw == -91 ~ -1,
      pol17_raw %in% c(-1) ~ -8,
      pol17_raw %in% c(-99) ~ -3,
      .default = -3
    ),
    # Police contact - count:
    polcnt14 = recode_cnt(polcnt14_raw, pol14_raw),
    polcnt15 = recode_cnt(polcnt15_raw, pol15_raw),
    polcnt16 = recode_cnt(polcnt16_raw, pol16_raw),
    polcnt17 = recode_cnt(polcnt17_raw, pol17_raw),
    # Add labels
    across(
      c(pol14, pol15, pol16, pol17),
      ~ labelled(
        .x,
        labels = c(
          "No" = 0,
          "Yes/not in last 3 years" = 1,
          common_missing_labels
        )
      )
    ),
    across(
      c(starts_with("polcnt") & !ends_with("raw")),
      ~ labelled(
        .x,
        labels = c(
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    )
  )


# Cross-tabs
police_rec_contact %>%
  count(pol14_raw, pol14)
police_rec_contact %>%
  count(pol15_raw, pol15)
police_rec_contact %>%
  count(pol16_raw, pol16)
police_rec_contact %>%
  count(pol17_raw, pol17)

police_rec_contact %>%
  count(pol14_raw, polcnt14_raw, polcnt14) %>%
  print(n = Inf)
police_rec_contact %>%
  count(pol15_raw, polcnt15_raw, polcnt15) %>%
  print(n = Inf)
# For polcnt16_raw, -1 is dk/insufficient info (checked the data dictionary, label only missing in the Stata file).
police_rec_contact %>%
  count(pol16_raw, polcnt16_raw, polcnt16) %>%
  print(n = Inf)
police_rec_contact %>%
  count(pol17_raw, polcnt17_raw, polcnt17) %>%
  print(n = Inf)

## Police warning, arrest, caution  --------------------------------------------------------------------

police_rec_warning <- police_rec_contact %>%
  mutate(
    across(
      starts_with("polwrn"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      ),
      .names = "{stringr::str_remove(.col, '_raw$')}"
    ),
    across(
      starts_with("polars"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      ),
      .names = "{stringr::str_remove(.col, '_raw$')}"
    ),
    across(
      starts_with("polcau"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      ),
      .names = "{stringr::str_remove(.col, '_raw$')}"
    ),
    # Add labels
    across(
      c(
        starts_with("polwrn") & !ends_with("raw"),
        starts_with("polars") & !ends_with("raw"),
        starts_with("polcau") & !ends_with("raw")
      ),
      ~ labelled(
        .x,
        labels = c(
          "No" = 0,
          "Yes" = 1,
          common_missing_labels
        )
      )
    )
  )

# Cross-tabs
pol_warn_pairs <- tibble::tibble(
  raw_var = names(police_rec_warning) |>
    stringr::str_subset("^(polwrn|polars|polcau).*_raw$")
) |>
  dplyr::mutate(
    rec_var = stringr::str_remove(raw_var, "_raw$"),
    has_rec = rec_var %in% names(police_rec_warning)
  ) |>
  dplyr::filter(has_rec) |>
  dplyr::select(raw_var, rec_var)

police_warn_crosstabs <- pol_warn_pairs |>
  purrr::pmap(function(raw_var, rec_var) {
    police_rec_warning |>
      dplyr::count(
        raw = .data[[raw_var]],
        rec = .data[[rec_var]],
        name = "n"
      ) |>
      dplyr::mutate(
        raw_var = raw_var,
        rec_var = rec_var,
        .before = 1
      )
  })

police_warn_crosstabs

## Found guilty, penalty notice  --------------------------------------------------------------------

police_rec_guilty <- police_rec_warning %>%
  mutate(
    across(
      starts_with("polglt"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      ),
      .names = "{stringr::str_remove(.col, '_raw$')}"
    ),
    across(
      starts_with("polpnd"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      ),
      .names = "{stringr::str_remove(.col, '_raw$')}"
    ),
    # Add labels
    across(
      c(
        starts_with("polglt") & !ends_with("raw"),
        starts_with("polpnd") & !ends_with("raw")
      ),
      ~ labelled(
        .x,
        labels = c(
          "No" = 0,
          "Yes" = 1,
          common_missing_labels
        )
      )
    )
  )

# Cross-tabs
police_rec_guilty %>%
  count(polglt25_raw, polglt25)
police_rec_guilty %>%
  count(polglt32_raw, polglt32)
police_rec_guilty %>%
  count(polpnd25_raw, polpnd25)
police_rec_guilty %>%
  count(polpnd32_raw, polpnd32)

police_all <- police_rec_guilty %>%
  select(!ends_with("raw")) %>%
  select(
    NSID,
    pol14,
    pol15,
    pol16,
    pol17,
    polcnt14,
    polcnt15,
    polcnt16,
    polcnt17,
    starts_with("polwrn"),
    starts_with("polars"),
    starts_with("polcau"),
    starts_with("polglt"),
    starts_with("polpnd")
  )

# Bully --------------------------------------------------------------------
# Load and harmonise bullying variables across sweeps 1–4, 7–8
bully_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, bul14 = W1bulrc),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, bul15 = W2bulrc),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, bul16 = W3bulrc),
  S4 = ns_data[["S4youngperson"]] %>%
    select(
      NSID,
      bul17_1 = W4V1perSYP,
      bul17_2 = W4ViolentYP,
      bul17_3 = W4HurtYP,
      bul17_4 = W4ThreatsYP,
      bul17_5 = W4MadegiveYP,
      bul17_6 = W4NamesYP
    ),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, starts_with("W7BullyTypeYP0")),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, starts_with("W8BULLYTYPE0"))
)

# Merge all
bully_all <- reduce(bully_vars, full_join, by = "NSID")

# Recode for yes/no
recode_yesno <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    x %in% c(-92, -97) ~ -9,
    x %in% c(-96, -8) ~ -8,
    x == -99 ~ -3,
    is.na(x) ~ -3,
    TRUE ~ -2
  )
}

# Apply recodes
bully_all <- bully_all %>%
  mutate(
    bul14 = recode_yesno(bul14),
    bul15 = recode_yesno(bul15),
    bul16 = recode_yesno(bul16),
    bul17 = case_when(
      rowSums(across(starts_with("bul17_")) == 1, na.rm = TRUE) > 0 ~ 1,
      rowSums(
        across(c("bul17_1", "bul17_2", "bul17_4", "bul17_5", "bul17_6")) == 2,
        na.rm = TRUE
      ) ==
        5 ~ 0,
      rowSums(
        across(c("bul17_1", "bul17_2", "bul17_4", "bul17_5", "bul17_6")) < 0,
        na.rm = TRUE
      ) >
        0 ~ -8,
      rowSums(is.na(across(starts_with("bul17_")))) == 6 ~ -3,
      TRUE ~ -2
    ),
    bul20 = case_when(
      rowSums(
        across(starts_with("W7BullyTypeYP0")) > 0 &
          across(starts_with("W7BullyTypeYP0")) < 8,
        na.rm = TRUE
      ) >
        0 ~ 1,
      rowSums(across(starts_with("W7BullyTypeYP0")) == 8, na.rm = TRUE) ==
        6 ~ 0,
      rowSums(across(starts_with("W7BullyTypeYP0")) < 0, na.rm = TRUE) > 0 ~ -8,
      rowSums(is.na(across(starts_with("W7BullyTypeYP0")))) == 6 ~ -3,
      TRUE ~ -2
    ),
    bul25 = case_when(
      rowSums(across(starts_with("W8BULLYTYPE0")) == 1, na.rm = TRUE) > 0 ~ 1,
      rowSums(across(starts_with("W8BULLYTYPE0")) == 2, na.rm = TRUE) == 7 ~ 0,
      rowSums(across(starts_with("W8BULLYTYPE0")) < 0, na.rm = TRUE) > 0 ~ -8,
      rowSums(is.na(across(starts_with("W8BULLYTYPE0")))) == 7 ~ -3,
      TRUE ~ -2
    )
  ) %>%
  mutate(across(
    starts_with("bul"),
    ~ labelled(
      .x,
      labels = c(
        "No" = 0,
        "Yes" = 1,
        common_missing_labels
      )
    )
  )) %>%
  select(NSID, bul14, bul15, bul16, bul17, bul20, bul25)
