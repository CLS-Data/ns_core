# Prerequisite: 00-load-raw-data.R (this script also sources helpers.R)
#
# If you are running this script on its own, please run the following first
# from the project root:
#
# source(here::here("R", "00-load-raw-data.R"))
#
# or manually run 00-load-raw-data.R before this script.

# Sex --------------------------------------------------------------------

# Load sex variables from relevant sweeps
sex_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, sex_S1 = W1sexYP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, sex_S2 = W2SexYP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, sex_S3 = W3sexYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, W4Boost, sex_S4 = W4SexYP),
  S5 = ns_data[["S5youngperson"]] %>%
    select(NSID, sex_S5 = W5SexYP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, sex_S6 = W6Sex),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, sex_S7 = W7Sex),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, sex_S8 = W8CMSEX),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, sex_S9 = W9DSEX)
)

# Merge all sweeps by NSID
sex_all <- reduce(sex_vars, full_join, by = "NSID")

# Harmonised the missing values for S1-7
# Vector of S1–S7 variable names
sex_vars_s1_s7 <- paste0("sex_S", 1:7)

# Apply custom recode to S1–S7
sex_all <- sex_all %>%
  mutate(across(
    all_of(sex_vars_s1_s7),
    ~ case_when(
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -99 ~ -3,
      TRUE ~ .x
    )
  ))

# Derive harmonised sex
sex_all <- sex_all %>%
  mutate(
    # First pass: positive values only
    sex_final_main = case_when(
      !is.na(sex_S9) & sex_S9 > 0 ~ sex_S9,
      !is.na(sex_S1) & sex_S1 > 0 ~ sex_S1,
      !is.na(sex_S2) & sex_S2 > 0 ~ sex_S2,
      !is.na(sex_S3) & sex_S3 > 0 ~ sex_S3,
      !is.na(sex_S4) & sex_S4 > 0 & W4Boost == 2 ~ sex_S4, # main
      !is.na(sex_S4) & sex_S4 > 0 & W4Boost == 1 ~ sex_S4, # boost
      !is.na(sex_S5) & sex_S5 > 0 ~ sex_S5,
      !is.na(sex_S6) & sex_S6 > 0 ~ sex_S6,
      !is.na(sex_S7) & sex_S7 > 0 ~ sex_S7,
      !is.na(sex_S8) & sex_S8 > 0 ~ sex_S8,
      TRUE ~ NA_real_
    ),

    # Second pass: fallback to non-positive values (< 0)
    sex_final = case_when(
      !is.na(sex_final_main) ~ sex_final_main,
      !is.na(sex_S1) & sex_S1 < 1 ~ sex_S1,
      !is.na(sex_S2) & sex_S2 < 1 ~ sex_S2,
      !is.na(sex_S3) & sex_S3 < 1 ~ sex_S3,
      !is.na(sex_S4) & sex_S4 < 1 ~ sex_S4,
      !is.na(sex_S5) & sex_S5 < 1 ~ sex_S5,
      !is.na(sex_S6) & sex_S6 < 1 ~ sex_S6,
      !is.na(sex_S7) & sex_S7 < 1 ~ sex_S7,
      !is.na(sex_S8) & sex_S8 < 1 ~ sex_S8,
      TRUE ~ NA_real_
    )
  )

sex_all <- sex_all %>%
  mutate(
    sex = case_when(
      sex_final == 1 ~ 0, # 1 = male → 0
      sex_final == 2 ~ 1, # 2 = female → 1
      TRUE ~ sex_final # handle others or missing
    )
  ) %>%
  mutate(
    sex = factor(
      sex,
      levels = c(0, 1, -1, -3, -9),
      labels = c(
        "male",
        "female",
        "Item not applicable",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Refusal"
      )
    )
  ) %>%
  select(NSID, sex)

# Ethnicity --------------------------------------------------------------------
# Load ethnicity variables from relevant sweeps
ethnicity_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, eth_S1 = W1ethnic2YP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, eth_S2 = W2ethnicYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, eth_S4 = w4ethnic2YP),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, eth_S8 = W8DETHN15),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, eth_S9 = W9DETHN15)
)

# Merge into one dataset
eth_all <- reduce(ethnicity_vars, full_join, by = "NSID")

# Harmonise missing values for S1–S4
# Create a vector of ethnicity variables
eth_vars <- c("eth_S1", "eth_S2", "eth_S4")

# Apply the recoding (recode missing values in Sweeps 1-4)
eth_all <- eth_all %>%
  mutate(across(
    all_of(eth_vars),
    ~ case_when(
      .x == -999 ~ -2,
      .x == -998 ~ -2,
      .x == -997 ~ -2,
      .x == -99 ~ -3,
      .x == -94 ~ -2,
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -1 ~ -8,
      TRUE ~ .x
    )
  ))

# Derive ethnicity: use S1 if available, else later
eth_all <- eth_all %>%
  mutate(
    eth = case_when(
      !is.na(eth_S1) & eth_S1 > 0 ~ eth_S1,
      !is.na(eth_S2) & eth_S2 > 0 ~ eth_S2,
      !is.na(eth_S4) & eth_S4 > 0 ~ eth_S4,
      !is.na(eth_S8) & eth_S8 > 0 ~ eth_S8,
      !is.na(eth_S9) & eth_S9 > 0 ~ eth_S9,
      !is.na(eth_S1) & eth_S1 < 1 ~ eth_S1,
      !is.na(eth_S2) & eth_S2 < 1 ~ eth_S2,
      !is.na(eth_S4) & eth_S4 < 1 ~ eth_S4,
      !is.na(eth_S8) & eth_S8 < 1 ~ eth_S8,
      !is.na(eth_S9) & eth_S9 < 1 ~ eth_S9,
      TRUE ~ -3 # Not interviewed/present
    )
  ) %>%
  mutate(
    eth = factor(
      eth,
      levels = c(
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        -1,
        -2,
        -3,
        -8,
        -9
      ),
      labels = c(
        "White-British",
        "White-Irish",
        "Any other White background",
        "Mixed-White and Black Caribbean",
        "Mixed-White and Black African",
        "Mixed-White and Asian",
        "Any other Mixed background",
        "Asian or Asian British-Indian",
        "Asian or Asian British-Pakistani",
        "Asian or Asian British-Bangladeshi",
        "Any other Asian background",
        "Black or Black British-Caribbean",
        "Black or Black British-African",
        "Any other Black background",
        "Chinese",
        "Any other background",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  ) %>%
  select(NSID, eth)

# Language --------------------------------------------------------------------
# Load relevant language variables
lang_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, lang_S1 = W1englangYP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, lang_S2 = W2EnglangYP),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, lang_S3 = W3englangHH),
  S4 = ns_data[["S4familybackground"]] %>%
    select(NSID, lang_S4 = W4EngLangHH)
)

# Merge
lang_all <- reduce(lang_vars, full_join, by = "NSID")

# Apply the recoding (recode missing values in Sweeps 1-4)
lang_all <- lang_all %>%
  mutate(across(
    starts_with("lang_S"),
    ~ case_when(
      .x %in% c(-999, -998, -997, -995, -94) ~ -2, # error/information lost
      .x == -99 ~ -3, # not interviewed
      .x == -92 ~ -9, # refused
      .x == -91 ~ -1, # not applicable
      .x == -1 ~ -8, # don't know
      TRUE ~ .x
    )
  ))

# Derive final language variable: use S1, else S2, else S4
lang_all <- lang_all %>%
  mutate(
    lang = case_when(
      !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
      !is.na(lang_S1) & lang_S1 < 1 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 < 1 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 < 1 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 < 1 ~ lang_S4,
      TRUE ~ -3 # Not interviewed/present
    )
  ) %>%
  mutate(
    lang = factor(
      lang,
      levels = c(1, 2, 3, 4, -1, -2, -3, -8, -9),
      labels = c(
        "English only",
        "English first/main and speaks other languages",
        "Another language is respondent’s first or main language",
        "Bilingual",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  ) %>%
  select(NSID, lang)

# Sexual Orientation --------------------------------------------------------------------
# Load sexuality variables
sexuality_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, sori19 = W6SexualityYP),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, sori20 = W7SexualityYP),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, sori25 = W8SEXUALITY),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, sori32 = W9SORI)
)

# Merge by ID
sexuality_all <- reduce(sexuality_vars, full_join, by = "NSID")

# recode missing values and response categories
sexuality_all <- sexuality_all %>%
  mutate(
    sori19 = case_when(
      sori19 == 1 ~ 1,
      sori19 == 2 ~ 2,
      sori19 == 3 ~ 3,
      sori19 == 4 ~ 4,
      sori19 %in% c(-100, -97, -3) ~ -3,
      sori19 %in% c(-92, -9) ~ -9,
      sori19 %in% c(-91, -1, -8) ~ -8,
      sori19 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori20 = case_when(
      sori20 == 1 ~ 1,
      sori20 == 2 ~ 2,
      sori20 == 3 ~ 3,
      sori20 == 4 ~ 4,
      sori20 %in% c(-100, -97, -3) ~ -3,
      sori20 %in% c(-92, -9) ~ -9,
      sori20 %in% c(-91, -1, -8) ~ -8,
      sori20 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori25 = case_when(
      sori25 == 1 ~ 1,
      sori25 == 2 ~ 2,
      sori25 == 3 ~ 3,
      sori25 == 4 ~ 4,
      sori25 == -9 ~ -9,
      sori25 == -8 ~ -8,
      sori25 == -1 ~ -8,
      TRUE ~ -3
    ),
    sori32 = case_when(
      sori32 == 1 ~ 1,
      sori32 == 2 ~ 2,
      sori32 == 3 ~ 3,
      sori32 == 4 ~ 4,
      sori32 == 5 ~ -7,
      sori32 == -9 ~ -9,
      sori32 %in% c(-8, -1) ~ -8,
      sori32 == -3 ~ -3,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(
    starts_with("sori"),
    ~ factor(
      .x,
      levels = c(1, 2, 3, 4, -1, -2, -3, -7, -8, -9),
      labels = c(
        "Heterosexual/straight",
        "Gay/lesbian",
        "Bisexual",
        "Other",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Prefer not to say",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Partnership --------------------------------------------------------------------
# Load partnership variables from relevant sweeps
partnr_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, partnr19 = W6MarStatYP),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, partnradu25 = W8DMARSTAT),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, partnradu32 = W9DMARSTAT)
)

# Merge all sweeps by ID
partnr_all <- reduce(partnr_vars, full_join, by = "NSID")

# recode missing values and response categories
partnr_all <- partnr_all %>%
  mutate(
    partnr19 = case_when(
      partnr19 > 0 ~ partnr19 - 1,
      partnr19 == -92 ~ -9,
      partnr19 == -91 ~ -1,
      partnr19 == -1 ~ -8,
      partnr19 %in% c(-997, -97) ~ -3,
      TRUE ~ -3
    ),
    partnr25 = case_when(
      partnradu25 == 1 ~ 0,
      partnradu25 %in% c(2, 6) ~ 1,
      partnradu25 %in% c(3, 7) ~ 2,
      partnradu25 %in% c(4, 8) ~ 3,
      partnradu25 %in% c(5, 9) ~ 4,
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    partnr32 = case_when(
      partnradu32 == 1 ~ 0,
      partnradu32 %in% c(2, 6) ~ 1,
      partnradu32 %in% c(3, 7) ~ 2,
      partnradu32 %in% c(4, 8) ~ 3,
      partnradu32 %in% c(5, 9) ~ 4,
      partnradu32 == -9 ~ -9,
      partnradu32 == -8 ~ -8,
      partnradu32 == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Derive adult partnership variables detailed response categories (age 25 and 32)
partnr_all <- partnr_all %>%
  mutate(
    partnradu25 = case_when(
      partnradu25 %in% 1:9 ~ partnradu25 - 1,
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    partnradu32 = case_when(
      partnradu32 %in% 1:9 ~ partnradu32 - 1,
      partnradu32 == -9 ~ -9,
      partnradu32 == -8 ~ -8,
      partnradu32 == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    across(
      c(partnr19, partnr25, partnr32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, -1, -3, -8, -9),
        labels = c(
          "Single and never married or in a CP",
          "Married",
          "Separated but still legally married/in a CP",
          "Divorced/former CP",
          "Widowed/surviving CP",
          "Item not applicable",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(partnradu25, partnradu32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -3, -8, -9),
        labels = c(
          "Single and never married or in a CP",
          "Married",
          "Separated but still legally married",
          "Divorced",
          "Widowed",
          "A civil partner",
          "Separated but still legally in a CP",
          "A former civil partner",
          "A surviving civil partner",
          "Item not applicable",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Region --------------------------------------------------------------------
# Load region variables from relevant sweeps
region_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, regub15 = urbind, regov15 = gor),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, regub16 = urbind, regov16 = gor),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, regor25 = W8DGOR),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, regor32 = W9DRGN),
  S9_2 = ns_data[["S9maininterview"]] %>%
    select(NSID, regint32 = W9NATIONRES)
)

# Merge all region variables by NSID
region_all <- reduce(region_vars, full_join, by = "NSID")

# recode missing valuse and response categories
region_all <- region_all %>%
  mutate(across(
    c(regub15, regub16),
    ~ case_when(
      .x %in% 1:8 ~ .x,
      .x == -94 ~ -2,
      TRUE ~ -3
    )
  )) %>%

  mutate(across(
    c(regov15, regov16),
    ~ case_when(
      .x %in% 1:9 ~ .x,
      .x == -94 ~ -2,
      TRUE ~ -3
    )
  )) %>%

  mutate(across(
    c(regor25, regor32),
    ~ case_when(
      .x %in% 1:12 ~ .x,
      .x == 13 ~ -2, # faulty location
      .x == -9 ~ -9, # refused
      .x == -8 ~ -8, # don't know
      .x == -1 ~ -1, # not applicable
      TRUE ~ -3 # not participated
    )
  )) %>%

  mutate(
    regint32 = case_when(
      regint32 %in% c(1, 2, 3, 4) ~ 1, # in the UK
      regint32 == 5 ~ 2, # abroad
      regint32 == -9 ~ -9,
      regint32 == -8 ~ -8,
      regint32 == -3 ~ -3,
      regint32 == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    across(
      c(regub15, regub16),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -7, -8, -9),
        labels = c(
          "Urban >=10k – sparse",
          "Town & Fringe – sparse",
          "Village – sparse",
          "Hamlet and Isolated Dwelling – sparse",
          "Urban >= 10k - less sparse",
          "Town & Fringe - less sparse",
          "Village - less sparse",
          "Hamlet and Isolated Dwelling - less sparse",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Prefer not to say",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(regov15, regov16),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -2, -3, -7, -8, -9),
        labels = c(
          "North East",
          "North West",
          "Yorkshire and The Humber",
          "East Midlands",
          "West Midlands",
          "East of England",
          "London",
          "South East",
          "South West",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Prefer not to say",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(regor25, regor32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -1, -2, -3, -8, -9),
        labels = c(
          "North East",
          "North West",
          "Yorkshire and The Humber",
          "East Midlands",
          "West Midlands",
          "East of England",
          "London",
          "South East",
          "South West",
          "Wales",
          "Scotland",
          "Northern Ireland",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    regint32 = factor(
      regint32,
      levels = c(1, 2, -1, -3, -8, -9),
      labels = c(
        "In the UK",
        "Abroad",
        "Item not applicable",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  ) %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)
