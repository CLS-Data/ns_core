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

# Harmonise the missing values for S1-7
# Vector of S1–S7 variable names
sex_vars_s1_s7 <- paste0("sex_S", 1:7)

# Apply custom recode to S1–S7
sex_all <- sex_all %>%
  mutate(
    across(
      all_of(sex_vars_s1_s7),
      ~ case_when(
        .x == -92 ~ -9,
        .x == -91 ~ -1,
        .x == -99 ~ -3,
        .default = .x
      )
    )
  )

# Harmonise sex: prefer self-report at S9, else taken from other sweeps in order S1 -> S9.
sex_all <- sex_all %>%
  mutate(
    # First pass: positive values only
    sex_final_main = case_when(
      sex_S9 > 0 ~ sex_S9,
      sex_S1 > 0 ~ sex_S1,
      sex_S2 > 0 ~ sex_S2,
      sex_S3 > 0 ~ sex_S3,
      sex_S4 > 0 ~ sex_S4,
      sex_S5 > 0 ~ sex_S5,
      sex_S6 > 0 ~ sex_S6,
      sex_S7 > 0 ~ sex_S7,
      sex_S8 > 0 ~ sex_S8,
      .default = NA
    ),
    # Otherwise first non‑substantive (<1) from S1–S8
    sex_final = case_when(
      !is.na(sex_final_main) ~ sex_final_main,
      sex_S1 < 1 ~ sex_S1,
      sex_S2 < 1 ~ sex_S2,
      sex_S3 < 1 ~ sex_S3,
      sex_S4 < 1 ~ sex_S4,
      sex_S5 < 1 ~ sex_S5,
      sex_S6 < 1 ~ sex_S6,
      sex_S7 < 1 ~ sex_S7,
      sex_S8 < 1 ~ sex_S8,
      .default = NA
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
    sex = haven::labelled(
      sex,
      labels = c(
        "Male" = 0L,
        "Female" = 1L,
        "Item not applicable" = -1L,
        "Not asked at the fieldwork stage/participated/interviewed" = -3L,
        "Refusal" = -9L
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
  mutate(
    across(
      all_of(eth_vars),
      ~ case_when(
        .x %in% -999:-997 ~ -2,
        .x == -99 ~ -3,
        .x == -94 ~ -8, # This recodes insufficient info -> script error, changed from -2 to -8.
        .x == -92 ~ -9,
        .x == -91 ~ -1,
        .x == -1 ~ -8,
        .default = .x
      )
    )
  )

# Derive ethnicity: use S1 if available, else later
eth_all <- eth_all %>%
  mutate(
    eth = case_when(
      eth_S1 > 0 ~ eth_S1,
      eth_S2 > 0 ~ eth_S2,
      eth_S4 > 0 ~ eth_S4,
      eth_S8 > 0 ~ eth_S8,
      eth_S9 > 0 ~ eth_S9,
      eth_S1 < 1 ~ eth_S1,
      eth_S2 < 1 ~ eth_S2,
      eth_S4 < 1 ~ eth_S4,
      eth_S8 < 1 ~ eth_S8,
      eth_S9 < 1 ~ eth_S9,
      TRUE ~ -3 # Not interviewed/present
    )
  )

eth_labels_substantive <- c(
  "White-British" = 1L,
  "White-Irish" = 2L,
  "Any other White background" = 3L,
  "Mixed-White and Black Caribbean" = 4L,
  "Mixed-White and Black African" = 5L,
  "Mixed-White and Asian" = 6L,
  "Any other Mixed background" = 7L,
  "Asian or Asian British-Indian" = 8L,
  "Asian or Asian British-Pakistani" = 9L,
  "Asian or Asian British-Bangladeshi" = 10L,
  "Any other Asian background" = 11L,
  "Black or Black British-Caribbean" = 12L,
  "Black or Black British-African" = 13L,
  "Any other Black background" = 14L,
  "Chinese" = 15L,
  "Any other background" = 16L
)

eth_all <- eth_all |>
  mutate(
    eth = as.integer(eth),
    eth = labelled(
      eth,
      labels = c(
        eth_labels_substantive,
        # only the missing codes actually used for this variable
        common_missing_labels
      )
    )
  ) |>
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

# Final value labels
lang_labels_substantive <- c(
  "English only" = 1L,
  "English first/main and speaks other languages" = 2L,
  "Another language is respondent’s first or main language" = 3L,
  "Bilingual" = 4L
)

# Derive final language variable: use S1, else S2, else S4
lang_all <- lang_all %>%
  mutate(
    lang = case_when(
      lang_S1 > 0 ~ lang_S1,
      lang_S2 > 0 ~ lang_S2,
      lang_S3 > 0 ~ lang_S3,
      lang_S4 > 0 ~ lang_S4,
      lang_S1 < 1 ~ lang_S1,
      lang_S2 < 1 ~ lang_S2,
      lang_S3 < 1 ~ lang_S3,
      lang_S4 < 1 ~ lang_S4,
      TRUE ~ -3 # Not interviewed/present
    )
  ) %>%
  mutate(
    lang = labelled(
      as.integer(lang),
      labels = c(
        lang_labels_substantive,
        common_missing_labels
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

# Labels
sexuality_labels_substantive <- c(
  "Heterosexual/straight" = 1L,
  "Gay/lesbian" = 2L,
  "Bisexual" = 3L,
  "Other" = 4L
)

# recode missing values and response categories
sexuality_all <- sexuality_all %>%
  mutate(
    sori19 = case_when(
      sori19 == 1 ~ 1,
      sori19 == 2 ~ 2,
      sori19 == 3 ~ 3,
      sori19 == 4 ~ 4,
      sori19 %in% c(-100, -97, -3) ~ -3, # Check: 100 stands for 'Respondent declined sexual experience Qs', -97: Refused self-completion, this maps them onto 'Not asked (...)' instead of 'Refused'
      sori19 %in% c(-92, -9) ~ -9,
      sori19 %in% c(-1, -8) ~ -8,
      sori19 == -91 ~ -1, # Not applicable
      sori19 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori20 = case_when(
      sori20 == 1 ~ 1,
      sori20 == 2 ~ 2,
      sori20 == 3 ~ 3,
      sori20 == 4 ~ 4,
      sori20 %in% c(-100, -97, -3) ~ -3, # Check: 100 stands for 'Respondent declined sexual experience Qs', -97: Refused self-completion, this maps them onto 'Not asked (...)' instead of 'Refused'
      sori20 %in% c(-92, -9) ~ -9,
      sori20 %in% c(-1, -8) ~ -8,
      sori20 == -91 ~ -1, # Not applicable
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
      sori25 == -1 ~ -1, # Not applicable
      TRUE ~ -3
    ),
    sori32 = case_when(
      sori32 == 1 ~ 1,
      sori32 == 2 ~ 2,
      sori32 == 3 ~ 3,
      sori32 == 4 ~ 4,
      sori32 == 5 ~ -7,
      sori32 == -9 ~ -9,
      sori32 %in% c(-8) ~ -8,
      sori32 == -3 ~ -3,
      sori32 == -1 ~ -1, # Not applicable
      TRUE ~ -3
    )
  ) %>%
  mutate(
    across(
      starts_with("sori"),
      ~ labelled(
        as.integer(.x),
        labels = c(
          sexuality_labels_substantive,
          common_missing_labels
        )
      )
    )
  ) %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Partnership --------------------------------------------------------------------

# Load partnership variables from relevant sweeps
partnr_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, partnr19_orig = W6MarStatYP),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, partnradu25_orig = W8DMARSTAT),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, partnradu32_orig = W9DMARSTAT)
)

# Merge all sweeps by ID
partnr_all <- reduce(partnr_vars, full_join, by = "NSID")

## Complete version --------------------------------------------------------------------

# Look up tables for simpler re-coding
partnradu25_lookup <- tibble::tribble(
  ~old_label                            , ~new_value , ~new_label                              ,
  # missing
  "Refused"                             , -9L        , "Refusal"                               ,
  "Insufficient information"            , -8L        , "Don't know / insufficient information" ,
  "Not applicable"                      , -1L        , "Item not applicable"                   ,
  # substantive 0–8
  "Single and never married or in a CP" ,  0L        , "Single, never married / in CP"         ,
  "Married"                             ,  1L        , "Married"                               ,
  "Separated but still legally married" ,  2L        , "Separated, still legally married"      ,
  "Divorced"                            ,  3L        , "Divorced"                              ,
  "Widowed"                             ,  4L        , "Widowed"                               ,
  "A Civil Partner"                     ,  5L        , "Civil partner"                         ,
  "Separated but still legally in a CP" ,  6L        , "Separated, still legally in CP"        ,
  "A former Civil Partner"              ,  7L        , "Former civil partner"                  ,
  "A surviving Civil Partner"           ,  8L        , "Surviving civil partner"
)

partnradu32_lookup <- tibble::tribble(
  ~old_label                                                           , ~new_value , ~new_label                              ,
  # missing
  "Refused"                                                            , -9L        , "Refusal"                               ,
  "Insufficient information"                                           , -8L        , "Don't know / insufficient information" ,
  # substantive 0–8
  "Single that is never married or never in a Civil Partnership"       ,  0L        , "Single, never married / in CP"         ,
  "Married"                                                            ,  1L        , "Married"                               ,
  "Legally separated"                                                  ,  2L        , "Separated, still legally married"      ,
  "Divorced"                                                           ,  3L        , "Divorced"                              ,
  "Widowed"                                                            ,  4L        , "Widowed"                               ,
  "A Civil Partner in a legally recognised Civil Partnership"          ,  5L        , "Civil partner"                         ,
  "A former Civil Partner (where Civil Partnership legally dissolved)" ,  7L        , "Former civil partner"                  ,
  "A surviving Civil Partner (where Civil Partner has died)"           ,  8L        , "Surviving civil partner"
)

# Re-code through look up tables
partnr_all <- partnr_all |>
  mutate(
    partnradu25 = recode_labelled_by_labels(
      partnradu25_orig,
      partnradu25_lookup,
      na_to = -3, # All uncoded NAs -> -3
      na_label = "Not asked at the fieldwork stage/participated/interviewed" # Label for NAs
    ),
    partnradu32 = recode_labelled_by_labels(
      partnradu32_orig,
      partnradu32_lookup,
      na_to = -3,
      na_label = "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Cross-tab checks
partnr_all |>
  count(partnradu25_orig, partnradu25)

partnr_all |>
  count(partnradu32_orig, partnradu32)

## Simple version --------------------------------------------------------------------

# Shared look up for recoding complete -> simple partnradu25 and partnradu32
partnr_simple_lookup <- tibble::tribble(
  ~old_label                              , ~new_value , ~new_label                                 ,
  "Refusal"                               , -9L        , "Refusal"                                  ,
  "Don't know / insufficient information" , -8L        , "Don't know / insufficient information"    ,
  "Item not applicable"                   , -1L        , "Item not applicable"                      ,
  "Single, never married / in CP"         ,  0L        , "Single, never married / in CP"            ,
  "Married"                               ,  1L        , "Married / civil partner"                  ,
  "Civil partner"                         ,  1L        , "Married / civil partner"                  ,
  "Separated, still legally married"      ,  2L        , "Separated, still legally married / in CP" ,
  "Separated, still legally in CP"        ,  2L        , "Separated, still legally married / in CP" ,
  "Divorced"                              ,  3L        , "Divorced / former civil partner"          ,
  "Former civil partner"                  ,  3L        , "Divorced / former civil partner"          ,
  "Widowed"                               ,  4L        , "Widowed / surviving civil partner"        ,
  "Surviving civil partner"               ,  4L        , "Widowed / surviving civil partner"
)

# Look up table for recoding partnr19_orig -> simple partnr19
partnr19_simple_lookup <- tibble::tribble(
  ~old_label                            , ~new_value , ~new_label                                 ,
  "Script error"                        , -2L        , "Script error / information lost"          ,
  "Respondent declined self completion" , -7L        , "Prefer not to say / declined"             ,
  "Refused"                             , -9L        , "Refusal"                                  ,
  "Not applicable"                      , -1L        , "Item not applicable"                      ,
  "Don't know"                          , -8L        , "Don't know / insufficient information"    ,
  "Single, that is never married"       ,  0L        , "Single, never married / in CP"            ,
  "Married"                             ,  1L        , "Married / civil partner"                  ,
  "Separated"                           ,  2L        , "Separated, still legally married / in CP" ,
  "Divorced"                            ,  3L        , "Divorced / former civil partner"          ,
  "Widowed"                             ,  4L        , "Widowed / surviving civil partner"
)

# Custom function to recode variables according to the look up table
partnr_all <- partnr_all |>
  mutate(
    partnr19 = recode_labelled_by_labels(
      partnr19_orig,
      partnr19_simple_lookup,
      na_to = -3,
      na_label = "Not asked at the fieldwork stage/participated/interviewed"
    ),
    partnr25 = recode_labelled_by_labels(
      partnradu25,
      partnr_simple_lookup,
      unmatched = "keep", # Keep any unmatched values & their labels, called to preserve -3 for NAs
      na_to = -3,
      na_label = "Not asked at the fieldwork stage/participated/interviewed"
    ),
    partnr32 = recode_labelled_by_labels(
      partnradu32,
      partnr_simple_lookup,
      unmatched = "keep",
      na_to = -3,
      na_label = "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Checks
partnr_all %>%
  count(partnr19_orig, partnr19)

partnr_all %>%
  count(partnradu25, partnr25)

partnr_all %>%
  count(partnradu32, partnr32)

partnr_all <- partnr_all %>%
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

# Recode missing valuse and response categories
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

  mutate(
    across(
      c(regor25, regor32),
      ~ case_when(
        .x %in% 1:12 ~ .x,
        .x == 13 ~ -2, # faulty location
        .x == -9 ~ -9, # refused
        .x == -8 ~ -8, # don't know
        .x == -1 ~ -1, # not applicable
        TRUE ~ -3 # not participated
      )
    )
  ) %>%

  mutate(
    regint32 = case_when(
      regint32 %in% 1:4 ~ 1, # in the UK
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
      ~ labelled(
        x = .x,
        labels = c(
          "Urban >=10k – sparse" = 1,
          "Town & Fringe – sparse" = 2,
          "Village – sparse" = 3,
          "Hamlet and Isolated Dwelling – sparse" = 4,
          "Urban >= 10k - less sparse" = 5,
          "Town & Fringe - less sparse" = 6,
          "Village - less sparse" = 7,
          "Hamlet and Isolated Dwelling - less sparse" = 8,
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Prefer not to say" = -7,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    ),
    across(
      c(regov15, regov16),
      ~ labelled(
        x = .x,
        labels = c(
          "North East" = 1,
          "North West" = 2,
          "Yorkshire and The Humber" = 3,
          "East Midlands" = 4,
          "West Midlands" = 5,
          "East of England" = 6,
          "London" = 7,
          "South East" = 8,
          "South West" = 9,
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Prefer not to say" = -7,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    ),
    across(
      c(regor25, regor32),
      ~ labelled(
        x = .x,
        labels = c(
          "North East" = 1,
          "North West" = 2,
          "Yorkshire and The Humber" = 3,
          "East Midlands" = 4,
          "West Midlands" = 5,
          "East of England" = 6,
          "London" = 7,
          "South East" = 8,
          "South West" = 9,
          "Wales" = 10,
          "Scotland" = 11,
          "Northern Ireland" = 12,
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    ),
    regint32 = labelled(
      x = regint32,
      labels = c(
        "In the UK" = 1,
        "Abroad" = 2,
        "Item not applicable" = -1,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  ) %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)
