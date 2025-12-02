# This script is used to run the Next Steps MSEU 
# Load packages
library(haven)  # for reading SPSS/Stata files
library(dplyr)  # for data manipulation
library(purrr)  # for functional programming (map, reduce)
library(here)  # for file paths
library(labelled)  # for handling labelled data

# Set folder path (change as needed)
data_path <- here("data", "UKDA-5545-stata", "stata", "stata13", "safeguarded_eul")

# Define sweep file names
sweeps <- list(
  S1familybackground = "wave_one_lsype_family_background_2020.dta",
  S1youngperson = "wave_one_lsype_young_person_2020.dta",
  S2familybackground = "wave_two_lsype_family_background_2020.dta",
  S2youngperson = "wave_two_lsype_young_person_2020.dta",
  S12history = "lsype_history_file_wave_one_and_wave_two_june_2008.dta",
  S3familybackground = "wave_three_lsype_family_background_2020.dta",
  S3youngperson = "wave_three_lsype_young_person_2020.dta",
  S4familybackground = "wave_four_lsype_family_background_2020.dta",
  S4youngperson = "wave_four_lsype_young_person_2020.dta",
  S4history = "wave_four_lsype_history_2020.dta",
  S5familybackground = "wave_five_lsype_family_background_2020.dta",
  S5youngperson = "wave_five_lsype_young_person_2020.dta",
  S6youngperson = "wave_six_lsype_young_person_2020.dta",
  S7youngperson = "wave_seven_lsype_young_person_2020.dta",
  S8maininterview = "ns8_2015_main_interview.dta",
  S8selfcompletion = "ns8_2015_self_completion.dta",
  S8derivedvariable = "ns8_2015_derived.dta",
  S9maininterview = "ns9_2022_main_interview.dta",
  S9derivedvariable = "ns9_2022_derived_variables.dta",
  longitudinal = "ns9_2022_longitudinal_file.dta")


# Load all datasets
ns_data <- map(sweeps, ~ read_dta(file.path(data_path, .x)))

#### sex ####
# Load sex variables from relevant sweeps
sex_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID, sex_S1 = W1sexYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(NSID, sex_S2 = W2SexYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% 
    select(NSID, sex_S3 = W3sexYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID, W4Boost, sex_S4 = W4SexYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>% 
    select(NSID, sex_S5 = W5SexYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(NSID, sex_S6 = W6Sex),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(NSID, sex_S7 = W7Sex),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% 
    select(NSID, sex_S8 = W8CMSEX),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
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
    !is.na(sex_S4) & sex_S4 > 0 & W4Boost == 2 ~ sex_S4,  # main
    !is.na(sex_S4) & sex_S4 > 0 & W4Boost == 1 ~ sex_S4,  # boost
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
      sex_final == 1 ~ 0,  # 1 = male → 0
      sex_final == 2 ~ 1,  # 2 = female → 1
      TRUE ~ sex_final      # handle others or missing
    )
  ) %>%
  mutate(sex = factor(sex, 
                      levels = c(0, 1, -1, -3, -9), 
                      labels = c("male", 
                                 "female", 
                                 "Item not applicable", 
                                 "Not asked at the fieldwork stage/participated/interviewed", 
                                 "Refusal"))
         ) %>%
  select(NSID, sex)

#### ethnicity ####
# Load ethnicity variables from relevant sweeps
ethnicity_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID, eth_S1 = W1ethnic2YP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(NSID, eth_S2 = W2ethnicYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID, eth_S4 = w4ethnic2YP),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>% 
    select(NSID, eth_S8 = W8DETHN15),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>% 
    select(NSID, eth_S9 = W9DETHN15)
)

# Merge into one dataset
eth_all <- reduce(ethnicity_vars, full_join, by = "NSID")

# Harmonise missing values for S1–S4
# Create a vector of ethnicity variables
eth_vars <- c("eth_S1", "eth_S2", "eth_S4")

# Apply the recoding (recode missing values in Sweeps 1-4)
eth_all <- eth_all %>%
  mutate(across(all_of(eth_vars), ~ case_when(
    .x == -999 ~ -2,
    .x == -998 ~ -2,
    .x == -997 ~ -2,
    .x == -99  ~ -3,
    .x == -94  ~ -2,
    .x == -92  ~ -9,
    .x == -91  ~ -1,
    .x == -1   ~ -8,
    TRUE ~ .x
  )))

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
      TRUE ~ -3  # Not interviewed/present
    )
  )%>%
  mutate(eth = factor(eth, 
                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, -1, -2, -3, -8, -9), 
                      labels = c("White-British", 
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
                                 "Refusal"))
  ) %>%
  select(NSID, eth)

#### language ####
# Load relevant language variables
lang_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID, lang_S1 = W1englangYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(NSID, lang_S2 = W2EnglangYP),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>% 
    select(NSID, lang_S3 = W3englangHH),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>% 
    select(NSID, lang_S4 = W4EngLangHH)
)

# Merge
lang_all <- reduce(lang_vars, full_join, by = "NSID")

# Apply the recoding (recode missing values in Sweeps 1-4)
lang_all <- lang_all %>%
  mutate(across(starts_with("lang_S"), ~ case_when(
    .x %in% c(-999, -998, -997, -995, -94) ~ -2,  # error/information lost
    .x == -99 ~ -3,                        # not interviewed
    .x == -92 ~ -9,                        # refused
    .x == -91 ~ -1,                        # not applicable
    .x == -1  ~ -8,                        # don't know
    TRUE ~ .x
  )))

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
      TRUE ~ -3  # Not interviewed/present
    )
  )%>%
  mutate(lang = factor(lang, 
                      levels = c(1, 2, 3, 4, -1, -2, -3, -8, -9), 
                      labels = c("English only",
                                 "English first/main and speaks other languages",
                                 "Another language is respondent’s first or main language",
                                 "Bilingual",
                                 "Item not applicable", 
                                 "Script error/information lost",
                                 "Not asked at the fieldwork stage/participated/interviewed", 
                                 "Don’t know/insufficient information",
                                 "Refusal"))
  ) %>%
  select(NSID, lang)

#### sexual orientation ####
# Load sexuality variables
sexuality_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(NSID, sori19 = W6SexualityYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(NSID, sori20 = W7SexualityYP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>% 
    select(NSID, sori25 = W8SEXUALITY),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
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
  mutate(across(starts_with("sori"), ~ factor(.x, 
                       levels = c(1, 2, 3, 4, -1, -2, -3, -7, -8, -9), 
                       labels = c("Heterosexual/straight",
                                  "Gay/lesbian",
                                  "Bisexual",
                                  "Other",
                                  "Item not applicable", 
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed", 
                                  "Prefer not to say",
                                  "Don’t know/insufficient information",
                                  "Refusal")))
  ) %>%
  select(NSID, sori19, sori20, sori25, sori32)

#### partnership ####
# Load partnership variables from relevant sweeps
partnr_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(NSID, partnr19 = W6MarStatYP),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>% 
    select(NSID, partnradu25 = W8DMARSTAT),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>% 
    select(NSID, partnradu32 = W9DMARSTAT)
)

# Merge all sweeps by ID
partnr_all <- reduce(partnr_vars, full_join, by = "NSID")

# recode missing values and response categories
partnr_all <- partnr_all %>%
  mutate(
    partnr19 = case_when(
      partnr19 > 0 ~ partnr19-1,
      partnr19 == -92 ~ -9,
      partnr19 == -91 ~ -1,
      partnr19 == -1 ~ -8,
      partnr19 %in% c(-997, -97) ~ -3,
      TRUE ~ -3
    ),
    partnr25 = case_when(
      partnradu25 == 1 ~ 0,
      partnradu25 %in% c(2,6) ~ 1,
      partnradu25 %in% c(3,7) ~ 2,
      partnradu25 %in% c(4,8) ~ 3 ,
      partnradu25 %in% c(5,9) ~ 4,
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    partnr32 = case_when(
      partnradu32 == 1 ~ 0,
      partnradu32 %in% c(2,6) ~ 1,
      partnradu32 %in% c(3,7) ~ 2,
      partnradu32 %in% c(4,8) ~ 3 ,
      partnradu32 %in% c(5,9) ~ 4,
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
      partnradu25 %in% 1:9 ~ partnradu25-1,
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    partnradu32 = case_when(
      partnradu32 %in% 1:9 ~ partnradu32-1,
      partnradu32 == -9 ~ -9,
      partnradu32 == -8 ~ -8,
      partnradu32 == -1 ~ -1,
      TRUE ~ -3
    )
  )%>%
  mutate(across(c(partnr19, partnr25, partnr32), ~ factor(.x, 
                                              levels = c(0, 1, 2, 3, 4, -1, -3, -8, -9), 
                                              labels = c("Single and never married or in a CP",
                                                         "Married",
                                                         "Separated but still legally married/in a CP",
                                                         "Divorced/former CP",
                                                         "Widowed/surviving CP",
                                                         "Item not applicable", 
                                                         "Not asked at the fieldwork stage/participated/interviewed", 
                                                         "Don’t know/insufficient information",
                                                         "Refusal"))),
         across(c(partnradu25, partnradu32), ~ factor(.x, 
                                              levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -3, -8, -9), 
                                              labels = c("Single and never married or in a CP",
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
                                                         "Refusal")))
  ) %>%
  select(NSID, partnr19, partnr25, partnr32,
         partnradu25, partnradu32)

#### region ####
# Load region variables from relevant sweeps
region_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(NSID, regub15 = urbind, regov15 = gor),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(NSID, regub16 = urbind, regov16 = gor),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(NSID, regor25 = W8DGOR),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, regor32 = W9DRGN),
  S9_2 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, regint32 = W9NATIONRES)
)

# Merge all region variables by NSID
region_all <- reduce(region_vars, full_join, by = "NSID")

# recode missing valuse and response categories
region_all <- region_all %>%
  mutate(across(c(regub15, regub16), ~ case_when(
    .x %in% 1:8 ~ .x,
    .x == -94 ~ -2,
    TRUE ~ -3
  ))) %>%
  
  mutate(across(c(regov15, regov16), ~ case_when(
    .x %in% 1:9 ~ .x,
    .x == -94 ~ -2,
    TRUE ~ -3
  ))) %>%
  
  mutate(across(c(regor25, regor32), ~ case_when(
    .x %in% 1:12 ~ .x,
    .x == 13 ~ -2,                # faulty location
    .x == -9 ~ -9,                # refused
    .x == -8 ~ -8,                # don't know
    .x == -1 ~ -1,                # not applicable
    TRUE ~ -3                     # not participated
  ))) %>%
  
  mutate(regint32 = case_when(
    regint32 %in% c(1,2,3,4) ~ 1,   # in the UK
    regint32 == 5 ~ 2,   # abroad
    regint32 == -9 ~ -9,
    regint32 == -8 ~ -8,
    regint32 == -3 ~ -3,
    regint32 == -1 ~ -1,
    TRUE ~ -3
  ))%>%
  mutate(across(c(regub15, regub16), ~ factor(.x, 
                                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -7, -8, -9), 
                                              labels = c("Urban >=10k – sparse",
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
                                                         "Refusal"))),
         across(c(regov15, regov16), ~ factor(.x, 
                                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -2, -3, -7, -8, -9), 
                                              labels = c("North East",
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
                                                         "Refusal"))),
         across(c(regor25, regor32), ~ factor(.x, 
                                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -1, -2, -3, -8, -9), 
                                              labels = c("North East",
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
                                                         "Refusal"))),
         regint32 = factor(regint32, 
                           levels = c(1, 2, -1, -3, -8, -9), 
                           labels = c("In the UK",
                                      "Abroad",
                                      "Item not applicable", 
                                      "Not asked at the fieldwork stage/participated/interviewed", 
                                      "Don’t know/insufficient information",
                                      "Refusal"))
  ) %>%
  select(NSID, regub15, regov15, regub16, regov16,
         regor25, regor32, regint32)

#### current aim education own ####
# Load education variables from relevant sweeps
educaim_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson )) %>%
    select(NSID, educaim17_raw = w4saim),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(NSID, educaim19_raw = W6Saim),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(NSID, educaim20_raw = W7SAim),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(NSID, W8ACTIVITY05, starts_with("W8ACQUC"), starts_with("W8VCQUC")),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, W9ECONACT2, starts_with("W9ACQUC"), starts_with("W9VCQUC"))
)

# Merge by ID
educaim_all <- reduce(educaim_vars, full_join, by = "NSID")

# recode missing valuse and response categories
educaim_all <- educaim_all %>%
  mutate(
    # Sweep 4
    educaim17 = case_when(
      educaim17_raw %in% c(1:9,10, 11) ~ 1, # NVQ 1-3
      educaim17_raw == 14 ~ 5, # not studying
      educaim17_raw %in% 12 ~ 3, # other
      educaim17_raw == 13 ~ 4, # none of these
      educaim17_raw == -94 ~ -2,
      educaim17_raw == -91 ~ -1,
      TRUE ~ -3  # Not interviewed/present
    ),
    
    # Sweep 6
    educaim19 = case_when(
      educaim19_raw %in% 1:4 ~ 0,
      educaim19_raw %in% 5:12 ~ 1,
      educaim19_raw == 14 ~ 3,
      educaim19_raw == 15 ~ 4,
      educaim19_raw == 16 ~ 5,
      educaim19_raw == -94 ~ -2,
      educaim19_raw == -91 ~ -1,
      TRUE ~ -3
    ),
    
    # Sweep 7
    educaim20 = case_when(
      educaim20_raw %in% 10:13 ~ 0,
      educaim20_raw %in% 1:9 ~ 1,
      educaim20_raw == 14 ~ 3,
      educaim20_raw == -94 ~ -2,
      educaim20_raw == -91 ~ 5,
      TRUE ~ -3
    )) %>%
  
  # Sweep 8
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5, #not studying
      W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 |
        W8ACQUC0D == 1 | W8ACQUC0E == 1 |
        W8VCQUC0J == 1 | W8VCQUC0K == 1  ~ 0,
      W8ACQUC0F == 1 | W8ACQUC0G ==1 | W8ACQUC0H ==1 | 
        W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1| 
        W8ACQUC0L == 1 | W8ACQUC0M == 1 | 
        W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 |
        W8VCQUC0E == 1 | W8VCQUC0F == 1 | W8VCQUC0G == 1 | 
        W8VCQUC0H == 1 | W8VCQUC0I == 1 | 
        W8VCQUC0L == 1 | W8VCQUC0M == 1 | W8VCQUC0N == 1 ~ 1,
      W8VCQUC0D == 1 |W8VCQUC0P == 1 ~ 2,
      W8ACQUC0N == 1 | 
        W8VCQUC0O ==1 ~ 3,
      W8ACQUC0O == 1 | 
        W8ACQUC0P == 1 ~ 4,
      W8ACQUC0Q == 1 | 
        W8VCQUC0R == 1 ~ -9,
      W8ACQUC0P == 1 |
        W8VCQUC0Q == 1 ~ -8,
      TRUE ~ -3
    ),
    
    # Sweep 9
    educaim32 = case_when(
      W9ECONACT2 == -91 ~ -8, 
      W9ECONACT2 != 6 & W9ECONACT2 != 7 ~ 5, # not studying
      W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 |
        W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9ACQUC0F == 1 |
        W9VCQUC0A == 1 | W9VCQUC0B == 1 | W9VCQUC0C == 1 |
        W9VCQUC0S == 1 | W9VCQUC0V == 1 | W9VCQUCAC == 1 ~ 0,
      W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | 
        W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | 
        W9ACQUC0M == 1 | W9ACQUC0O == 1 | 
        W9ACQUC0P == 1 | W9ACQUC0Q == 1 | 
        W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 |
        W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 |
        W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 |
        W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | 
        W9VCQUC0R == 1 | W9VCQUC0T == 1 | 
        W9VCQUC0U == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 |
        W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | 
        W9VCQUCAB == 1 | W9VCQUCAD == 1 | W9VCQUCAE == 1 ~ 1,
      W9ACQUC0N == 1  ~ 2,
      W9ACQUC0R ==1 | 
        W9VCQUCAF == 1 ~ 3,
      W9ACQUC0S == 1 |
        W9VCQUCAG == 1 ~ 4,
      W9ACQUC0T == 1  | 
        W9VCQUCAH == 1 ~ -8,
      W9ACQUC0U == 1 | 
        W9VCQUCAI == 1 ~ -9,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(c(educaim17, educaim19, educaim20, educaim25, educaim32), ~ factor(.x, 
                                                                    levels = c(0, 1, 2, 3, 4, 5, -1, -2, -3, -8, -9), 
                                                                    labels = c("NVQ 4-5",
                                                                               "NVQ 1-3",
                                                                               "None/entry",
                                                                               "Other",
                                                                               "None of these qualifications",
                                                                               "Not studying",
                                                                               "Item not applicable", 
                                                                               "Script error/information lost",
                                                                               "Not asked at the fieldwork stage/participated/interviewed", 
                                                                               "Don’t know/insufficient information",
                                                                               "Refusal")))
  ) %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)



#### education own ####
# Load education variables from relevant sweeps
educ_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson )) %>%
    select(NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(NSID, W8EDUS, starts_with("W8ACQU"), starts_with("W8VCQU")),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, starts_with("W9ACQU"), starts_with("W9VCQU"))
)

# Merge by ID
educ_all <- reduce(educ_vars, full_join, by = "NSID")

# recode missing valuse and response categories
educ_all <- educ_all %>%
  # Sweep 8
  mutate(
    educ25 = case_when(
      W8ACQU0A == 1 | W8ACQU0B == 1 | W8ACQU0C == 1 |
      W8ACQU0D == 1 | W8ACQU0E == 1 |
        W8VCQU0J == 1 | W8VCQU0K == 1  ~ 0,
     W8ACQU0F == 1 | W8ACQU0G ==1 | W8ACQU0H ==1 | 
     W8ACQU0I == 1 | W8ACQU0J == 1 | W8ACQU0K == 1| 
     W8ACQU0L == 1 | W8ACQU0M == 1 | 
       W8VCQU0A == 1 | W8VCQU0B == 1 | W8VCQU0C == 1 |
       W8VCQU0E == 1 | W8VCQU0F == 1 | W8VCQU0G == 1 | 
       W8VCQU0H == 1 | W8VCQU0I == 1 | 
       W8VCQU0L == 1 | W8VCQU0M == 1 | W8VCQU0N == 1 ~ 1,
     W8VCQU0D == 1 |W8VCQU0P == 1 ~ 2,
     W8ACQU0N == 1 | 
       W8VCQU0O ==1 ~ 3,
     W8ACQU0O == 1 | 
       W8ACQU0P == 1 ~ 4,
     W8ACQU0Q == 1 | 
       W8VCQU0R == 1 ~ -9,
     W8ACQU0P == 1 |
       W8VCQU0Q == 1 ~ -8,
     NA ~ -3
    ),
    
    # Sweep 9
    educ32 = case_when(
      W9ACQU0A == 1 | W9ACQU0B == 1 | W9ACQU0C == 1 |
      W9ACQU0D == 1 | W9ACQU0E == 1 | W9ACQU0F == 1 |
        W9VCQU0A == 1 | W9VCQU0B == 1 | W9VCQU0C == 1 |
        W9VCQU0S == 1 | W9VCQU0V == 1 | W9VCQUAC == 1 ~ 0,
      W9ACQU0G == 1 | W9ACQU0H == 1 | W9ACQU0I == 1 | 
      W9ACQU0J == 1 | W9ACQU0K == 1 | W9ACQU0L == 1 | 
      W9ACQU0M == 1 | W9ACQU0O == 1 | 
      W9ACQU0P == 1 | W9ACQU0Q == 1 | 
        W9VCQU0D == 1 | W9VCQU0E == 1 | W9VCQU0F == 1 |
        W9VCQU0G == 1 | W9VCQU0H == 1 | W9VCQU0I == 1 |
        W9VCQU0L == 1 | W9VCQU0M == 1 | W9VCQU0N == 1 |
        W9VCQU0O == 1 | W9VCQU0P == 1 | W9VCQU0Q == 1 | 
        W9VCQU0R == 1 | W9VCQU0T == 1 | 
        W9VCQU0U == 1 | W9VCQU0W == 1 | W9VCQU0X == 1 |
        W9VCQU0Y == 1 | W9VCQU0Z == 1 | W9VCQUAA == 1 | 
        W9VCQUAB == 1 | W9VCQUAD == 1 | W9VCQUAE == 1 ~ 1,
      W9ACQU0N == 1  ~ 2,
      W9ACQU0R ==1 | 
        W9VCQUAF == 1 ~ 3,
      W9ACQU0S == 1 |
        W9VCQUAG == 1 ~ 4,
      W9ACQU0T == 1  | 
        W9VCQUAH == 1 ~ -8,
      W9ACQU0U == 1 | 
        W9VCQUAI == 1 ~ -9,
      NA ~ -3
    )
  ) %>%
  mutate(across(c(educ25, educ32), ~ factor(.x, 
                                            levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9), 
                                            labels = c("NVQ 4-5",
                                                       "NVQ 1-3",
                                                       "None/entry",
                                                       "Other",
                                                       "None of these qualifications",
                                                       "Item not applicable", 
                                                       "Script error/information lost",
                                                       "Not asked at the fieldwork stage/participated/interviewed", 
                                                       "Don’t know/insufficient information",
                                                       "Refusal")))
  ) %>%
  select(NSID, educ25, educ32)


#### education parents ####
# Load and rename relevant variables from each sweep
parent_edu_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(NSID, educma_S1 = W1hiqualmum, educpa_S1 = W1hiqualdad),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(NSID, educma_S2 = W2hiqualmum, educpa_S2 = W2hiqualdad),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(NSID, educma_S4 = w4hiqualmum, educpa_S4 = w4hiqualdad)
)

#  Merge all sweeps by NSID
parent_edu_all <- reduce(parent_edu_vars, full_join, by = "NSID")

# Recode missing values and response categories
parent_edu_all <- parent_edu_all %>%
  mutate(across(
    matches("educ(ma|pa)_S[1-4]"),
    ~ case_when(
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x %in% c(-98) ~ -3,
      .x %in% c(-999, -99, -94, 19) ~ -2,
      TRUE ~ .x
    )
  ))

# Step 4: Derive full education and transform to simple education
parent_edu_all <- parent_edu_all %>%
  mutate(
    #mother full education (aggregate the information from sweeps 1-4)
    educmadtl = case_when(
      !is.na(educma_S4) & educma_S4 > 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 > 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 > 0 ~ educma_S1,
      !is.na(educma_S4) & educma_S4 < 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 < 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 < 0 ~ educma_S1,
      TRUE ~ -3  # Not interviewed / present
    ), 
    #transform to 3-level education (mother)
    educma = case_when(
      educmadtl %in% 1:4 ~ 0,
      educmadtl %in% 5:17 ~ 1,
      educmadtl == 18 ~ 2,
      educmadtl == 19 ~ 3, # other
      educmadtl == 20 ~ 4, # none of these qualifications
      TRUE ~ educmadtl  # keep negatives as-is
    ),
    #father full education (aggregate the information from sweeps 1-4)
    educpadtl = case_when(
      !is.na(educpa_S1) & educpa_S1 > 0 ~ educpa_S1,
      !is.na(educpa_S2) & educpa_S2 > 0 ~ educpa_S2,
      !is.na(educpa_S4) & educpa_S4 > 0 ~ educpa_S4,
      !is.na(educpa_S1) & educpa_S1 < 0 ~ educpa_S1,
      !is.na(educpa_S2) & educpa_S2 < 0 ~ educpa_S2,
      !is.na(educpa_S4) & educpa_S4 < 0 ~ educpa_S4,
      TRUE ~ -3
    ),
    #transform to 3-level education (father)
    educpa = case_when(
      educpadtl %in% 1:4 ~ 0,
      educpadtl %in% 5:17 ~ 1,
      educpadtl == 18 ~ 2,
      educpadtl == 19 ~ 3, 
      educpadtl == 20 ~ 4,
      TRUE ~ educpadtl  # keep negatives as-is
    )
  )%>%
  mutate(across(c(educma, educpa), ~ factor(.x, 
                                            levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9), 
                                            labels = c("NVQ 4-5",
                                                       "NVQ 1-3",
                                                       "None/entry",
                                                       "Other",
                                                       "None of these qualifications",
                                                       "Item not applicable", 
                                                       "Script error/information lost",
                                                       "Not asked at the fieldwork stage/participated/interviewed", 
                                                       "Don’t know/insufficient information",
                                                       "Refusal")))
  ) %>%
  select(NSID, educma, educpa)


#### economic activity ####
ecoact_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID, ecoact17 = W4empsYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>%
    select(NSID, ecoact18 = W5mainactYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(NSID, ecoact19 = W6TCurrentAct),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(NSID, ecoact20 = W7TCurrentAct),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(NSID, ecoactadu25 = W8DACTIVITYC),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, ecoactadu32 = W9DACTIVITYC)
)
# Merge by NSID
ecoact_all <- reduce(ecoact_vars, full_join, by = "NSID")

# Harmonise missing values and derive economic activity variables
ecoact_all <- ecoact_all %>%
  mutate(
    ## Sweep 4
    ecoact17 = case_when(
      ecoact17 %in% 1:2 ~ 1, # In paid work
      ecoact17 == 4 ~ 2, # Apprenticeship/government training scheme/training 
      ecoact17 == 5 | ecoact17 == -91 ~ 3, # Education
      ecoact17 == 3 ~ 4, # Unemployed
      ecoact17 == 6 ~ 5, # Looking after home/family
      ecoact17 %in% c(7, 8, 9) ~ 6, # Sick/disabled, other, doing something else
      ecoact17 == -92 ~ -9,
      ecoact17 == -999 ~ -2,
      ecoact17 == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 5
    ecoact18 = case_when(
      ecoact18 == 3 ~ 1,
      ecoact18 %in% c(1, 5, 6) ~ 2,
      ecoact18 %in% c(2, 4) ~ 3,
      ecoact18 == 7 ~ 4,
      ecoact18 == 8 ~ 5,
      ecoact18 %in% 9:11 ~ 6,
      ecoact18 == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 6
    ecoact19 = case_when(
      ecoact19 == 3 ~ 1,
      ecoact19 %in% c(4, 5) ~ 2,
      ecoact19 %in% c(1, 2, 10) ~ 3,
      ecoact19 == 8 ~ 4,
      ecoact19 == 7 ~ 5,
      ecoact19 %in% c(6, 9, 11) ~ 6,
      ecoact19 == -91 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 7
    ecoact20 = case_when(
      ecoact20 == 3 ~ 1,
      ecoact20 %in% c(4, 5, 11) ~ 2,
      ecoact20 %in% c(1, 2, 9) ~ 3,
      ecoact20 == 8 ~ 4,
      ecoact20 == 7 ~ 5,
      ecoact20 %in% c(6, 10, 12:15) ~ 6,
      ecoact20 == -91 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 8
    ecoact25 = case_when(
      ecoactadu25 %in% c(1, 2) ~ 1,
      ecoactadu25 %in% c(6, 7) ~ 2,
      ecoactadu25 == 5 ~ 3,
      ecoactadu25 == 4 ~ 4,
      ecoactadu25 == 9 ~ 5,
      ecoactadu25 %in% c(3, 8, 10) ~ 6,
      ecoactadu25 == -9 ~ -9,
      ecoactadu25 == -8 ~ -8,
      ecoactadu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 9
    ecoact32 = case_when(
      ecoactadu32 %in% c(1, 2) ~ 1,
      ecoactadu32 %in% c(6, 7) ~ 2,
      ecoactadu32 == 5 ~ 3,
      ecoactadu32 == 4 ~ 4,
      ecoactadu32 == 9 ~ 5,
      ecoactadu32 %in% c(3, 8, 10) ~ 6,
      ecoactadu32 == -9 ~ -9,
      ecoactadu32 == -8 ~ -8,
      ecoactadu32 == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Detailed versions (S8, S9 only)
    ecoactadu25 = case_when(
      !is.na(ecoactadu25) ~ ecoactadu25,
      is.na(ecoactadu25) ~ -3
    ),
    ecoactadu32 = case_when(
      !is.na(ecoactadu32) ~ ecoactadu32,
      is.na(ecoactadu32) ~ -3
    )
  ) %>%
  mutate(across(c(ecoact17, ecoact18, ecoact19, ecoact20,ecoact25, ecoact32), 
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9), 
                         labels = c("In paid work",
                                    "Apprenticeship/government training scheme/training",
                                    "Education",
                                    "Unemployed",
                                    "Looking after home",
                                    "Other",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal"))),
         across(c(ecoactadu25, ecoactadu32),
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1, -2, -3, -8, -9), 
                         labels = c("employee – in paid work",
                                    "self employed",
                                    "voluntary work",
                                    "Unemployed",
                                    "Education",
                                    "Apprenticeship",
                                    "government employment scheme",
                                    "sick/disabled",
                                    "Looking after home/family",
                                    "Something else",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal")))
  ) %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20,
         ecoact25, ecoact32, ecoactadu25, ecoactadu32)

#### economic activity parents ####
# Load & select parental employment variables for Sweeps 1–4
ecoactDT_parents_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(NSID, ecoactdtma14 = W1empsmum, ecoactdtpa14 = W1empsdad),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(NSID, ecoactdtma15 = W2empsmum, ecoactdtpa15 = W2empsdad),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(NSID, ecoactdtma16 = W3empsmum, ecoactdtpa16 = W3empsdad),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(NSID, ecoactdtma17 = w4empsmum, ecoactdtpa17 = w4empsdad)
)

# Merge all
ecoactDT_parents_all <- reduce(ecoactDT_parents_vars, full_join, by = "NSID")

# Recode helper function
recode_detailed <- function(x) {
  case_when(
    x == 1 ~ 1,  # FT
    x == 2 ~ 2,  # PT
    x == 3 ~ 3,  # Unemployed
    x == 4 ~ 4,  # Training
    x == 5 ~ 5,  # Education
    x == 6 ~ 6,  # Home
    x == 7 ~ 7,  # Retired
    x == 8 ~ 8,  # Sick/disabled
    x == 9 ~ 9,  # Other
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -999 ~ -2,
    x %in% c(-996, -98, -99) ~ -3,
    is.na(x) ~ -3,
    TRUE ~ NA_real_
  )
}

# Apply recode to each sweep
ecoactDT_parents_all <- ecoactDT_parents_all %>%
  mutate(
    ecoactdtma14 = recode_detailed(ecoactdtma14),
    ecoactdtpa14 = recode_detailed(ecoactdtpa14),
    ecoactdtma15 = recode_detailed(ecoactdtma15),
    ecoactdtpa15 = recode_detailed(ecoactdtpa15),
    ecoactdtma16 = recode_detailed(ecoactdtma16),
    ecoactdtpa16 = recode_detailed(ecoactdtpa16),
    ecoactdtma17 = recode_detailed(ecoactdtma17),
    ecoactdtpa17 = recode_detailed(ecoactdtpa17)
  )%>%
  mutate(across(c(starts_with("ecoactdtma"), starts_with("ecoactdtpa")), 
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -2, -3, -8, -9), 
                         labels = c("FT paid work",
                                    "PT paid work",
                                    "Unemployed",
                                    "Training",
                                    "Education",
                                    "Looking after home/family",
                                    "Retired from work altogether",
                                    "Sick/disabled",
                                    "Other",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal")))
  ) %>%
  select(NSID, starts_with("ecoactdtma"), starts_with("ecoactdtpa"))

#### NS-SEC own ####
# Load NS-SEC variables from relevant sweeps
nssec_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID, nssec17 = W4nsseccatYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>%
    select(NSID, nssec18 = W5nsseccatYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(NSID, nssec19 = w6nsseccatYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(NSID, nssec20 = W7NSSECCat),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(NSID, nssec25 = W8DNSSEC17, ecoactadu25 = W8DACTIVITYC),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, nssec32 = W9NSSEC)
)

# Merge all NS-SEC variables by NSID
nssec_all <- reduce(nssec_vars, full_join, by = "NSID")

# Harmonise NS-SEC values and derive categories
nssec_all <- nssec_all %>%
  mutate(
    ## Sweep 4 (age 17)
    nssec17 = case_when(
      is.na(nssec17) ~ -3,
      floor(nssec17) %in% 1:17 ~ floor(nssec17),
      nssec17 == -91 ~ -1,
      nssec17 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 5 (age 18)
    nssec18 = case_when(
      is.na(nssec18) ~ -3,
      floor(nssec18) %in% 1:17 ~ floor(nssec18),
      nssec18 == -91 ~ -1,
      nssec18 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 6 (age 19)
    nssec19 = case_when(
      is.na(nssec19) ~ -3,
      floor(nssec19) %in% 1:17 ~ floor(nssec19),
      nssec19 == -91 ~ -1,
      nssec19 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 7 (age 20)
    nssec20 = case_when(
      is.na(nssec20) ~ -3,
      floor(nssec20) %in% 1:17 ~ floor(nssec20),
      nssec20 == -91 ~ -1,
      nssec20 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 8 (age 25)
    nssec25 = case_when(
      is.na(nssec25) ~ -3,
      floor(nssec25) %in% 1:14 ~ floor(nssec25),
      ecoactadu25 == 5 ~ 15, # full-time student
      nssec25 == -9 ~ -9,
      nssec25 == -8 ~ -8,
      nssec25 == -1 ~ -1,
    ),
    ## Sweep 9 (age 32)
    nssec32 = case_when(
      is.na(nssec32) ~ -3,
      nssec32 %in% 1:17 ~ nssec32,
      nssec32 == -9 ~ -9,
      nssec32 == -8 ~ -8,
      nssec32 == -1 ~ -1
    )
  ) %>%
  mutate(across(starts_with("nssec"), 
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, -1, -2, -3, -8, -9), 
                         labels = c("Employers in large organisations",
                                    "Higher managerial and administrative occupations",
                                    "Higher professional occupations",
                                    "Lower professional and higher technical occupations",
                                    "Lower managerial and administrative occupations",
                                    "Higher supervisory occupations",
                                    "Intermediate occupations",
                                    "Employers in small establishments",
                                    "Own account workers",
                                    "Lower supervisory occupations",
                                    "Lower technical occupations",
                                    "Semi-routine occupations",
                                    "Routine occupations",
                                    "Never worked and long-term unemployed",
                                    "Full-time student",
                                    "Not classified or inadequately stated",
                                    "Not classifiable for other reasons",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal")))
  ) %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

#### NS-SEC parents ####
# Load and select parental NS-SEC variables from Sweeps 1–5
nssec_parents_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(NSID, nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(NSID, nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(NSID, nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(NSID, nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad),
  S5 = read_dta(file.path(data_path, sweeps$S5familybackground)) %>%
    select(NSID, nssecma18 = w5Cnsseccatmum, nssecpa18 = w5Cnsseccatdad)
)

# Merge all parental NS-SEC variables by NSID
nssec_parents_all <- reduce(nssec_parents_vars, full_join, by = "NSID")

# Harmonise values (preserve decimals, apply missing codes)
recode_nssec_detail <- function(x) {
  case_when(
    floor(x) %in% 1:17 ~ floor(x),
    x %in% c(-999, -94) ~ -2,
    x %in% c(-99, -98) | is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Apply recode and assign to derived variables
nssec_parents_all <- nssec_parents_all %>%
  mutate(
    nssecma14 = recode_nssec_detail(nssecma14),
    nssecpa14 = recode_nssec_detail(nssecpa14),
    nssecma15 = recode_nssec_detail(nssecma15),
    nssecpa15 = recode_nssec_detail(nssecpa15),
    nssecma16 = recode_nssec_detail(nssecma16),
    nssecpa16 = recode_nssec_detail(nssecpa16),
    nssecma17 = recode_nssec_detail(nssecma17),
    nssecpa17 = recode_nssec_detail(nssecpa17),
    nssecma18 = recode_nssec_detail(nssecma18),
    nssecpa18 = recode_nssec_detail(nssecpa18)
  ) %>%
  mutate(across(c(starts_with("nssecma"), starts_with("nssecpa")),
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, -1, -2, -3, -8, -9), 
                         labels = c("Employers in large organisations",
                                    "Higher managerial and administrative occupations",
                                    "Higher professional occupations",
                                    "Lower professional and higher technical occupations",
                                    "Lower managerial and administrative occupations",
                                    "Higher supervisory occupations",
                                    "Intermediate occupations",
                                    "Employers in small establishments",
                                    "Own account workers",
                                    "Lower supervisory occupations",
                                    "Lower technical occupations",
                                    "Semi-routine occupations",
                                    "Routine occupations",
                                    "Never worked and long-term unemployed",
                                    "Full-time student",
                                    "Not classified or inadequately stated",
                                    "Not classifiable for other reasons",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal")))
  ) %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, 
         nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)


#### house ownership ####
# Load and select house ownership variables from relevant sweeps
housing_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(NSID, hown14 = W1hous12HH),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(NSID, hown15 = W2Hous12HH),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(NSID, hown16 = W3hous12HH),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(NSID, hown17 = W4Hous12HH),
  S5 = read_dta(file.path(data_path, sweeps$S5familybackground)) %>%
    select(NSID, W5Hous12HH, W5Hous12BHH, W5Hous12CHH),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(NSID, W6Hous12YP, W6Hous12bYP, W6Hous12cYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(NSID, W7Hous12YP, W7Hous12bYP, W7Hous12cYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(NSID, hown25 = W8TENURE),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, hown32 = W9DTENURE)
)

# Merge all datasets
hown_all <- reduce(housing_vars, full_join, by = "NSID")

# Derive harmonised variables
hown_all <- hown_all %>%
  mutate(# Detailed versions for S1-S7
    hownteen14 = case_when(
      hown14 > 0 ~ hown14,
      hown14 == -999 ~ -2,
      hown14 == -92 ~ -9,
      hown14 == -91 ~ -1,
      hown14 == -1 ~ -8,
      is.na(hown14) ~ -3
    ),
    hownteen15 = case_when(
      hown15 > 0 ~ hown15,
      hown15 == -999 ~ -2,
      hown15 == -92 ~ -9,
      hown15 == -91 ~ -1,
      hown15 == -1 ~ -8,
      is.na(hown15) ~ -3
    ),
    hownteen16 = case_when(
      hown16 > 0 ~ hown16,
      hown16 == -999 ~ -2,
      hown16 == -92 ~ -9,
      hown16 == -91 ~ -1,
      hown16 == -1 ~ -8,
      is.na(hown16) ~ -3
    ),
    hownteen17 = case_when(
      hown17 > 0 ~ hown17,
      hown17 == -999 ~ -2,
      hown17 == -92 ~ -9,
      hown17 == -91 ~ -1,
      hown17 == -1 ~ -8,
      is.na(hown17) ~ -3
    ),
    hownteen18 = case_when(
      W5Hous12BHH == 1 ~ 1, # Owned outright
      W5Hous12BHH == 2 ~ 2, # Being bought on a mortgage/bank loan
      W5Hous12BHH == 3 ~ 3, # Shared ownership (owns & rents property)
      W5Hous12CHH == 1 ~ 4, # Rented from a Council or New Town
      W5Hous12CHH == 2 ~ 5, # Rented from a Housing Association
      W5Hous12CHH == 3 ~ 6, # Rented privately 
      W5Hous12CHH == 4 ~ 7, # Rent free 
      W5Hous12HH == 3 | W5Hous12BHH == 4 | W5Hous12CHH == 5 ~ 8, # Other
      W5Hous12BHH %in% c(-999, -92) | W5Hous12CHH == -92 ~ -9,
      W5Hous12BHH == -91 | W5Hous12CHH == -91 ~ -1,
      W5Hous12BHH == -1 | W5Hous12CHH == -1 ~ -8,
      is.na(W5Hous12BHH) & is.na(W5Hous12CHH) ~ -3
    ),
    hownteen19 = case_when(
      W6Hous12bYP == 1 ~ 1,
      W6Hous12bYP == 2 ~ 2,
      W6Hous12bYP == 3 ~ 3,
      W6Hous12cYP == 1 ~ 4,
      W6Hous12cYP == 2 ~ 5,
      W6Hous12cYP == 3 ~ 6,
      W6Hous12cYP == 4 ~ 7,
      W6Hous12YP == 3 | W6Hous12bYP == 4 | W6Hous12cYP == 5 ~ 8,
      W6Hous12bYP %in% c(-999, -92) | W6Hous12cYP == -92 ~ -9,
      W6Hous12bYP == -91 | W6Hous12cYP == -91 ~ -1,
      W6Hous12bYP == -1 | W6Hous12cYP == -1 ~ -8,
      is.na(W6Hous12bYP) & is.na(W6Hous12cYP) ~ -3
    ),
    hownteen20 = case_when(
      W7Hous12bYP == 1 ~ 1,
      W7Hous12bYP == 2 ~ 2,
      W7Hous12bYP == 3 ~ 3,
      W7Hous12cYP == 1 ~ 4,
      W7Hous12cYP == 2 ~ 5,
      W7Hous12cYP == 3 ~ 6,
      W7Hous12cYP == 4 ~ 7,
      W7Hous12YP == 3 | W7Hous12bYP == 4 | W7Hous12cYP == 5 ~ 8,
      W7Hous12bYP %in% c(-999, -92) | W7Hous12cYP == -92 ~ -9,
      W7Hous12bYP == -91 | W7Hous12cYP == -91 ~ -1,
      W7Hous12bYP == -1 | W7Hous12cYP == -1 ~ -8,
      is.na(W7Hous12bYP) & is.na(W7Hous12cYP) ~ -3
    )) %>%
  mutate(
    hown14 = case_when(
      hown14 == 1 ~ 1, # own outright 
      hown14 == 2 ~ 2, # own, buying with help of mortgage/loan
      hown14 == 3 ~ 3, # part rent, part mortgage
      hown14 %in% 4:6 ~ 4, # rent it
      hown14 == 7 ~ 5, # live-in rent free
      hown14 == 8 ~ 6, # other
      hown14 == -999 ~ -2,
      hown14 == -92 ~ -9,
      hown14 == -91 ~ -1,
      hown14 == -1 ~ -8,
      is.na(hown14) ~ -3
    ),
    hown15 = case_when(
      hown15 == 1 ~ 1,
      hown15 == 2 ~ 2,
      hown15 == 3 ~ 3,
      hown15 %in% 4:6 ~ 4,
      hown15 == 7 ~ 5,
      hown15 == 8 ~ 6,
      hown15 == -999 ~ -2,
      hown15 == -92 ~ -9,
      hown15 == -91 ~ -1,
      hown15 == -1 ~ -8,
      is.na(hown15) ~ -3
    ),
    hown16 = case_when(
      hown16 == 1 ~ 1,
      hown16 == 2 ~ 2,
      hown16 == 3 ~ 3,
      hown16 %in% 4:6 ~ 4,
      hown16 == 7 ~ 5,
      hown16 == 8 ~ 6,
      hown16 == -999 ~ -2,
      hown16 == -92 ~ -9,
      hown16 == -91 ~ -1,
      hown16 == -1 ~ -8,
      is.na(hown16) ~ -3
    ),
    hown17 = case_when(
      hown17 == 1 ~ 1,
      hown17 == 2 ~ 2,
      hown17 == 3 ~ 3,
      hown17 %in% 4:6 ~ 4,
      hown17 == 7 ~ 5,
      hown17 == 8 ~ 6,
      hown17 == -999 ~ -2,
      hown17 == -92 ~ -9,
      hown17 == -91 ~ -1,
      hown17 == -1 ~ -8,
      is.na(hown17) ~ -3
    ),
    hown18 = case_when(
      W5Hous12BHH == 1 ~ 1,
      W5Hous12BHH == 2 ~ 2,
      W5Hous12BHH == 3 ~ 3,
      W5Hous12CHH %in% 1:3 ~ 4,
      W5Hous12CHH == 4 ~ 5,
      W5Hous12BHH == 4 | W5Hous12CHH == 5 ~ 6,
      W5Hous12BHH %in% c(-999, -92) | W5Hous12CHH == -92 ~ -9,
      W5Hous12BHH == -91 | W5Hous12CHH == -91 ~ -1,
      W5Hous12BHH == -1 | W5Hous12CHH == -1 ~ -8,
      is.na(W5Hous12BHH) & is.na(W5Hous12CHH) ~ -3
    ),
    hown19 = case_when(
      W6Hous12bYP == 1 ~ 1,
      W6Hous12bYP == 2 ~ 2,
      W6Hous12bYP == 3 ~ 3,
      W6Hous12cYP %in% 1:3 ~ 4,
      W6Hous12cYP == 4 ~ 5,
      W6Hous12bYP == 4 | W6Hous12cYP == 5 ~ 6,
      W6Hous12bYP %in% c(-999, -92) | W6Hous12cYP == -92 ~ -9,
      W6Hous12bYP == -91 | W6Hous12cYP == -91 ~ -1,
      W6Hous12bYP == -1 | W6Hous12cYP == -1 ~ -8,
      is.na(W6Hous12bYP) & is.na(W6Hous12cYP) ~ -3
    ),
    hown20 = case_when(
      W7Hous12bYP == 1 ~ 1,
      W7Hous12bYP == 2 ~ 2,
      W7Hous12bYP == 3 ~ 3,
      W7Hous12cYP %in% 1:3 ~ 4,
      W7Hous12cYP == 4 ~ 5,
      W7Hous12bYP == 4 | W7Hous12cYP == 5 ~ 6,
      W7Hous12bYP %in% c(-999, -92) | W7Hous12cYP == -92 ~ -9,
      W7Hous12bYP == -91 | W7Hous12cYP == -91 ~ -1,
      W7Hous12bYP == -1 | W7Hous12cYP == -1 ~ -8,
      is.na(W7Hous12bYP) & is.na(W7Hous12cYP) ~ -3
    ),
    hown25 = case_when(
      hown25 == 1 ~ 1,
      hown25 == 2 ~ 2,
      hown25 == 3 ~ 3,
      hown25 == 4 ~ 4,
      hown25 == 5 ~ 5,
      hown25 %in% 6:7 ~ 6,
      hown25 == -9 ~ -9,
      hown25 == -8 ~ -8,
      hown25 == -1 ~ -1,
      is.na(hown25) ~ -3
    ),
    hown32 = case_when(
      hown32 == 1 ~ 1,
      hown32 == 2 ~ 2,
      hown32 == 3 ~ 3,
      hown32 == 4 ~ 4,
      hown32 == 5 ~ 5,
      hown32 %in% 6:7 ~ 6,
      hown32 == -9 ~ -9,
      hown32 == -8 ~ -8,
      hown32 == -1 ~ -1,
      is.na(hown32) ~ -3
    )) %>%
  mutate(across(c(hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20), 
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9), 
                         labels = c("Owned outright",
                                    "Being bought on a mortgage/bank loan",
                                    "Shared ownership (owns & rents property)",
                                    "Rented from a Council or New Town",
                                    "Rented from a Housing Association",
                                    "Rented privately",
                                    "Rent free",
                                    "Some other arrangement",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal"))),
         across(c(hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32),
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9), 
                         labels = c("Owned outright",
                                    "Owned, buying with help of mortgage/loan",
                                    "Spart rent, part mortgage",
                                    "Rent it",
                                    "live rent-free",
                                    "Other",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal")))
         ) %>%
  select(NSID, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32,
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)


#### income own + partner ####
# Load and select income variables from relevant sweeps
income_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(NSID), 
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(NSID, inc25 = W8DINCB),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, inc32 = W9DINCB)
)

# Merge all income variables by NSID
income_all <- reduce(income_vars, full_join, by = "NSID")

# Recode
income_all <- income_all %>%
  mutate(
    inc25 = case_when(
      is.na(inc25) ~ -3,
      TRUE ~ inc25
    ),
    inc32 = case_when(
      is.na(inc32) ~ -3,
      TRUE ~ inc32
    )
  ) %>%
  mutate(across(c(inc25, inc32), 
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, -1, -2, -3, -8, -9), 
                         labels = c("less than £25 per week",
                                    "25-50",
                                    "50-90",
                                    "90-140",
                                    "140-240",
                                    "240-300",
                                    "300-350",
                                    "350-400",
                                    "400-500",
                                    "500-600",
                                    "600-700",
                                    "700-800",
                                    "800-900",
                                    "900-1200",
                                    "1200-1400",
                                    "more than 1400",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal"))
  )) %>%
  select(NSID, inc25, inc32)

#### income parents ####
# Load and select household income variables
hh_income_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(NSID, incwhh14 = W1GrsswkHH),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(NSID, incwhh15 = W2GrsswkHH),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(NSID, incwhh16 = W3incestw),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(NSID, incwhh17 = w4IncEstW)
)

# Merge all household income variables by NSID
hh_income_all <- reduce(hh_income_vars, full_join, by = "NSID")

# Derive banded income for continuous measures (S1–S2)
convert_to_band <- function(x) {
  case_when(
    x < 0 ~ x,
    x < 25 ~ 1,
    x < 50 ~ 2,
    x < 90 ~ 3,
    x < 140 ~ 4,
    x < 240 ~ 5,
    x < 300 ~ 6,
    x < 350 ~ 7,
    x < 400 ~ 8,
    x < 500 ~ 9,
    x < 600 ~ 10,
    x < 700 ~ 11,
    x < 800 ~ 12,
    x < 900 ~ 13,
    x < 1200 ~ 14,
    x < 1400 ~ 15,
    x >= 1400 ~ 16
  )
}

hh_income_all <- hh_income_all %>%
  mutate(
    # Sweep 1
    incwhh14 = case_when(
      is.na(incwhh14) ~ -3,
      incwhh14 == -92 ~ -9,
      incwhh14 %in% c(-999, -992, -94) ~ -2,
      incwhh14 == -99 ~ -3,
      incwhh14 == -91 ~ -1,
      incwhh14 == -1 ~ -8,
      incwhh14 == -3 ~ -3,
      TRUE ~ convert_to_band(incwhh14)
    ),
    incwhhcnt14 = case_when(
      is.na(incwhh14) ~ -3,
      incwhh14 == -92 ~ -9,
      incwhh14 %in% c(-999, -992, -94) ~ -2,
      incwhh14 == -99 ~ -3,
      incwhh14 == -91 ~ -1,
      incwhh14 == -1 ~ -8,
      incwhh14 == -3 ~ -3,
      TRUE ~ incwhh14
    ),
    
    # Sweep 2
    incwhh15 = case_when(
      is.na(incwhh15) ~ -3,
      incwhh15 == -92 ~ -9,
      incwhh15 %in% c(-999, -992, -94) ~ -2,
      incwhh15 == -99 ~ -3,
      incwhh15 == -91 ~ -1,
      incwhh15 == -1 ~ -8,
      incwhh15 == -3 ~ -3,
      TRUE ~ convert_to_band(incwhh15)
    ),
    incwhhcnt15 = case_when(
      is.na(incwhh15) ~ -3,
      incwhh15 == -92 ~ -9,
      incwhh15 %in% c(-999, -992, -94) ~ -2,
      incwhh15 == -99 ~ -3,
      incwhh15 == -91 ~ -1,
      incwhh15 == -1 ~ -8,
      incwhh15 == -3 ~ -3,
      TRUE ~ incwhh15
    ),
    
    # Sweep 3
    incwhh16 = case_when(
      is.na(incwhh16) ~ -3,
      incwhh16 == -99 ~ -3,
      incwhh16 == -92 ~ -9,
      incwhh16 == -91 ~ -8,
      incwhh16 >= 1 & incwhh16 <= 12 ~ incwhh16
    ),
    
    # Sweep 4
    incwhh17 = case_when(
      is.na(incwhh17) ~ -3,
      incwhh17 == -99 ~ -3,
      incwhh17 == -92 ~ -9,
      incwhh17 == -91 ~ -8,
      incwhh17 >= 1 & incwhh17 <= 12 ~ incwhh17
    )
  ) %>%
  mutate(across(c(incwhh14, incwhh15), 
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, -1, -2, -3, -8, -9), 
                         labels = c("less than £25 per week",
                                    "25-50",
                                    "50-90",
                                    "90-140",
                                    "140-240",
                                    "240-300",
                                    "300-350",
                                    "350-400",
                                    "400-500",
                                    "500-600",
                                    "600-700",
                                    "700-800",
                                    "800-900",
                                    "900-1200",
                                    "1200-1400",
                                    "more than 1400",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal"))),
         across(c(incwhhcnt14, incwhhcnt15), 
                ~ labelled(.x, 
                           labels = c("Item not applicable" = -1, 
                                      "Script error/information lost" = -2,
                                      "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                      "Don’t know/insufficient information" = -8,
                                      "Refusal" = -9))),
         across(c(incwhh16, incwhh17), 
                ~ factor(.x, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -1, -2, -3, -8, -9), 
                         labels = c("up to 49",
                                    "50-99",
                                    "100-199",
                                    "200-299",
                                    "300-399",
                                    "400-499",
                                    "500-599",
                                    "600-699",
                                    "700-799",
                                    "800-899",
                                    "900-999",
                                    "1000 or more",
                                    "Item not applicable", 
                                    "Script error/information lost",
                                    "Not asked at the fieldwork stage/participated/interviewed", 
                                    "Don’t know/insufficient information",
                                    "Refusal")))
         ) %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

#### IMD ####
# Load IMD variables from relevant sweeps
imd_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%select(NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
      select(NSID, imd15 = IMDRSCORE),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
      select(NSID, imd16 = IMDRSCORE),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
      select(NSID, imd32 = W9DIMDD)
  )

# Merge all IMD variables by NSID  
imd_all <- reduce(imd_vars, full_join, by = "NSID")
  
# Recode derived variables
imd_all <- imd_all %>%
    mutate(
      imd15 = case_when(
        is.na(imd15) ~ -3,
        imd15 == -94 ~ -8,
        TRUE ~ imd15
      ),
      
      imd16 = case_when(
        is.na(imd16) ~ -3,
        imd16 == -94 ~ -8,
        TRUE ~ imd16
      ),
      
      imd32 = case_when(
        is.na(imd32) ~ -3,
        imd32 == -8 ~ -8,
        TRUE ~ imd32
      )
    ) %>%
  mutate(across(c(imd15, imd16, imd32), 
                ~ labelled(.x, 
                           labels = c("Item not applicable" = -1, 
                                      "Script error/information lost" = -2,
                                      "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                      "Don’t know/insufficient information" = -8)))) %>%
  select(NSID, imd15, imd16, imd32)

#### GHQ ####
# Load GHQ-12 derived score and item-level data
ghq_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, ghq15 = W2ghq12scr,
           paste0("W2", c("concenYP", "nosleepYP", "usefulYP", "decideYP", "strainYP", 
                          "difficYP", "activYP", "probsYP", "depressYP", "noconfYP", 
                          "wthlessYP", "happyYP"))),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID, ghq17 = W4ghq12scr,
           paste0("W4", c("ConcenYP", "NoSleepYP", "UsefulYP", "DecideYP", "StrainYP", 
                          "DifficYP", "ActivYP", "ProbsYP", "DepressYP", "NoConfYP", 
                          "WthlessYP", "HappyYP"))),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(NSID, starts_with("W8GHQ12_")),
  S8_derive = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(NSID, ghq25 = W8DGHQSC),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, starts_with("W9GHQ12_")),
  S9_derive = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, ghq32 = W9DGHQSC)
)

# Merge all sweeps by NSID
ghq_all <- reduce(ghq_vars, full_join, by = "NSID")

# Define item lists for sum scores
ghq_items <- list(
  ghqtl15 = paste0("W2", c("concenYP", "nosleepYP", "usefulYP", "decideYP", "strainYP", 
                           "difficYP", "activYP", "probsYP", "depressYP", "noconfYP", 
                           "wthlessYP", "happyYP")),
  ghqtl17 = paste0("W4", c("ConcenYP", "NoSleepYP", "UsefulYP", "DecideYP", "StrainYP", 
                           "DifficYP", "ActivYP", "ProbsYP", "DepressYP", "NoConfYP", 
                           "WthlessYP", "HappyYP")),
  ghqtl25 = paste0("W8GHQ12_", 1:12),
  ghqtl32 = paste0("W9GHQ12_", 1:12)
)

# Derive GHQ sum scores (0–12) with custom missing logic
ghq_all <- ghq_all %>%
  mutate(
    ghqtl15 = {
      items <- select(., all_of(ghq_items$ghqtl15))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE)) # has missing value(s) in any of the GHQ items
      all_na <- apply(items, 1, function(x) all(is.na(x))) # all GHQ items are missing - not participating
      score <- rowSums(items, na.rm = TRUE) # sum GHQ items, ignoring NAs
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    ghqtl17 = {
      items <- select(., all_of(ghq_items$ghqtl17))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    ghqtl25 = {
      items <- select(., all_of(ghq_items$ghqtl25))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    ghqtl32 = {
      items <- select(., all_of(ghq_items$ghqtl32))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    }
  ) %>%
  mutate(
    ghq15 = case_when(
      is.na(ghq15) ~ -3,
      ghq15 == -99 ~ -3,
      ghq15 %in% c(-97, -96, -92) ~ -9,
      TRUE ~ ghq15
    ),
    ghq17 = case_when(
      is.na(ghq17) ~ -3,
      ghq17 == -99 ~ -3,
      ghq17 %in% c(-97, -96, -92) ~ -9,
      TRUE ~ ghq17
    ),
    ghq25 = if_else(is.na(ghq25), -3, ghq25),
    ghq32 = if_else(is.na(ghq32), -3, ghq32)
  )%>%
  mutate(across(c(ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32), 
                ~ labelled(.x, 
                           labels = c("Item not applicable" = -1, 
                                      "Script error/information lost" = -2,
                                      "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                      "Don’t know/insufficient information" = -8,
                                      "Refusal" = -9)))) %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32,
         ghqtl15, ghqtl17, ghqtl25, ghqtl32)


#### life satisfaction ####
# Load life satisfaction variables from each sweep
lsat_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(NSID),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(NSID, lsat20 = W7OSatisYP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(NSID, lsat25 = W8OSATIS),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, lsat32 = W9OSATIS)
)

# Merge into a single dataset
lsat_all <- reduce(lsat_vars, full_join, by = "NSID")

# Harmonise life satisfaction variables
lsat_all <- lsat_all %>%
  mutate(
    lsat20 = case_when(
      lsat20 %in% 1:5 ~ lsat20,
      lsat20 %in% c(-97, -92) ~ -9,
      lsat20 == -91 ~ -1,
      lsat20 == -1 ~ -8,
      is.na(lsat20) ~ -3,
      TRUE ~ -2
    ),
    lsat25 = case_when(
      lsat25 %in% 1:5 ~ lsat25,
      lsat25 == -9 ~ -9,
      lsat25 == -8 ~ -8,
      lsat25 == -1 ~ -1,
      is.na(lsat25) ~ -3,
      TRUE ~ -2
    ),
    lsat32 = case_when(
      lsat32 %in% 1:5 ~ lsat32,
      lsat32 == -9 ~ -9,
      lsat32 == -8 ~ -8,
      lsat32 == -1 ~ -1,
      is.na(lsat32) ~ -3,
      TRUE ~ -2
    )
  ) %>%
  mutate(across(c(lsat20, lsat25, lsat32), 
                ~ labelled(.x, 
                           labels = c("Item not applicable" = -1, 
                                      "Script error/information lost" = -2,
                                      "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                      "Don’t know/insufficient information" = -8,
                                      "Refusal" = -9)))) %>%
  select(NSID, lsat20, lsat25, lsat32)

#### self-harm ####
# Load S8 self-harm variables
sharm_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
  select(NSID, sharm25_ever = W8HARM, sharm25_freq = W8HARM2))

# Merge by NSID
sharm_all <- reduce(sharm_vars, full_join, by = "NSID")

# Recode to harmonised sharm25
sharm_all <- sharm_all %>%
  mutate(
    sharm25 = case_when(
      is.na(sharm25_ever) & is.na(sharm25_freq) ~ -3,  
      sharm25_ever == 2 ~ 0,                          
      sharm25_freq == 9 ~ 1,                         
      sharm25_freq == 8 ~ 2,                          
      sharm25_freq == 7 ~ 3,                         
      sharm25_freq == 6 ~ 4,                         
      sharm25_freq == 5 ~ 5,                          
      sharm25_freq == 4 ~ 6,                          
      sharm25_freq == 3 ~ 7,                         
      sharm25_freq == 2 ~ 8,                          
      sharm25_freq == 1 ~ 9,                          
      sharm25_freq == -9 ~ -9,                        
      sharm25_freq == -8 ~ -8,                      
      sharm25_freq == -1 ~ -1,                       
      TRUE ~ -2                                
    )
  ) %>%
  mutate(sharm25 = labelled(sharm25, 
                            labels = c("Item not applicable" = -1, 
                                       "Script error/information lost" = -2,
                                       "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                       "Don’t know/insufficient information" = -8,
                                       "Refusal" = -9))) %>%
  select(NSID, sharm25)

#### GAD ####
# Load GAD-7 derived score and item-level data
gad_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, gad32 = W9DGAD2))

# Merge by NSID
gad_all <- reduce(gad_vars, full_join, by = "NSID")

# Recode to harmonised gad32
gad_all <- gad_all %>%
  mutate(
    gad32 = case_when(
      !is.na(gad32) ~ gad32,                    
      TRUE ~ -3                                
    )
  ) %>%
  mutate(gad32 = labelled(gad32, 
                            labels = c("Item not applicable" = -1, 
                                       "Script error/information lost" = -2,
                                       "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                       "Don’t know/insufficient information" = -8,
                                       "Refusal" = -9))) %>%
  select(NSID, gad32)

#### PHQ ####
# Load GAD-7 derived score and item-level data
phq_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, phq32 = W9DPHQ2))

# Merge by NSID
phq_all <- reduce(phq_vars, full_join, by = "NSID")

# Recode to harmonised phq32
phq_all <- phq_all %>%
  mutate(
    phq32 = case_when(
      !is.na(phq32) ~ phq32,                    
      TRUE ~ -3                                
    )
  ) %>%
  mutate(phq32 = labelled(phq32, 
                          labels = c("Item not applicable" = -1, 
                                     "Script error/information lost" = -2,
                                     "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                     "Don’t know/insufficient information" = -8,
                                     "Refusal" = -9))) %>%
  select(NSID, phq32)

#### UCLA Loneliness ####
# Load GAD-7 derived score and item-level data
lon_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, lon32 = W9DLONELINESS))

# Merge by NSID
lon_all <- reduce(lon_vars, full_join, by = "NSID")

# Recode to harmonised lon32
lon_all <- lon_all %>%
  mutate(
    lon32 = case_when(
      !is.na(lon32) ~ lon32,                    
      TRUE ~ -3                                
    )
  ) %>%
  mutate(lon32 = labelled(lon32, 
                          labels = c("Item not applicable" = -1, 
                                     "Script error/information lost" = -2,
                                     "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                     "Don’t know/insufficient information" = -8,
                                     "Refusal" = -9))) %>%
  select(NSID, lon32)


#### self-rated general health ####
# Load relevant sweep files and select needed variables
health_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(NSID, ghea15 = W2hea1cYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% 
    select(NSID, ghea16 = W3hea1cYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID, ghea17 = W4Hea1CYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% 
    select(NSID, ghea25 = W8GENA),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(NSID, ghea32 = W9HLTHGEN)
)

# Merge all data by NSID
health_all <- reduce(health_vars, full_join, by = "NSID")

# Harmonise adolescent responses (S2–S4): gheateenXX
health_all <- health_all %>%
  mutate(
    gheateen15 = case_when(
      ghea15 %in% c(3, 4, 5, 6) ~ ghea15 - 2,
      ghea15 %in% c(-998, -997, -995, -99, -94) ~ -2,
      ghea15 %in% c(-97, -96, -92, -1) ~ -8,
      ghea15 == -91 ~ -1,
      TRUE ~ -3
      ),  
    gheateen16 = case_when(
      ghea16 %in% c(1, 2, 3, 4) ~ ghea16,
      ghea16 %in% c(-998, -997, -995, -99, -94) ~ -2,
      ghea16 %in% c(-97, -96, -92, -1) ~ -8,
      ghea16 == -91 ~ -1,
      TRUE ~ -3
    ),
    gheateen17 = case_when(
      ghea17 %in% c(1, 2, 3, 4) ~ ghea17,
      ghea17 %in% c(-998, -997, -995, -99, -94) ~ -2,
      ghea17 %in% c(-97, -96, -92, -1) ~ -8,
      ghea17 == -91 ~ -1,
      TRUE ~ -3
    )
  )%>%
  mutate(
    # Harmonise adult responses (S8–S9): gheaaduXX
    gheaadu25 = case_when(
      ghea25 %in% c(1, 2, 3, 4, 5) ~ ghea25,
      ghea25 == -8 ~ -8,
      ghea25 == -1 ~ -1,
      ghea25 == -9 ~ -9,
      TRUE ~ -3
    ),
    gheaadu32 = case_when(
      ghea32 %in% c(1, 2, 3, 4, 5) ~ ghea32,
      ghea32 == -8 ~ -8,
      ghea32 == -1 ~ -1,
      ghea32 == -9 ~ -9,
      TRUE ~ -3
    )
  )

# Binary general health: 1 = poor/fair, 0 = good/excellent
health_all <- health_all %>%
  mutate(
    ghea15 = case_when(gheateen15 %in% c(3, 4) ~ 1,
                       gheateen15 %in% c(1, 2) ~ 0,
                       TRUE ~ gheateen15),
    ghea16 = case_when(gheateen16 %in% c(3, 4) ~ 1,
                       gheateen16 %in% c(1, 2) ~ 0,
                       TRUE ~ gheateen16),
    ghea17 = case_when(gheateen17 %in% c(3, 4) ~ 1,
                       gheateen17 %in% c(1, 2) ~ 0,
                       TRUE ~ gheateen17),
    ghea25 = case_when(gheaadu25 %in% c(4, 5) ~ 1,
                       gheaadu25 %in% c(1, 2, 3) ~ 0,
                       TRUE ~ gheaadu25),
    ghea32 = case_when(gheaadu32 %in% c(4, 5) ~ 1,
                       gheaadu32 %in% c(1, 2, 3) ~ 0,
                       TRUE ~ gheaadu32)
  ) %>%
  mutate(across(c(gheateen15, gheateen16, gheateen17), ~ factor(.x, 
                                                                levels = c(1, 2, 3, 4, -1, -2, -3, -8, -9), 
                                                                labels = c("Very good",
                                                                           "Fairly good",
                                                                           "Not very good",
                                                                           "Not good at all",
                                                                           "Item not applicable", 
                                                                           "Script error/information lost",
                                                                           "Not asked at the fieldwork stage/participated/interviewed",
                                                                           "Don’t know/insufficient information",
                                                                           "Refusal"))),
         across(c(gheaadu25, gheaadu32), ~ factor(.x, 
                                                  levels = c(1, 2, 3, 4, 5, -1, -2, -3, -8, -9), 
                                                  labels = c("Excellent",
                                                             "Very good",
                                                             "Good",
                                                             "Fair",
                                                             "Poor",
                                                             "Item not applicable", 
                                                             "Script error/information lost",
                                                             "Not asked at the fieldwork stage/participated/interviewed",
                                                             "Don’t know/insufficient information",
                                                             "Refusal"))),
         across(c(ghea15, ghea16, ghea17, ghea25, ghea32), ~ factor(.x, 
                                                  levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                  labels = c("Good/fairly good/very good/excellent",
                                                             "Poor/not good at all/not very good/fair",
                                                             "Item not applicable", 
                                                             "Script error/information lost",
                                                             "Not asked at the fieldwork stage/participated/interviewed",
                                                             "Don’t know/insufficient information",
                                                             "Refusal")))
  ) %>%
  select(NSID, gheateen15, gheateen16, gheateen17,
         gheaadu25, gheaadu32,
         ghea15, ghea16, ghea17, ghea25, ghea32)

#### long-term illness ####
# Load relevant sweep files and select needed variables
long_term_illness_files <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID, lsi14 = W1chea1HS),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(NSID, lsi15 = W2chea1HS),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID, lsi17 = W4Hea2YP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(NSID, lsi19 = W6HealthYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(NSID, lsi20 = W7HealthYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% 
    select(NSID, lsi25 = W8LOIL),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(NSID, lsi32 = W9LOIL)
)

# Merge all data
lsi_all <- reduce(long_term_illness_files, full_join, by = "NSID")

# recode value function
recode_lsi <- function(x) {
  case_when(
    x == 1 ~ 1, # having long-term illness
    x == 2 ~ 0, # not having long-term illness
    x %in% c(-92, -9) ~ -9,
    x == -1 ~ -1,
    x %in% c(-998, -997, -995, -99, -98, -97) ~ -2,
    x %in% c(-91, -8) ~ -8,
    TRUE ~ -3  # Not present/interviewed or NA
  )
}

lsi_all <- lsi_all %>%
  mutate(
    lsi14_15 = case_when(
      !is.na(lsi14) ~ recode_lsi(lsi14),
      !is.na(lsi15) ~ recode_lsi(lsi15),
      TRUE ~ -3
    ),
    lsi17 = recode_lsi(lsi17),
    lsi19 = recode_lsi(lsi19),
    lsi20 = recode_lsi(lsi20),
    lsi25 = recode_lsi(lsi25),
    lsi32 = recode_lsi(lsi32)
  ) %>%
  mutate(across(c(lsi14_15, lsi17, lsi19, lsi20, lsi25, lsi32), ~ factor(.x, 
                                                                    levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                                    labels = c("No",
                                                                               "Yes",
                                                                               "Item not applicable", 
                                                                               "Script error/information lost",
                                                                               "Not asked at the fieldwork stage/participated/interviewed",
                                                                               "Don’t know/insufficient information",
                                                                               "Refusal")))
  ) %>%
  select(NSID, lsi14_15, lsi17, lsi19, lsi20, lsi25, lsi32)

#### weight ####
# Load weight variables from each sweep
wt_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S12history)) %>% 
    select(NSID, wt0 = bwtkg),
  S4 = read_dta(file.path(data_path, sweeps$S4history)) %>%
    select(NSID, wt0 = W4bwtkgYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(NSID, wt25 = W8WEIGHT),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, wt32 = W9WEIGHT)
)

# Merge datasets
wt_all <- reduce(wt_vars, full_join, by = "NSID")

# Harmonise weight variables
wt_all <- wt_all %>%
  mutate(
    wt0 = coalesce(as.numeric(wt0.x), as.numeric(wt0.y))
  ) %>% 
  select(-wt0.x, -wt0.y)  %>%
  mutate(
    wt0 = case_when(
      wt0 > 0 ~ wt0,
      wt0 == -92 ~ -9,
      wt0 == -91 ~ -1,
      wt0 == -1 ~ -8,
      wt0 %in% c(-996, -99) ~ -3,
      is.na(wt0) ~ -3,
      TRUE ~ -2
    ),
    wt25 = case_when(
      wt25 > 0 ~ wt25,
      wt25 == -9 ~ -9,
      wt25 == -8 ~ -8,
      wt25 == -1 ~ -1,
      is.na(wt25) ~ -3,
      TRUE ~ -2
    ),
    wt32 = case_when(
      wt32 > 0 ~ wt32,
      wt32 == -9 ~ -9,
      wt32 == -8 ~ -8,
      wt32 == -1 ~ -1,
      is.na(wt32) ~ -3,
      TRUE ~ -2
    )
  ) %>%
  mutate(across(c(wt0, wt25, wt32), 
                ~ labelled(.x, 
                           labels = c("Item not applicable" = -1, 
                                      "Script error/information lost" = -2,
                                      "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                      "Don’t know/insufficient information" = -8,
                                      "Refusal" = -9)))) %>%
  select(NSID, wt0, wt25, wt32)

#### height ####
# Load height data from sweeps 8 and 9
ht_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(NSID, ht25 = W8HEIGHT),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, ht32 = W9HEIGHT)
)

# Merge datasets
ht_all <- reduce(ht_vars, full_join, by = "NSID")

# Recode height
ht_all <- ht_all %>%
  mutate(
    ht25 = case_when(
      ht25 > 0 ~ ht25,
      ht25 == -9 ~ -9,
      ht25 == -8 ~ -8,
      ht25 == -1 ~ -1,
      TRUE ~ -3
    ),
    ht32 = case_when(
      ht32 > 0 ~ ht32,
      ht32 == -9 ~ -9,
      ht32 == -8 ~ -8,
      ht32 == -1 ~ -1,
      TRUE ~ -3
    ),
    # combined height variable using most recent valid data
    ht25_32 = case_when(
      ht32 > 0 ~ ht32,
      ht25 > 0 ~ ht25,
      ht32 %in% c(-9, -8, -1) ~ ht32,
      ht25 %in% c(-9, -8, -1) ~ ht25, 
      TRUE ~ -3  
    )
  ) %>%
  mutate(across(c(ht25, ht32, ht25_32), 
                ~ labelled(.x, 
                           labels = c("Item not applicable" = -1, 
                                      "Script error/information lost" = -2,
                                      "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                      "Don’t know/insufficient information" = -8,
                                      "Refusal" = -9)))) %>%
  select(NSID, ht25, ht32, ht25_32)

#### BMI ####
# Load BMI data from relevant sweeps
bmi_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(NSID, bmi25 = W8DBMI),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, bmi32 = W9DBMI)
)

# Merge all BMI data by NSID
bmi_all <- reduce(bmi_vars, full_join, by = "NSID")

# Recode BMI variables
bmi_all <- bmi_all %>%
  mutate(
    bmi25 = case_when(
      bmi25 > 0 ~ bmi25,
      bmi25 == -9 ~ -9,
      bmi25 == -8 ~ -8,
      bmi25 == -1 ~ -1,
      TRUE ~ -3
    ),
    bmi32 = case_when(
      bmi32 > 0 ~ bmi32,
      bmi32 == -9 ~ -9,
      bmi32 == -8 ~ -8,
      bmi32 == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(c(bmi25, bmi32), 
                ~ labelled(.x, 
                           labels = c("Item not applicable" = -1, 
                                      "Script error/information lost" = -2,
                                      "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                      "Don’t know/insufficient information" = -8,
                                      "Refusal" = -9)))) %>%
  select(NSID, bmi25, bmi32)

#### smoke ####
# Load smoking data from relevant sweeps
smoking_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID, smknw14 = W1cignowYP, smk14 = W1cigfreqYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, smknw15 = W2cignowYP, smk15 = W2cigfreqYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>%
    select(NSID, smknw16 = W3cignowYP, smk16 = W3cigfreqYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(NSID, smk25 = W8SMOKING),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, smk32 = W9SMOKING)
)

# Merge all sweeps
smoking_all <- reduce(smoking_vars, full_join, by = "NSID")

# Recode smoke ever/frequency
recode_smk14_16 <- function(x) {
  case_when(
    x %in% c(1, 2, -91) ~ 0, # Never 
    x == 3 ~ 1, # used to, don’t at all now
    x %in% c(4, 5) ~ 2, #  smoke cigs occasionally – not every day 
    x == 6 ~ 3, # smoke cigs almost every day
    x %in% c(-99, -97, -96) ~ -2,
    x == -92 ~ -9,
    x == -1 ~ -8,
    TRUE ~ -3
  )
}

recode_smk25_32 <- function(x) {
  case_when(
    x > 0 ~ x - 1, # Convert 1-4 to 0-3
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ -3
  )
}

# Recode smoke now
recode_smknw14_16 <- function(x) {
  case_when(
    x == 1 ~ 1, # Yes
    x == 2 ~ 0, # No
    x %in% c(-99, -97, -96) ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ -3
  )
}

recode_smknw25_32 <- function(x) {
  case_when(
    x %in% c(0, 1) ~ 0,
    x %in% c(2, 3) ~ 1, 
    TRUE ~ x
  )
}

# Apply recoding
smoking_all <- smoking_all %>%
  mutate(
    smk14 = recode_smk14_16(smk14),
    smk15 = recode_smk14_16(smk15),
    smk16 = recode_smk14_16(smk16),
    smk25 = recode_smk25_32(smk25),
    smk32 = recode_smk25_32(smk32)
    )%>%
  mutate(
    smknw14 = case_when(
      smk14 == 0 ~ 0,
      smknw14 > 0 ~ recode_smknw14_16(smknw14),
      TRUE ~ recode_smknw14_16(smknw14)),
    smknw15 = case_when(
      smk15 == 0 ~ 0,
      smknw15 > 0 ~ recode_smknw14_16(smknw15),
      TRUE ~ recode_smknw14_16(smknw15)),
    smknw16 = case_when(
      smk16 == 0 ~ 0,
      smknw16 > 0 ~ recode_smknw14_16(smknw16),
      TRUE ~ recode_smknw14_16(smknw16)),
    smknw25 = recode_smknw25_32(smk25),
    smknw32 = recode_smknw25_32(smk32)
  ) %>%
  mutate(across(c(smk14, smk15, smk16, smk25, smk32), ~ factor(.x, 
                                                               levels = c(0, 1, 2, 3, -1, -2, -3, -8, -9), 
                                                               labels = c("Never",
                                                                          "Used to smoke, don’t at all now",
                                                                          "Smoke occasionally – not every day",
                                                                          "Smoke almost every day",
                                                                          "Item not applicable", 
                                                                          "Script error/information lost",
                                                                          "Not asked at the fieldwork stage/participated/interviewed",
                                                                          "Don’t know/insufficient information",
                                                                          "Refusal"))),
         across(c(smknw14, smknw15, smknw16, smknw25, smknw32), ~ factor(.x, 
                                                                    levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                                    labels = c("No",
                                                                               "Yes",
                                                                               "Item not applicable", 
                                                                               "Script error/information lost",
                                                                               "Not asked at the fieldwork stage/participated/interviewed",
                                                                               "Don’t know/insufficient information",
                                                                               "Refusal")))
  ) %>%
  select(NSID, smknw14, smknw15, smknw16, smknw25, smknw32,
         smk14, smk15, smk16, smk25, smk32)

#### alcohol ####
# Load and Select Variables
alc_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID, alcever_S1 = W1alceverYP, alcmon_S1 = W1alcmonYP, alcfreq_S1 = W1alcfreqYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(NSID, alcever_S2 = W2alceverYP, alcfreq_S2 = W2alcfreqYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% 
    select(NSID, alcever_S3 = W3alceverYP, alcfreq_S3 = W3alcfreqYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID, alcever_S4 = W4AlcEverYP, alcfreq_S4 = W4AlcFreqYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(NSID, alcever_S6 = W6AlcEverYP, alcfreq_S6 = W6AlcFreqYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(NSID, alcever_S7 = W7AlcEverYP, alcfreq_S7 = W7AlcFreqYP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>% 
    select(NSID, 
           audita25 = W8AUDIT1, auditb25 = W8AUDIT2, auditc25 = W8AUDIT6),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(NSID, 
           audita32 = W9AUDIT1, auditb32 = W9AUDIT2, auditc32 = W9AUDIT3)
)

# Merge all alcohol variables by NSID
alc_all <- reduce(alc_vars, full_join, by = "NSID")

# First Time Had Alcohol 
alc_all <- alc_all %>%
  rowwise() %>%
  mutate(
    ever_flags = list(c(
      ifelse(alcever_S1 == 1 & alcmon_S1 == 1, 14, NA),
      ifelse(alcever_S2 == 1, 15, NA),
      ifelse(alcever_S3 == 1, 16, NA),
      ifelse(alcever_S4 == 1, 17, NA),
      ifelse(alcever_S6 == 1, 19, NA),
      ifelse(alcever_S7 == 1, 20, NA),
      ifelse(audita25 > 1, 25, NA),
      ifelse(audita32 > 1, 32, NA)
    )),
    alcfst = case_when(
      any(ever_flags %in% 14:32, na.rm = TRUE) ~ min(unlist(ever_flags), na.rm = TRUE),
      all(c(alcever_S1, alcever_S2, alcever_S3, alcever_S4, alcever_S6, alcever_S7) == 2 &c(audita25, audita32) == 1, na.rm = TRUE) ~ 99,
      TRUE ~ -8
    )
  ) %>%
  ungroup()

# function - Frequency Recode Across Sweeps 
recode_freq <- function(x, sweep, ever) {
  case_when(
    sweep %in% c("S2", "S3", "S4") ~ case_when(
      x == 1 ~ 4, # most days
      x == 2 ~ 3, # once or twice a week
      x %in% c(3, 4) ~ 2, # once to three times a month 
      x == 5 ~ 1, # once every couple of month
      x == 6 ~ 0, # less often/not at all
      ever == 2 ~ 0, # less often/not at all
      x %in% c(-99, -97, -96) ~ -2,
      x == -92 ~ -9,
      x == -1 ~ -1,
      x == -91 ~ -1,
      TRUE ~ -3
    ),
    sweep %in% c("S6", "S7") ~ case_when(
      x %in% c(1, 2) ~ 4,
      x %in% c(3, 4) ~ 3,
      x == 5 ~ 2,
      x == 6 ~ 1,
      x %in% c(7, 8) ~ 0,
      ever == 2 ~ 0,
      x == -997 ~ -2,
      x == -97 ~ -9,
      x == -92 ~ -9,
      x == -91 ~ -1,
      x == -1 ~ -1,
      TRUE ~ -3
    )
  )
}

# recode frequency Variables 
alc_all <- alc_all %>%
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
      alcfreq_S1 %in% c(-99, -97, -96) ~ -2,
      alcfreq_S1 == -92 ~ -9,
      alcfreq_S1 == -1 ~ -1,
      alcfreq_S1 == -91 ~ -1,
      TRUE ~ -3
      ),
    alcfreq15 = recode_freq(alcfreq_S2, "S2", alcever_S2),
    alcfreq16 = recode_freq(alcfreq_S3, "S3", alcever_S3),
    alcfreq17 = recode_freq(alcfreq_S4, "S4", alcever_S4),
    alcfreq19 = recode_freq(alcfreq_S6, "S6", alcever_S6),
    alcfreq20 = recode_freq(alcfreq_S7, "S7", alcever_S7),
  )

# AUDIT Recode 
recode_audit <- function(x) {
  case_when(
    x >= 0 & x <= 5 ~ x,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ -3
  )
}

alc_all <- alc_all %>%
  mutate(
    audita25 = recode_audit(audita25), 
    audita32 = recode_audit(audita32))%>%
  mutate(
    auditb25 = case_when(
      audita25 == 1 ~ 0, 
      audita25 > 1 ~ recode_audit(auditb25), 
      TRUE ~ recode_audit(auditb25)),
    auditb32 = case_when(
      audita32 == 1 ~ 0,
      auditb32 > 1 ~ recode_audit(auditb32),
      TRUE ~ recode_audit(auditb32)),
    auditc25 = case_when(
      audita25 == 1 ~ 0,
      auditb25 == 1 ~ 0,
      auditc25 > 1 ~ recode_audit(auditc25),
      TRUE ~ recode_audit(auditc25)),
    auditc32 = case_when(
      audita32 == 1 ~ 0,
      auditb32 == 1 ~ 0,
      auditc32 > 1 ~recode_audit(auditc32),
      TRUE ~ recode_audit(auditc32))
  ) %>%
  mutate(alcfst = factor(alcfst, 
                           levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -1, -2, -3, -8, -9), 
                           labels = c("Age 14",
                                      "Age 15",
                                      "Age 16",
                                      "Age 17",
                                      "Age 19",
                                      "Age 20",
                                      "Age 25",
                                      "Age 32",
                                      "Never had alcohol",
                                      "Item not applicable", 
                                      "Script error/information lost",
                                      "Not asked at the fieldwork stage/participated/interviewed",
                                      "Don’t know/insufficient information",
                                      "Refusal")),
         across(c(alcfreq14, alcfreq15, alcfreq16, alcfreq17, alcfreq19, alcfreq20), ~ factor(.x, 
                                                                                             levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9), 
                                                                                             labels = c("Less often/not at all",
                                                                                                        "Once every couple of months",
                                                                                                        "Once to three times a month",
                                                                                                        "Once or twice a week",
                                                                                                        "Most days",
                                                                                                        "Item not applicable", 
                                                                                                        "Script error/information lost",
                                                                                                        "Not asked at the fieldwork stage/participated/interviewed",
                                                                                                        "Don’t know/insufficient information",
                                                                                                        "Refusal"))),
         across(c(audita25, audita32), ~ factor(.x, 
                                               levels = c(0, 1, 2, 3, 4,  -1, -2, -3, -8, -9), 
                                               labels = c("Never",
                                                          "Monthly or less",
                                                          "2–4 times a month",
                                                          "2–3 times a week",
                                                          "4 or more times a week",
                                                          "Item not applicable", 
                                                          "Script error/information lost",
                                                          "Not asked at the fieldwork stage/participated/interviewed",
                                                          "Don’t know/insufficient information",
                                                          "Refusal"))),
         across(c(auditb25, auditb32), ~ factor(.x, 
                                               levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
                                               labels = c("1–2 drinks",
                                                          "3–4 drinks",
                                                          "5–6 drinks",
                                                          "7–9 drinks",
                                                          "10+",
                                                          "Item not applicable", 
                                                          "Script error/information lost",
                                                          "Not asked at the fieldwork stage/participated/interviewed",
                                                          "Don’t know/insufficient information",
                                                          "Refusal"))),
         across(c(auditc25, auditc32), ~ factor(.x, 
                                               levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
                                               labels = c("Never",
                                                          "Less than monthly",
                                                          "Monthly",
                                                          "Weekly",
                                                          "Daily or almost daily",
                                                          "Item not applicable", 
                                                          "Script error/information lost",
                                                          "Not asked at the fieldwork stage/participated/interviewed",
                                                          "Don’t know/insufficient information",
                                                          "Refusal")))
  ) %>%
  select(NSID, alcfst, alcfreq14, alcfreq15, alcfreq16, alcfreq17, alcfreq19, alcfreq20,
         audita25, audita32, auditb25, auditb32, auditc25, auditc32)

#### drug use ####
# Load drug use data from relevant sweeps
drug_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID, canevr14 = W1canntryYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, canevr15 = W2canntryYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>%
    select(NSID, canevr16 = W3canntryYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID, canevr17 = W4CannTryYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(NSID, 
           canevr19 = W6DrugYP0a, 
           othevr19 = W6DrugYP0b, 
           now_cann19 = W6DrugOftenYP0a, 
           now_oth19 = W6DrugOftenYP0b),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(NSID,
           canevr20 = W7DrugYP1YP0a,
           othevr20 = W7DrugYP1YP0b,
           now_cann20 = W7DrugOftenYP0a,
           now_oth20 = starts_with("W7DrugOftenYP0")),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(NSID,
           canevr25 = W8DRUGYP10A,
           othevr25 = starts_with("W8DRUGYP10"),
           yr_cann25 = W8DRUGYP20A,
           yr_oth25 = starts_with("W8DRUGYP20"),
           now_cann25 = W8DRUGOFTEN0A,
           now_oth25 = starts_with("W8DRUGOFTEN0")),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID,
           canevr32 = W9DRUGYP10A,
           othevr32 = starts_with("W9DRUGYP1"),
           yr_cann32 = W9DRUGYP20A,
           yr_oth32 = starts_with("W9DRUGYP2"),
           now_cann32 = W9DRUGOFTEN0A,
           now_oth32 = starts_with("W9DRUGOFTEN0"))
)

# Merge all datasets
drug_all <- reduce(drug_vars, full_join, by = "NSID")

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
drug_all <- drug_all %>%
  mutate(across(c(canevr14, canevr15, canevr16, canevr17, canevr19, canevr20,
                  now_cann19, 
                  othevr19, othevr20,
                  now_oth19),
                recode_drugbin1_7),
         across(c(now_cann20, starts_with("now_oth20")), recode_drugoft7),
         across(c(canevr25, canevr32, yr_cann25, yr_cann32, 
                  now_cann25, now_cann32,
                  starts_with("yr_oth25"), starts_with("yr_oth32"),
                  starts_with("othevr25"), starts_with("othevr32"), 
                  starts_with("now_oth25"), starts_with("now_oth32")),
                recode_drugbin89))

# Derive oth7–9 and now_oth7–9 from multiple variables
drug_all <- drug_all %>%
  rowwise() %>%
  mutate(
    othevr25 = max(c_across(starts_with("othevr25"))[2:9], na.rm = TRUE),
    othevr32 = max(c_across(starts_with("othevr32"))[2:10], na.rm = TRUE),
    now_oth20 = max(c_across(starts_with("now_oth20"))[2:9], na.rm = TRUE),
    now_oth25 = max(c_across(starts_with("now_oth25"))[2:9], na.rm = TRUE),
    now_oth32 = max(c_across(starts_with("now_oth32"))[2:10], na.rm = TRUE),
    yr_oth25 = max(c_across(starts_with("yr_oth25"))[2:9], na.rm = TRUE),
    yr_oth32 = max(c_across(starts_with("yr_oth32"))[2:10], na.rm = TRUE)
  ) %>%
  ungroup()

# Derive: Ever used
drug_all <- drug_all %>%
  mutate(
    drgcnbevr = pmax(canevr14, canevr15, canevr16, canevr17, canevr19, canevr20, canevr25, canevr32, na.rm = FALSE),
    drgothevr = pmax(othevr19, othevr20, othevr25, othevr32, na.rm = FALSE)
  )

# Derive: First time use
first_wave_age <- c(14, 15, 16, 17, 19, 20, 25, 32)
cann_vars <- c("canevr14", "canevr15", "canevr16", "canevr17", "canevr19", "canevr20", "canevr25", "canevr32")
oth_vars  <- c("othevr19", "othevr20", "othevr25", "othevr32")

drug_all <- drug_all %>%
  mutate(
    # first time reported using cannabis (14-32)
    drgcnbfst = case_when(
      canevr14 == 1 ~ 14,
      canevr15 == 1 ~ 15,
      canevr16 == 1 ~ 16,
      canevr17 == 1 ~ 17,
      canevr19 == 1 ~ 19,
      canevr20 == 1 ~ 20,
      canevr25 == 1 ~ 25,
      canevr32 == 1 ~ 32,
      all(is.na(canevr14:canevr32)) ~ -3,
      rowSums(select(., canevr14:canevr32), na.rm = TRUE) == 0 ~ 99,
      TRUE ~ -2
    ),
    # first time reported using other drugs (19-32)
    drgothfst = case_when(
      othevr19 == 1 ~ 19,
      othevr20 == 1 ~ 20,
      othevr25 == 1 ~ 25,
      othevr32 == 1 ~ 32,
      all(is.na(c_across(c(othevr19, othevr20, othevr25, othevr32)))) ~ -3,
      rowSums(select(., othevr19, othevr20, othevr25, othevr32), na.rm = TRUE) == 0 ~ 99,
      TRUE ~ -2
    )
  )

# Derive: Current use
drug_all <- drug_all %>%
  mutate(
    drgcnbnw19 = case_when(
      canevr19 == 0 ~ 0,
      TRUE ~ now_cann19),
    drgcnbnw20 = case_when(
      canevr20 == 0 ~ 0,
      TRUE ~ now_cann20),
    drgcnbnw25 = case_when(
      canevr25 == 0 ~ 0,
      yr_cann25 == 0 ~ 0,
      TRUE ~ now_cann25),
    drgcnbnw32 = case_when(
      canevr32 == 0 ~ 0,
      yr_cann32 == 0 ~ 0,
      TRUE ~ now_cann32),
    
    drgothnw19 = case_when(
      othevr19 == 0 ~ 0,
      TRUE ~ now_oth19),
    drgothnw20 = case_when(
      othevr20 == 0 ~ 0,
      TRUE ~ now_oth20),
    drgothnw25 = case_when(
      othevr25 == 0 ~ 0,
      yr_oth25 == 0 ~ 0,
      TRUE ~ now_oth25),
    drgothnw32 = case_when(
      othevr32 == 0 ~ 0,
      yr_oth32 == 0 ~ 0,
      TRUE ~ now_oth32)
  )

# Final selection
drug_final <- drug_all %>%
  mutate(across(c(drgcnbevr,drgothevr, starts_with("drgcnbnw"),starts_with("drgothnw")), ~ factor(.x, 
                                                 levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                 labels = c("No",
                                                            "Yes",
                                                            "Item not applicable", 
                                                            "Script error/information lost",
                                                            "Not asked at the fieldwork stage/participated/interviewed",
                                                            "Don’t know/insufficient information",
                                                            "Refusal"))),
         across(c(drgcnbfst, drgothfst), ~ factor(.x, 
                                                 levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -1, -2, -3, -8, -9), 
                                                 labels = c("Age 14",
                                                            "Age 15",
                                                            "Age 16",
                                                            "Age 17",
                                                            "Age 19",
                                                            "Age 20",
                                                            "Age 25",
                                                            "Age 32",
                                                            "Never used",
                                                            "Item not applicable", 
                                                            "Script error/information lost",
                                                            "Not asked at the fieldwork stage/participated/interviewed",
                                                            "Don’t know/insufficient information",
                                                            "Refusal")))
  ) %>%
  select(NSID,
         drgcnbevr, drgcnbfst, starts_with("drgcnbnw"),
         drgothevr, drgothfst, starts_with("drgothnw"))

#### exercise ####
# Load relevant sweep files and select variables
exercise_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID, spt14 = W1sportYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, spt15 = W2sportYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID, spt17 = W4SportYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(NSID, spt19 = W6SportYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(NSID, spt20 = W7SportYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(NSID, spt25 = W8EXERCISE),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, spt32 = W9EXERCISEH)
)

# Merge all datasets
spt_all <- reduce(exercise_vars, full_join, by = "NSID")

# Recode function
recode_exercise <- function(x) {
  case_when(
    x %in% c(1, 2, 3) ~ x - 1,      # Keep as is
    x %in% c(4, 5, 6) ~ 3,     # 4- 6 = less than once a week/hardly ever/never
    x == -92 ~ -9,          # Refused
    x %in% c(-91) ~ -1,     # Not applicable / insufficient info
    x %in% c(-99) ~ -3,     # Not interviewed
    TRUE ~ -3              # Everything else = error/lost
  )
}

# Apply recoding
spt_all <- spt_all %>%
  mutate(
    spt14 = recode_exercise(spt14),
    spt15 = recode_exercise(spt15),
    spt17 = recode_exercise(spt17),
    spt19 = recode_exercise(spt19),
    spt20 = recode_exercise(spt20),  
    spt25 = case_when( # values from 0–7 days
      spt25 %in% c(5, 6, 7)  ~ 0,
      spt25 %in% c(2, 3, 4) ~ 1,
      spt25 == 1 ~ 2,
      spt25 == 0 ~ 3,
      spt25 == -9 ~ -9,
      spt25 == -8 ~ -8,
      spt25 == -1 ~ -1,
      TRUE ~ -3
    ),
    spt32 = case_when(
      spt32 %in% c(5, 6, 7)  ~ 0,
      spt32 %in% c(2, 3, 4) ~ 1,
      spt32 == 1 ~ 2,
      spt32  == 0 ~ 3,
      spt32 == -9 ~ -9,
      spt32 == -8 ~ -8,
      spt32 == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(c(starts_with("spt")), ~ factor(.x, 
                                                levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                labels = c("No",
                                                           "Yes",
                                                           "Item not applicable", 
                                                           "Script error/information lost",
                                                           "Not asked at the fieldwork stage/participated/interviewed",
                                                           "Don’t know/insufficient information",
                                                           "Refusal")))
  ) %>%
  select(NSID, spt14, spt15, spt17, spt19, spt20, spt25, spt32)

#### absence ####
# Load relevant sweep files and select variables
absence_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID, abs1m14 = W1abs1myMP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, abs1m15 = W2abs1myMP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>%
    select(NSID, abs1m16 = W3abs1myMP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID)
)

# Merge the datasets by NSID
absence_all <- reduce(absence_vars, full_join, by = "NSID")

# Recode function for harmonised values
recode_absence <- function(x) {
  case_when(
    x == 1 ~ 1, # yes
    x == 2 ~ 0, # no
    x %in% c(-97,-92) ~ -9,
    x %in% c(-91) ~ -1,
    x %in% c(-96, -1) ~ -8,
    x %in% c(-998, -997, -995, -99) ~ -2,
    is.na(x) ~ -3,
    TRUE ~ -3
  )
}

# Apply recode to each sweep
absence_all <- absence_all %>%
  mutate(
    abs1m14 = recode_absence(abs1m14),
    abs1m15 = recode_absence(abs1m15),
    abs1m16 = recode_absence(abs1m16)
  ) %>%
  mutate(across(starts_with("abs1m"), ~ factor(.x, 
                                                levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                labels = c("No",
                                                           "Yes",
                                                           "Item not applicable", 
                                                           "Script error/information lost",
                                                           "Not asked at the fieldwork stage/participated/interviewed",
                                                           "Don’t know/insufficient information",
                                                           "Refusal")))
  ) %>%
  select(NSID, abs1m14, abs1m15, abs1m16)

#### suspended/expelled ####
# Load suspension and expulsion variables from each sweep
suspend_expel_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID, susp14 = W1suspendMP, expl14 = W1expelMP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, susp15 = W2SuspendMP, expl15 = W2ExpelMP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>%
    select(NSID, susp16 = W3suspendMP, expl16 = W3expelMP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID, expl17 = W4Expel1YP, susp17 = W4Expel2YP)
)

# Merge all datasets by NSID
suspend_expel_all <- reduce(suspend_expel_vars, full_join, by = "NSID")

# Recode function 
recode_school_discipline <- function(x) {
  case_when(
    x == 1 ~ 1, # yes
    x == 2 ~ 0, # no
    x %in% c(-97, -92) ~ -9,
    x %in% c(-91) ~ -1,
    x %in% c(-96, -1) ~ -8,
    x %in% c(-99) ~ -3,
    is.na(x) ~ -3,
    TRUE ~ -3
  )
}

# Apply recoding
suspend_expel_all <- suspend_expel_all %>%
  mutate(
    susp14 = recode_school_discipline(susp14),
    susp15 = recode_school_discipline(susp15),
    susp16 = recode_school_discipline(susp16),
    susp17 = recode_school_discipline(susp17),
    expl14 = recode_school_discipline(expl14),
    expl15 = recode_school_discipline(expl15),
    expl16 = recode_school_discipline(expl16),
    expl17 = recode_school_discipline(expl17)
  ) %>%
  mutate(across(c(starts_with("abs1m"), starts_with("expl")), ~ factor(.x, 
                                                  levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                  labels = c("No",
                                                             "Yes",
                                                             "Item not applicable", 
                                                             "Script error/information lost",
                                                             "Not asked at the fieldwork stage/participated/interviewed",
                                                             "Don’t know/insufficient information",
                                                             "Refusal")))
  ) %>%
  select(NSID, starts_with("susp"), starts_with("expl"))

#### truancy ####
# Load original variables from S1–S4
truancy_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID, trua14_ever = W1truantYP, trua14_type = W1truant1YP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, trua15_ever = W2truantYP, trua15_type = W2truant1YP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>%
    select(NSID, trua16_ever = W3truantYP, trua16_type = W3truant1YP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
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
    type %in% c(-96, -1) ~ -8,
    type %in% c(-97, -92) ~ -9,
    type %in% c(-99) ~ -3,
    type %in% c(-91) ~ -1,
    is.na(type) & ever == 1 ~ -2,
    is.na(type) & is.na(ever) ~ -3,
    TRUE ~ -3
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
    x %in% c(-96, -1) ~ -8,
    x %in% c(-97, -92) ~ -9,
    x == -99 ~ -3,
    x == -91 ~ -1,
    is.na(x) ~ -3,
    TRUE ~ -3
  )
}

# Apply recoding
truancy_all <- truancy_all %>%
  mutate(
    trua14 = recode_truancy_early(trua14_ever, trua14_type),
    trua15 = recode_truancy_early(trua15_ever, trua15_type),
    trua16 = recode_truancy_early(trua16_ever, trua16_type),
    trua17 = recode_truancy_s4(trua17_raw)
  ) %>%
  mutate(across(starts_with("trua"), ~ factor(.x, 
                                              levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9), 
                                              labels = c("Never played truant",
                                                         "For weeks at a time",
                                                         "Several days at a time",
                                                         "Particular days or lessons",
                                                         "Odd day or lesson",
                                                         "Item not applicable", 
                                                         "Script error/information lost",
                                                         "Not asked at the fieldwork stage/participated/interviewed",
                                                         "Don’t know/insufficient information",
                                                         "Refusal")))
  ) %>%
  select(NSID, trua14, trua15, trua16, trua17)

#### police contact ####
# Load data for police contact
police_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID, pol14 = W1Police1MP, polcnt14 = W1police2MP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(NSID, pol15 = W2police1MP, polcnt15 = W2Police2MP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>%
    select(NSID, pol16 = W3police1MP, polcnt16 = W3police2MP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID, pol17 = W4Police1MP, polcnt17 = W4Police2MP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(NSID, 
           polwrn25 = W8CJSCONTACT0A, 
           polars25 = W8CJSCONTACT0B,
           polcau25 = W8CJSCONTACT0C, 
           polglt25 = W8CJSCONTACT0D, 
           polpnd25 = W8CJSCONTACT0E),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(NSID, 
           polwrn32 = W9CJSCONTACT0A, 
           polars32 = W9CJSCONTACT0B,
           polcau32 = W9CJSCONTACT0C, 
           polglt32 = W9CJSCONTACT0D, 
           polpnd32 = W9CJSCONTACT0E)
)

# Merge datasets
police_all <- reduce(police_vars, full_join, by = "NSID")

# Recode function for binary variables (pol15,16)
recode_pol <- function(x) {
  case_when(
    x == 1 ~ 1, 
    x == 2 ~ 0,
    x %in% c(-97, -92) ~ -9,
    x ==  -91 ~ -1,
    x %in% c(-96, -1) ~ -8,
    x %in% c(-99) ~ -3
  )
}

# Recode function for contact counts
recode_cnt <- function(x, ever) {
  case_when(
    ever %in% c(2,3) ~ 0,
    x >= 0 ~ x, 
    x %in% c(-97, -92) ~ -9,
    x ==  -91 ~ -1,
    x %in% c(-96, -1) ~ -8,
    x %in% c(-99) ~ -3
  )
}

# Apply recoding
police_all <- police_all %>%
  mutate(
    polcnt14 = recode_cnt(polcnt14, pol14),
    polcnt15 = recode_cnt(polcnt15, pol15),
    polcnt16 = recode_cnt(polcnt16, pol16),
    polcnt17 = recode_cnt(polcnt17, pol17),
    across(starts_with("polwrn"), 
           ~ case_when(
             .x == 1 ~ 1, 
             .x == 2 ~ 0, 
             .x < 0 ~ .x, 
             TRUE ~ -3)),
    across(starts_with("polars"), 
           ~ case_when(
             .x == 1 ~ 1, 
             .x == 2 ~ 0, 
             .x < 0 ~ .x, 
             TRUE ~ -3)),
    across(starts_with("polcau"),  
           ~ case_when(
             .x == 1 ~ 1, 
             .x == 2 ~ 0, 
             .x < 0 ~ .x, 
             TRUE ~ -3)),
    across(starts_with("polglt"), 
           ~ case_when(
             .x == 1 ~ 1, 
             .x == 2 ~ 0, 
             .x < 0 ~ .x, 
             TRUE ~ -3)),
    across(starts_with("polpnd"), 
           ~ case_when(
             .x == 1 ~ 1, 
             .x == 2 ~ 0, 
             .x < 0 ~ .x, 
             TRUE ~ -3))
  )%>%
  mutate(
    pol14 = case_when(
      pol14 %in% c(1, 3) ~ 1,
      pol14 == 2 ~ 0,
      pol14 %in% c(-97, -92) ~ -9,
      pol14 ==  -91 ~ -1,
      pol14 %in% c(-96, -1) ~ -8,
      pol14 %in% c(-99) ~ -3,
      TRUE ~ -3),
    pol15 = recode_pol(pol15),
    pol16 = recode_pol(pol16),
    pol17 = case_when(
      pol17 %in% c(1, 3) ~ 1,
      pol17 == 2 ~ 0,
      pol17 %in% c(-97, -92) ~ -9,
      pol17 ==  -91 ~ -1,
      pol17 %in% c(-96, -1) ~ -8,
      pol17 %in% c(-99) ~ -3,
      TRUE ~ -3)) %>%
  mutate(across(c(starts_with("polwrn"), starts_with("polars"),
                  starts_with("polcau"), starts_with("polglt"),
                  starts_with("polpnd")), ~ factor(.x, 
                                                  levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                  labels = c("No",
                                                             "Yes",
                                                             "Item not applicable", 
                                                             "Script error/information lost",
                                                             "Not asked at the fieldwork stage/participated/interviewed",
                                                             "Don’t know/insufficient information",
                                                             "Refusal"))),
         across(c(pol14, pol15, pol16, pol17), ~ factor(.x, 
                                                     levels = c(0, 1, -1, -2, -3, -8, -9), 
                                                     labels = c("No",
                                                                "Yes/not in last 3 years",
                                                                "Item not applicable", 
                                                                "Script error/information lost",
                                                                "Not asked at the fieldwork stage/participated/interviewed",
                                                                "Don’t know/insufficient information",
                                                                "Refusal"))),
         across(c(starts_with("polcnt")), ~ labelled(.x, 
                                               labels = c("Item not applicable" = -1, 
                                                          "Script error/information lost" = -2,
                                                          "Not asked at the fieldwork stage/participated/interviewed" = -3, 
                                                          "Don’t know/insufficient information" = -8,
                                                          "Refusal" = -9)))
  ) %>%
  select(NSID, pol14, pol15, pol16, pol17,
         polcnt14, polcnt15, polcnt16, polcnt17,
         starts_with("polwrn"), starts_with("polars"),
         starts_with("polcau"), starts_with("polglt"),
         starts_with("polpnd"))

#### bully ####
# Load and harmonise bullying variables across sweeps 1–4, 7–8
bully_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(NSID, bul14 = W1bulrc),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(NSID, bul15 = W2bulrc),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% 
    select(NSID, bul16 = W3bulrc),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(NSID, 
           bul17_1 = W4V1perSYP, 
           bul17_2 = W4ViolentYP, 
           bul17_3 = W4HurtYP,
           bul17_4 = W4ThreatsYP, 
           bul17_5 = W4MadegiveYP, 
           bul17_6 = W4NamesYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(NSID, 
           starts_with("W7BullyTypeYP0")),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>% 
    select(NSID, 
           starts_with("W8BULLYTYPE0"))
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
      rowSums(across(c("bul17_1","bul17_2","bul17_4","bul17_5","bul17_6")) == 2, na.rm = TRUE) == 5 ~ 0,
      rowSums(across(c("bul17_1","bul17_2","bul17_4","bul17_5","bul17_6")) < 0, na.rm = TRUE) > 0 ~ -8,
      rowSums(is.na(across(starts_with("bul17_")))) == 6 ~ -3,
      TRUE ~ -2
    ),
    bul20 = case_when(
      rowSums(across(starts_with("W7BullyTypeYP0")) > 0 &  across(starts_with("W7BullyTypeYP0")) < 8, na.rm = TRUE) > 0 ~ 1,
      rowSums(across(starts_with("W7BullyTypeYP0")) == 8, na.rm = TRUE) == 6 ~ 0,
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
  )%>%
  mutate(across(starts_with("bul"), ~ factor(.x, 
                                             levels = c(0, 1, -1, -2, -3, -8, -9), 
                                             labels = c("No",
                                                        "Yes",
                                                        "Item not applicable", 
                                                        "Script error/information lost",
                                                        "Not asked at the fieldwork stage/participated/interviewed",
                                                        "Don’t know/insufficient information",
                                                        "Refusal")))
  ) %>%
  select(NSID, bul14, bul15, bul16, bul17, bul20, bul25)

#### longitudinal dataset ####
# prepare for the longitudinal dataset by merging all derived variables
long_vars <- read_dta(file.path(data_path, sweeps$longitudinal)) %>% 
    select(NSID, SAMPPSU, SAMPSTRATUM, DESIGNWEIGHT,        
           MAINBOOST, W1OUTCOME, W1FINWT,
           W2OUTCOME, W2FINWT, 
           W3OUTCOME, W3FINWT, W2TOW3NRWT,
           W4OUTCOME, W4WEIGHT_MAIN_BOOST, W4WEIGHT_MAIN, 
           W5OUTCOME, W5FINWT_CROSS, 
           W6OUTCOME, W6FINWT_CROSS, W6LSYPEWT, W6LSYPEW4WT,
           W7OUTCOME, W7_LSYPE_WT, W7FINWT, W7_LSYPE_WT_SKIPONLY, 
           W8OUTCOME, W8FINWT, 
           W9OUTCOME, W9FINWT, DATA_AVAILABILITY) %>%
  mutate(across(c(W1OUTCOME, W2OUTCOME, W3OUTCOME,
                  W4OUTCOME, W5OUTCOME, W6OUTCOME, W7OUTCOME,
                  W8OUTCOME, W9OUTCOME), 
                ~ factor(.x,
                         levels = c(1, 2, 3, 4, 5, 6, -1),
                         labels = c("Productive", 
                                    "Refusal",
                                    "Non-contact and other unproductive", 
                                    "Ineligible",
                                    "Untraced", 
                                    "Not issued",
                                    "No contact"))),
         DATA_AVAILABILITY = factor(DATA_AVAILABILITY,
                                    levels = c(0, 1),
                                    labels = c("Not available",
                                               "Available for research")),
         MAINBOOST = factor(MAINBOOST,
                            levels = c(1, 2),
                            labels = c("Main",
                                       "Boost")))


#### merge all datasets ####
# Merge all derived variables into a single dataset
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
  alc_all,
  drug_final,
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
  educma = "Highest education level mother 3-category (CM age 17-14y)",
  educpa = "Highest education level father 3-category (CM age 17-14y)",
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
  MAINBOOST      = "Whether main or boost sample",
  DATA_AVAILABILITY = "Whether the data is available for research use",
  W1OUTCOME      = "Fieldwork outcome at Wave 1",
  W2OUTCOME      = "Fieldwork outcome at Wave 2",
  W3OUTCOME      = "Fieldwork outcome at Wave 3",
  W4OUTCOME      = "Fieldwork outcome at Wave 4",
  W5OUTCOME      = "Fieldwork outcome at Wave 5",
  W6OUTCOME      = "Fieldwork outcome at Wave 6",
  W7OUTCOME      = "Fieldwork outcome at Wave 7",
  W8OUTCOME      = "Fieldwork outcome at Wave 8",
  W9OUTCOME      = "Fieldwork outcome at Wave 9"
)

write_dta(derived_all, "derived_variables.dta")
