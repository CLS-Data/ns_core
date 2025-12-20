# Prerequisite: 00-load-raw-data.R (this script also sources helpers.R)
#
# If you are running this script on its own, please run the following first
# from the project root:
#
# source(here::here("R", "00-load-raw-data.R"))
#
# or manually run 00-load-raw-data.R before this script.

# GHQ --------------------------------------------------------------------
# Load GHQ-12 derived score and item-level data
ghq_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S2 = ns_data[["S2youngperson"]] %>%
    select(
      NSID,
      ghq15_raw = W2ghq12scr,
      paste0(
        "W2",
        c(
          "concenYP",
          "nosleepYP",
          "usefulYP",
          "decideYP",
          "strainYP",
          "difficYP",
          "activYP",
          "probsYP",
          "depressYP",
          "noconfYP",
          "wthlessYP",
          "happyYP"
        )
      )
    ),
  S4 = ns_data[["S4youngperson"]] %>%
    select(
      NSID,
      ghq17_raw = W4ghq12scr,
      paste0(
        "W4",
        c(
          "ConcenYP",
          "NoSleepYP",
          "UsefulYP",
          "DecideYP",
          "StrainYP",
          "DifficYP",
          "ActivYP",
          "ProbsYP",
          "DepressYP",
          "NoConfYP",
          "WthlessYP",
          "HappyYP"
        )
      )
    ),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, starts_with("W8GHQ12_")),
  S8_derive = ns_data[["S8derivedvariable"]] %>%
    select(NSID, ghq25_raw = W8DGHQSC),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, starts_with("W9GHQ12_")),
  S9_derive = ns_data[["S9derivedvariable"]] %>%
    select(NSID, ghq32_raw = W9DGHQSC)
)

# Merge all sweeps by NSID
ghq_all <- reduce(ghq_vars, full_join, by = "NSID")

# Define item lists for sum scores
ghq_items <- list(
  ghqtl15 = paste0(
    "W2",
    c(
      "concenYP",
      "nosleepYP",
      "usefulYP",
      "decideYP",
      "strainYP",
      "difficYP",
      "activYP",
      "probsYP",
      "depressYP",
      "noconfYP",
      "wthlessYP",
      "happyYP"
    )
  ),
  ghqtl17 = paste0(
    "W4",
    c(
      "ConcenYP",
      "NoSleepYP",
      "UsefulYP",
      "DecideYP",
      "StrainYP",
      "DifficYP",
      "ActivYP",
      "ProbsYP",
      "DepressYP",
      "NoConfYP",
      "WthlessYP",
      "HappyYP"
    )
  ),
  ghqtl25 = paste0("W8GHQ12_", 1:12),
  ghqtl32 = paste0("W9GHQ12_", 1:12)
)

# Derive GHQ sum scores (0–12) with custom missing logic
ghq_rec <- ghq_all %>%
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
      is.na(ghq15_raw) ~ -3,
      ghq15_raw %in% c(-96, -99) ~ -3,
      ghq15_raw %in% c(-97, -92) ~ -9,
      TRUE ~ ghq15_raw
    ),
    ghq17 = case_when(
      is.na(ghq17_raw) ~ -3,
      ghq17_raw %in% c(-96, -99) ~ -3,
      ghq17_raw %in% c(-97, -92) ~ -9,
      TRUE ~ ghq17_raw
    ),
    ghq25 = if_else(is.na(ghq25_raw), -3, ghq25_raw),
    ghq32 = if_else(is.na(ghq32_raw), -3, ghq32_raw)
  ) %>%
  mutate(
    across(
      c(ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32),
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

# Cross-checks for 12-item score
ghq_rec %>%
  count(ghq15_raw, ghq15)

ghq_rec %>%
  count(ghq17_raw, ghq17)

ghq_rec %>%
  count(ghq25_raw, ghq25)

ghq_rec %>%
  count(ghq32_raw, ghq32)

ghq_all <- ghq_rec %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Life Satisfaction --------------------------------------------------------------------
# Load life satisfaction variables from each sweep
lsat_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S4 = ns_data[["S4youngperson"]] %>% select(NSID),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, lsat20_raw = W7OSatisYP),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, lsat25_raw = W8OSATIS),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, lsat32_raw = W9OSATIS)
)

# Merge into a single dataset
lsat_all <- reduce(lsat_vars, full_join, by = "NSID")

# Harmonise life satisfaction variables
lsat_rec <- lsat_all %>%
  mutate(
    lsat20 = case_when(
      lsat20_raw %in% 1:5 ~ lsat20_raw,
      lsat20_raw %in% c(-97, -92) ~ -9,
      lsat20_raw == -91 ~ -1,
      lsat20_raw == -1 ~ -8,
      is.na(lsat20_raw) ~ -3,
      TRUE ~ -2
    ),
    lsat25 = case_when(
      lsat25_raw %in% 1:5 ~ lsat25_raw,
      lsat25_raw == -9 ~ -9,
      lsat25_raw == -8 ~ -8,
      lsat25_raw == -1 ~ -1,
      is.na(lsat25_raw) ~ -3,
      TRUE ~ -2
    ),
    lsat32 = case_when(
      lsat32_raw %in% 1:5 ~ lsat32_raw,
      lsat32_raw == -9 ~ -9,
      lsat32_raw == -8 ~ -8,
      lsat32_raw == -1 ~ -1,
      is.na(lsat32_raw) ~ -3,
      TRUE ~ -2
    )
  ) %>%
  mutate(
    across(
      c(lsat20, lsat25, lsat32),
      ~ labelled(
        .x,
        labels = c(
          "Very satisfied" = 1,
          "Fairly satisfied" = 2,
          "Neither satisfied nor dissatisfied" = 3,
          "Fairly dissatisfied" = 4,
          "Very dissatisfied" = 5,
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    )
  )

lsat_rec %>%
  count(lsat20_raw, lsat20)

lsat_rec %>%
  count(lsat25_raw, lsat25)

lsat_rec %>%
  count(lsat32_raw, lsat32)

# Extract derived variables
lsat_all <- lsat_rec %>%
  select(NSID, lsat20, lsat25, lsat32)

# Self-Harm --------------------------------------------------------------------
# Load S8 self-harm variables
sharm_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, sharm25_ever = W8HARM, sharm25_freq = W8HARM2)
)

# Merge by NSID
sharm_all <- reduce(sharm_vars, full_join, by = "NSID")

# Recode to harmonised sharm25
sharm_rec <- sharm_all %>%
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
      # Ever self-harmed takes precedence for missing values
      # e.g. if refused to answer sharm25_ever, sharm25_freq will always be 'Item not applicable'
      sharm25_ever %in% c(-1, -8, -9) ~ sharm25_ever,
      sharm25_freq == -9 ~ -9,
      sharm25_freq == -8 ~ -8,
      sharm25_freq == -1 ~ -1,
      TRUE ~ -2
    )
  ) %>%
  mutate(
    sharm25 = labelled(
      sharm25,
      labels = c(
        "Never" = 0,
        "Less often" = 1,
        "Once a year" = 2,
        "Several times a year" = 3,
        "Once a month" = 4,
        "Two or more times a month" = 5,
        "Once a week" = 6,
        "Several times a week" = 7,
        "Once a day" = 8,
        "More than once a day" = 9,
        "Item not applicable" = -1,
        "Script error/information lost" = -2,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  )

sharm_rec %>%
  count(sharm25_ever, sharm25_freq, sharm25)

sharm_all <- sharm_rec %>%
  select(NSID, sharm25)

# GAD --------------------------------------------------------------------
# Load GAD-7 derived score and item-level data
gad_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, gad32_raw = W9DGAD2)
)

# Merge by NSID
gad_all <- reduce(gad_vars, full_join, by = "NSID")

# Recode to harmonised gad32
gad_rec <- gad_all %>%
  mutate(
    gad32 = case_when(
      !is.na(gad32_raw) ~ gad32_raw,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    gad32 = labelled(
      gad32,
      labels = c(
        "Item not applicable" = -1,
        "Script error/information lost" = -2,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  )

gad_rec %>%
  count(gad32_raw, gad32)

gad_all <- gad_rec %>%
  select(NSID, gad32)

# PHQ --------------------------------------------------------------------
# Load GAD-7 derived score and item-level data
phq_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, phq32_raw = W9DPHQ2)
)

# Merge by NSID
phq_all <- reduce(phq_vars, full_join, by = "NSID")

# Recode to harmonised phq32
phq_rec <- phq_all %>%
  mutate(
    phq32 = case_when(
      !is.na(phq32_raw) ~ phq32_raw,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    phq32 = labelled(
      phq32,
      labels = c(
        "Item not applicable" = -1,
        "Script error/information lost" = -2,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  )

phq_rec %>%
  count(phq32_raw, phq32)

phq_all <- phq_rec %>%
  select(NSID, phq32)

# UCLA Loneliness --------------------------------------------------------------------
# Load GAD-7 derived score and item-level data
lon_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, lon32_raw = W9DLONELINESS)
)

# Merge by NSID
lon_all <- reduce(lon_vars, full_join, by = "NSID")

# Recode to harmonised lon32
lon_rec <- lon_all %>%
  mutate(
    lon32 = case_when(
      !is.na(lon32_raw) ~ lon32_raw,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    lon32 = labelled(
      lon32,
      labels = c(
        "Item not applicable" = -1,
        "Script error/information lost" = -2,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  )

lon_rec %>%
  count(lon32_raw, lon32)

lon_all <- lon_rec %>%
  select(NSID, lon32)

# Self-Rated General Health --------------------------------------------------------------------
# Load relevant sweep files and select needed variables
health_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, ghea15_raw = W2hea1cYP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, ghea16_raw = W3hea1cYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, ghea17_raw = W4Hea1CYP),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, ghea25_raw = W8GENA),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, ghea32_raw = W9HLTHGEN)
)

# Merge all data by NSID
health_all <- reduce(health_vars, full_join, by = "NSID")

# Helpers
## Harmonise adolescent responses (S2–S4): gheateenXX
## keep specifies which values are kept, adjust optionally shifts those values by some fixed amount (e.g. -2)
recode_gheateen <- function(x, keep = 1:4, adjust = 0) {
  dplyr::case_when(
    x %in% keep ~ as.integer(x + adjust),
    x %in% c(-998, -997, -995, -94) ~ -2L,
    x %in% c(-99, -96) ~ -3L,
    x %in% c(-96, -1) ~ -8L,
    x %in% c(-92, -97) ~ -9L,
    x == -91 ~ -1L,
    .default = -3L
  )
}

health_rec <- health_all %>%
  mutate(
    gheateen15 = recode_gheateen(ghea15_raw, keep = 3:6, adjust = -2),
    gheateen16 = recode_gheateen(ghea16_raw, keep = 1:4, adjust = 0),
    gheateen17 = recode_gheateen(ghea17_raw, keep = 1:4, adjust = 0)
  ) %>%
  mutate(
    # Harmonise adult responses (S8–S9): gheaaduXX
    gheaadu25 = case_when(
      ghea25_raw %in% c(1, 2, 3, 4, 5) ~ ghea25_raw,
      ghea25_raw == -8 ~ -8,
      ghea25_raw == -1 ~ -1,
      ghea25_raw == -9 ~ -9,
      TRUE ~ -3
    ),
    gheaadu32 = case_when(
      ghea32_raw %in% c(1, 2, 3, 4, 5) ~ ghea32_raw,
      ghea32_raw == -8 ~ -8,
      ghea32_raw == -1 ~ -1,
      ghea32_raw == -9 ~ -9,
      TRUE ~ -3
    )
  )

# Binary general health: 1 = poor/fair, 0 = good/excellent
health_rec <- health_rec %>%
  mutate(
    ghea15 = case_when(
      gheateen15 %in% c(3, 4) ~ 1,
      gheateen15 %in% c(1, 2) ~ 0,
      TRUE ~ gheateen15
    ),
    ghea16 = case_when(
      gheateen16 %in% c(3, 4) ~ 1,
      gheateen16 %in% c(1, 2) ~ 0,
      TRUE ~ gheateen16
    ),
    ghea17 = case_when(
      gheateen17 %in% c(3, 4) ~ 1,
      gheateen17 %in% c(1, 2) ~ 0,
      TRUE ~ gheateen17
    ),
    ghea25 = case_when(
      gheaadu25 %in% c(4, 5) ~ 1,
      gheaadu25 %in% c(1, 2, 3) ~ 0,
      TRUE ~ gheaadu25
    ),
    ghea32 = case_when(
      gheaadu32 %in% c(4, 5) ~ 1,
      gheaadu32 %in% c(1, 2, 3) ~ 0,
      TRUE ~ gheaadu32
    )
  ) %>%
  mutate(
    across(
      c(gheateen15, gheateen16, gheateen17),
      ~ labelled(
        .x,
        labels = c(
          "Very good" = 1,
          "Fairly good" = 2,
          "Not very good" = 3,
          "Not good at all" = 4,
          common_missing_labels
        )
      )
    ),
    across(
      c(gheaadu25, gheaadu32),
      ~ labelled(
        .x,
        labels = c(
          "Excellent" = 1,
          "Very good" = 2,
          "Good" = 3,
          "Fair" = 4,
          "Poor" = 5,
          common_missing_labels
        )
      )
    ),
    across(
      c(ghea15, ghea16, ghea17, ghea25, ghea32),
      ~ labelled(
        .x,
        labels = c(
          "Good/fairly good/very good/excellent" = 0,
          "Poor/not good at all/not very good/fair" = 1,
          common_missing_labels
        )
      )
    )
  )

# Cross-checks
health_rec %>%
  count(ghea15_raw, gheateen15, ghea15)

health_rec %>%
  count(ghea16_raw, gheateen16, ghea16)

health_rec %>%
  count(ghea17_raw, gheateen17, ghea17)

health_rec %>%
  count(ghea25_raw, ghea25)

health_rec %>%
  count(ghea32_raw, ghea32)

health_all <- health_rec %>%
  select(
    NSID,
    gheateen15,
    gheateen16,
    gheateen17,
    gheaadu25,
    gheaadu32,
    ghea15,
    ghea16,
    ghea17,
    ghea25,
    ghea32
  )

# Long-Term Illness --------------------------------------------------------------------
# Load relevant sweep files and select needed variables
long_term_illness_files <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, lsi14 = W1chea1HS),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, lsi15 = W2chea1HS),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, lsi17 = W4Hea2YP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, lsi19 = W6HealthYP),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, lsi20 = W7HealthYP),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, lsi25 = W8LOIL),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, lsi32 = W9LOIL)
)

# Merge all data
lsi_all <- reduce(long_term_illness_files, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'lsi*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("lsi")
  )

# Recode value function
recode_lsi <- function(x) {
  case_when(
    x == 1 ~ 1, # having long-term illness
    x == 2 ~ 0, # not having long-term illness
    x %in% c(-92, -9) ~ -9,
    x %in% c(-998, -997, -97) ~ -2,
    x == -91 ~ -1,
    x %in% c(-8, -1) ~ -8,
    x %in% c(-99, -98, -995) ~ -3, # history respondent not interviewed or not identified, missing history data unexplained
    TRUE ~ -3 # Not present/interviewed or NA
  )
}

# In S8 and S9, -1 stands for Not applicable -> keep as -1.
recode_lsi_s8_s9 <- function(x) {
  case_when(
    x == 1 ~ 1, # having long-term illness
    x == 2 ~ 0, # not having long-term illness
    x %in% c(-92, -9) ~ -9,
    x %in% c(-998, -997, -97) ~ -2,
    x %in% c(-91, -1) ~ -1,
    x == -8 ~ -8,
    x %in% c(-99, -98, -995) ~ -3, # history respondent not interviewed or not identified, missing history data unexplained
    TRUE ~ -3 # Not present/interviewed or NA
  )
}

lsi_rec <- lsi_all %>%
  mutate(
    lsi14 = recode_lsi(lsi14_raw),
    lsi15 = recode_lsi(lsi15_raw),
    lsi14_15 = case_when(
      # If age 14 valid, take response from there:
      lsi14 %in% c(0, 1) ~ lsi14,
      # If age 14 not valid, see if age 15 valid and take response from there:
      lsi15 %in% c(0, 1) ~ lsi15,
      # If both invalid, use a combination approach:
      lsi14 == -9 | lsi15 == -9 ~ -9, # Refusal if either is refusal, otherwise:
      lsi14 == -8 | lsi15 == -8 ~ -8, # Either wave has dk/insufficient info, otherwise:
      lsi14 == -2 | lsi15 == -2 ~ -2, # Either wave has script error/information loss, otherwise:
      lsi14 == -1 | lsi15 == -1 ~ -1, # Either has item not applicable, otherwise:
      .default = -3 # Everything else is Not asked/not interviewed/did not participate/was not surveyed
    ),
    lsi17 = recode_lsi(lsi17_raw),
    lsi19 = recode_lsi(lsi19_raw),
    lsi20 = recode_lsi(lsi20_raw),
    lsi25 = recode_lsi_s8_s9(lsi25_raw),
    lsi32 = recode_lsi_s8_s9(lsi32_raw)
  ) %>%
  mutate(
    across(
      c(lsi14_15, lsi17, lsi19, lsi20, lsi25, lsi32),
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
lsi_rec %>%
  count(lsi14_raw, lsi15_raw, lsi14_15) %>%
  print(n = Inf)

lsi_rec %>%
  count(lsi17_raw, lsi17)

lsi_rec %>%
  count(lsi19_raw, lsi19)

lsi_rec %>%
  count(lsi20_raw, lsi20)

lsi_rec %>%
  count(lsi25_raw, lsi25)

lsi_rec %>%
  count(lsi32_raw, lsi32)

lsi_all <- lsi_rec %>%
  select(NSID, lsi14_15, lsi17, lsi19, lsi20, lsi25, lsi32)
