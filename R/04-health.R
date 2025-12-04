# GHQ --------------------------------------------------------------------
# Load GHQ-12 derived score and item-level data
ghq_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(
      NSID,
      ghq15 = W2ghq12scr,
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
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(
      NSID,
      ghq17 = W4ghq12scr,
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
  ) %>%
  mutate(across(
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
  )) %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)


# Life Satisfaction --------------------------------------------------------------------
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
  mutate(across(
    c(lsat20, lsat25, lsat32),
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
  )) %>%
  select(NSID, lsat20, lsat25, lsat32)

# Self-Harm --------------------------------------------------------------------
# Load S8 self-harm variables
sharm_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(NSID, sharm25_ever = W8HARM, sharm25_freq = W8HARM2)
)

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
  mutate(
    sharm25 = labelled(
      sharm25,
      labels = c(
        "Item not applicable" = -1,
        "Script error/information lost" = -2,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  ) %>%
  select(NSID, sharm25)

# GAD --------------------------------------------------------------------
# Load GAD-7 derived score and item-level data
gad_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, gad32 = W9DGAD2)
)

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
  ) %>%
  select(NSID, gad32)

# PHQ --------------------------------------------------------------------
# Load GAD-7 derived score and item-level data
phq_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, phq32 = W9DPHQ2)
)

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
  ) %>%
  select(NSID, phq32)

# UCLA Loneliness --------------------------------------------------------------------
# Load GAD-7 derived score and item-level data
lon_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(NSID),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(NSID, lon32 = W9DLONELINESS)
)

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
  ) %>%
  select(NSID, lon32)


# Self-Rated General Health --------------------------------------------------------------------
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
  ) %>%
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
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, -1, -2, -3, -8, -9),
        labels = c(
          "Very good",
          "Fairly good",
          "Not very good",
          "Not good at all",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(gheaadu25, gheaadu32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, -1, -2, -3, -8, -9),
        labels = c(
          "Excellent",
          "Very good",
          "Good",
          "Fair",
          "Poor",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(ghea15, ghea16, ghea17, ghea25, ghea32),
      ~ factor(
        .x,
        levels = c(0, 1, -1, -2, -3, -8, -9),
        labels = c(
          "Good/fairly good/very good/excellent",
          "Poor/not good at all/not very good/fair",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
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
    TRUE ~ -3 # Not present/interviewed or NA
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
  mutate(across(
    c(lsi14_15, lsi17, lsi19, lsi20, lsi25, lsi32),
    ~ factor(
      .x,
      levels = c(0, 1, -1, -2, -3, -8, -9),
      labels = c(
        "No",
        "Yes",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, lsi14_15, lsi17, lsi19, lsi20, lsi25, lsi32)
