# Weight --------------------------------------------------------------------
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
  select(-wt0.x, -wt0.y) %>%
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
  mutate(across(
    c(wt0, wt25, wt32),
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
  select(NSID, wt0, wt25, wt32)

# Height --------------------------------------------------------------------
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
  mutate(across(
    c(ht25, ht32, ht25_32),
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
  select(NSID, ht25, ht32, ht25_32)

# BMI --------------------------------------------------------------------
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
  mutate(across(
    c(bmi25, bmi32),
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
  select(NSID, bmi25, bmi32)
