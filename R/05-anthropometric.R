# Weight --------------------------------------------------------------------
# Load weight variables from each sweep
wt_vars <- list(
  S1 = ns_data[["S12history"]] %>%
    select(NSID, wt0_s1 = bwtkg),
  S4 = ns_data[["S4history"]] %>%
    select(NSID, wt0_s4 = W4bwtkgYP),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, wt25_raw = W8WEIGHT),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, wt32_raw = W9WEIGHT)
)

# Merge datasets
wt_all <- reduce(wt_vars, full_join, by = "NSID")

# Harmonise weight variables
wt_rec <- wt_all %>%
  mutate(
    wt0 = case_when(
      wt0_s1 > 0 ~ as.numeric(wt0_s1),
      # Note: wt0_s4 has no substantive values when wt0_s1 < 0,
      # so it is only needed when wt0_s1 is NA.
      is.na(wt0_s1) & wt0_s4 > 0 ~ as.numeric(wt0_s4),
      # If both are missing, follow this logic for missing values:
      # If either has dk (-1) or insufficient information (-94), make wt0 dk/insufficient info (-8)
      wt0_s1 %in% c(-1, -94) | wt0_s4 == -94 ~ -8,
      # If either has 'not applicable' (-91), make wt0 'not applicable' (-1)
      wt0_s1 == -91 | wt0_s4 == -91 ~ -1,
      # All other options should default to -3 (not interviewed or similar)
      .default = -3
    ),
    wt25 = case_when(
      wt25_raw > 0 ~ wt25_raw,
      wt25_raw == -9 ~ -9,
      wt25_raw == -8 ~ -8,
      wt25_raw == -1 ~ -1,
      is.na(wt25_raw) ~ -3,
      TRUE ~ -2
    ),
    wt32 = case_when(
      wt32_raw > 0 ~ wt32_raw,
      wt32_raw == -9 ~ -9,
      wt32_raw == -8 ~ -8,
      wt32_raw == -1 ~ -1,
      is.na(wt32_raw) ~ -3,
      TRUE ~ -2
    )
  ) %>%
  mutate(
    across(
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
    )
  )

# Cross-checks (missing values)
wt_rec %>%
  filter(wt0_s1 < 0 | wt0_s4 < 0) %>%
  count(wt0_s1, wt0_s4, wt0)

wt_rec %>%
  filter(wt25_raw < 0 | is.na(wt25_raw)) %>%
  count(wt25_raw, wt25)

wt_rec %>%
  filter(wt32_raw < 0 | is.na(wt32_raw)) %>%
  count(wt32_raw, wt32)

wt_all <- wt_rec %>%
  select(NSID, wt0, wt25, wt32)

# Height --------------------------------------------------------------------
# Load height data from sweeps 8 and 9
ht_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, ht25_raw = W8HEIGHT),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, ht32_raw = W9HEIGHT)
)

# Merge datasets
ht_all <- reduce(ht_vars, full_join, by = "NSID")

# Recode height
ht_rec <- ht_all %>%
  mutate(
    ht25 = case_when(
      ht25_raw > 0 ~ ht25_raw,
      ht25_raw == -9 ~ -9,
      ht25_raw == -8 ~ -8,
      ht25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    ht32 = case_when(
      ht32_raw > 0 ~ ht32_raw,
      ht32_raw == -9 ~ -9,
      ht32_raw == -8 ~ -8,
      ht32_raw == -1 ~ -1,
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
  ))

# Checks
ht_rec %>%
  filter(ht25_raw < 0 | is.na(ht25_raw) | ht25 < 0 | is.na(ht25)) %>%
  count(ht25_raw, ht25)

ht_rec %>%
  filter(ht32_raw < 0 | is.na(ht32_raw) | ht32 < 0 | is.na(ht32)) %>%
  count(ht32_raw, ht32)

ht_rec %>%
  filter(
    (ht25 < 0 &
      ht32 < 0) |
      ht25_32 < 0 |
      is.na(ht25_32) |
      (is.na(ht25) & is.na(ht32))
  ) %>%
  count(ht25, ht32, ht25_32)

ht_all <- ht_rec %>%
  select(NSID, ht25, ht32, ht25_32)

# BMI --------------------------------------------------------------------
# Load BMI data from relevant sweeps
bmi_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, bmi25_raw = W8DBMI),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, bmi32_raw = W9DBMI)
)

# Merge all BMI data by NSID
bmi_all <- reduce(bmi_vars, full_join, by = "NSID")

# Recode BMI variables
bmi_rec <- bmi_all %>%
  mutate(
    bmi25 = case_when(
      bmi25_raw > 0 ~ bmi25_raw,
      bmi25_raw == -9 ~ -9,
      bmi25_raw == -8 ~ -8,
      bmi25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    bmi32 = case_when(
      bmi32_raw > 0 ~ bmi32_raw,
      bmi32_raw == -9 ~ -9,
      bmi32_raw == -8 ~ -8,
      bmi32_raw == -1 ~ -1,
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
  ))

bmi_rec %>%
  filter(bmi25_raw < 0 | is.na(bmi25_raw) | bmi25 < 0 | is.na(bmi25)) %>%
  count(bmi25_raw, bmi25)

bmi_rec %>%
  filter(bmi32_raw < 0 | is.na(bmi32_raw) | bmi32 < 0 | is.na(bmi32)) %>%
  count(bmi32_raw, bmi32)

bmi_all <- bmi_rec %>%
  select(NSID, bmi25, bmi32)
