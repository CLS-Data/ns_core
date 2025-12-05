# Smoke --------------------------------------------------------------------
# Load smoking data from relevant sweeps
smoking_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID, smknw14 = W1cignowYP, smk14 = W1cigfreqYP),
  S2 = ns_data[["S2youngperson"]] %>%
    select(NSID, smknw15 = W2cignowYP, smk15 = W2cigfreqYP),
  S3 = ns_data[["S3youngperson"]] %>%
    select(NSID, smknw16 = W3cignowYP, smk16 = W3cigfreqYP),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(NSID, smk25 = W8SMOKING),
  S9 = ns_data[["S9maininterview"]] %>%
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
  ) %>%
  mutate(
    smknw14 = case_when(
      smk14 == 0 ~ 0,
      smknw14 > 0 ~ recode_smknw14_16(smknw14),
      TRUE ~ recode_smknw14_16(smknw14)
    ),
    smknw15 = case_when(
      smk15 == 0 ~ 0,
      smknw15 > 0 ~ recode_smknw14_16(smknw15),
      TRUE ~ recode_smknw14_16(smknw15)
    ),
    smknw16 = case_when(
      smk16 == 0 ~ 0,
      smknw16 > 0 ~ recode_smknw14_16(smknw16),
      TRUE ~ recode_smknw14_16(smknw16)
    ),
    smknw25 = recode_smknw25_32(smk25),
    smknw32 = recode_smknw25_32(smk32)
  ) %>%
  mutate(
    across(
      c(smk14, smk15, smk16, smk25, smk32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, -1, -2, -3, -8, -9),
        labels = c(
          "Never",
          "Used to smoke, don’t at all now",
          "Smoke occasionally – not every day",
          "Smoke almost every day",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(smknw14, smknw15, smknw16, smknw25, smknw32),
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
    )
  ) %>%
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
      any(ever_flags %in% 14:32, na.rm = TRUE) ~ min(
        unlist(ever_flags),
        na.rm = TRUE
      ),
      all(
        c(
          alcever_S1,
          alcever_S2,
          alcever_S3,
          alcever_S4,
          alcever_S6,
          alcever_S7
        ) ==
          2 &
          c(audita25, audita32) == 1,
        na.rm = TRUE
      ) ~ 99,
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


alc_all_clean <- alc_all %>%
  mutate(
    audita25 = case_when(
      audita25 > 0 ~ audita25 - 1,
      audita25 < 0 ~ audita25,
      is.na(audita25) ~ -3,
    ),
    audita32 = case_when(
      audita32 > 0 ~ audita32 - 1,
      audita32 < 0 ~ audita32,
      is.na(audita32) ~ -3,
    )
  ) %>%
  mutate(
    auditb25 = case_when(
      audita25 == 0 ~ 0,
      audita25 > 0 & auditb25 > 0 ~ auditb25,
      auditb25 < 0 ~ auditb25,
      is.na(auditb25) ~ -3
    ),
    auditb32 = case_when(
      audita32 == 0 ~ 0,
      audita32 > 0 & auditb32 > 0 ~ auditb32,
      auditb32 < 0 ~ auditb32,
      is.na(auditb32) ~ -3
    ),
    auditc25 = case_when(
      audita25 == 0 ~ 0,
      is.na(auditc25) ~ -3,
      auditc25 < 0 ~ auditc25,
      TRUE ~ auditc25 - 1
    ),
    auditc32 = case_when(
      audita32 == 0 ~ 0,
      is.na(auditc32) ~ -3,
      auditc32 < 0 ~ auditc32,
      TRUE ~ auditc32 - 1
    )
  ) %>%
  mutate(
    alcfst = factor(
      alcfst,
      levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -1, -2, -3, -8, -9),
      labels = c(
        "Age 14",
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
        "Refusal"
      )
    ),
    across(
      c(alcfreq14, alcfreq15, alcfreq16, alcfreq17, alcfreq19, alcfreq20),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
        labels = c(
          "Less often/not at all",
          "Once every couple of months",
          "Once to three times a month",
          "Once or twice a week",
          "Most days",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(audita25, audita32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
        labels = c(
          "Never",
          "Monthly or less",
          "2–4 times a month",
          "2–3 times a week",
          "4 or more times a week",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(auditb25, auditb32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, 5, -1, -2, -3, -8, -9),
        labels = c(
          "0",
          "1–2 drinks",
          "3–4 drinks",
          "5–6 drinks",
          "7–9 drinks",
          "10+",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(auditc25, auditc32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
        labels = c(
          "Never",
          "Less than monthly",
          "Monthly",
          "Weekly",
          "Daily or almost daily",
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
      now_oth20 = starts_with("W7DrugOftenYP0")
    ),
  S8 = ns_data[["S8selfcompletion"]] %>%
    select(
      NSID,
      canevr25 = W8DRUGYP10A,
      othevr25 = starts_with("W8DRUGYP10"),
      yr_cann25 = W8DRUGYP20A,
      yr_oth25 = starts_with("W8DRUGYP20"),
      now_cann25 = W8DRUGOFTEN0A,
      now_oth25 = starts_with("W8DRUGOFTEN0")
    ),
  S9 = ns_data[["S9maininterview"]] %>%
    select(
      NSID,
      canevr32 = W9DRUGYP10A,
      othevr32 = starts_with("W9DRUGYP1"),
      yr_cann32 = W9DRUGYP20A,
      yr_oth32 = starts_with("W9DRUGYP2"),
      now_cann32 = W9DRUGOFTEN0A,
      now_oth32 = starts_with("W9DRUGOFTEN0")
    )
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
  mutate(
    across(
      c(
        canevr14,
        canevr15,
        canevr16,
        canevr17,
        canevr19,
        canevr20,
        now_cann19,
        othevr19,
        othevr20,
        now_oth19
      ),
      recode_drugbin1_7
    ),
    across(c(now_cann20, starts_with("now_oth20")), recode_drugoft7),
    across(
      c(
        canevr25,
        canevr32,
        yr_cann25,
        yr_cann32,
        now_cann25,
        now_cann32,
        starts_with("yr_oth25"),
        starts_with("yr_oth32"),
        starts_with("othevr25"),
        starts_with("othevr32"),
        starts_with("now_oth25"),
        starts_with("now_oth32")
      ),
      recode_drugbin89
    )
  )

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
    drgcnbevr = pmax(
      canevr14,
      canevr15,
      canevr16,
      canevr17,
      canevr19,
      canevr20,
      canevr25,
      canevr32,
      na.rm = FALSE
    ),
    drgothevr = pmax(othevr19, othevr20, othevr25, othevr32, na.rm = FALSE)
  )

# Derive: First time use
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
      rowSums(
        select(., othevr19, othevr20, othevr25, othevr32),
        na.rm = TRUE
      ) ==
        0 ~ 99,
      TRUE ~ -2
    )
  )

# Derive: Current use
drug_all <- drug_all %>%
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

# Final selection
drug_final <- drug_all %>%
  mutate(
    across(
      c(drgcnbevr, drgothevr, starts_with("drgcnbnw"), starts_with("drgothnw")),
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
    ),
    across(
      c(drgcnbfst, drgothfst),
      ~ factor(
        .x,
        levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -1, -2, -3, -8, -9),
        labels = c(
          "Age 14",
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
          "Refusal"
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
spt_all <- reduce(exercise_vars, full_join, by = "NSID")

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

# Apply recoding
spt_all <- spt_all %>%
  mutate(
    spt14 = recode_exercise(spt14),
    spt15 = recode_exercise(spt15),
    spt17 = recode_exercise(spt17),
    spt19 = recode_exercise(spt19),
    spt20 = recode_exercise(spt20),
    spt25 = case_when(
      # values from 0–7 days
      spt25 %in% c(5, 6, 7) ~ 0,
      spt25 %in% c(2, 3, 4) ~ 1,
      spt25 == 1 ~ 2,
      spt25 == 0 ~ 3,
      spt25 == -9 ~ -9,
      spt25 == -8 ~ -8,
      spt25 == -1 ~ -1,
      is.na(spt25) ~ -3
    ),
    spt32 = case_when(
      spt32 %in% c(5, 6, 7) ~ 0,
      spt32 %in% c(2, 3, 4) ~ 1,
      spt32 == 1 ~ 2,
      spt32 == 0 ~ 3,
      spt32 == -9 ~ -9,
      spt32 == -8 ~ -8,
      spt32 == -1 ~ -1,
      is.na(spt32) | spt32 == -3 ~ -3
    )
  ) %>%
  mutate(across(
    c(starts_with("spt")),
    ~ factor(
      .x,
      levels = c(0, 1, 2, 3, -1, -2, -3, -8, -9),
      labels = c(
        "Most days",
        "More than once a week",
        "Once a week",
        "Less than once a week/hardly ever/never",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, spt14, spt15, spt17, spt19, spt20, spt25, spt32)

# Absence --------------------------------------------------------------------
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
absence_all <- reduce(absence_vars, full_join, by = "NSID")

# Recode function for harmonised values
recode_absence <- function(x) {
  case_when(
    x == 1 ~ 1, # yes
    x == 2 ~ 0, # no
    x %in% c(-97, -92) ~ -9,
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
  mutate(across(
    starts_with("abs1m"),
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
  select(NSID, abs1m14, abs1m15, abs1m16)

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
  mutate(across(
    c(starts_with("abs1m"), starts_with("expl")),
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
  mutate(across(
    starts_with("trua"),
    ~ factor(
      .x,
      levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
      labels = c(
        "Never played truant",
        "For weeks at a time",
        "Several days at a time",
        "Particular days or lessons",
        "Odd day or lesson",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
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
police_all <- reduce(police_vars, full_join, by = "NSID")

# Recode function for binary variables (pol15,16)
recode_pol <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    x %in% c(-97, -92) ~ -9,
    x == -91 ~ -1,
    x %in% c(-96, -1) ~ -8,
    x %in% c(-998, -997, -995) ~ -2,
    x %in% c(-99) ~ -3,
    is.na(x) ~ -3
  )
}

# Recode function for contact counts
recode_cnt <- function(x, ever) {
  case_when(
    ever %in% c(2, 3) ~ 0,
    x >= 0 ~ x,
    x %in% c(-97, -92) ~ -9,
    x == -91 ~ -1,
    x %in% c(-96, -1) ~ -8,
    x %in% c(-998, -997, -995) ~ -2,
    x %in% c(-99, -996) ~ -3,
    is.na(x) ~ -3,
  )
}

# Apply recoding
police_all <- police_all %>%
  mutate(
    polcnt14 = recode_cnt(polcnt14, pol14),
    polcnt15 = recode_cnt(polcnt15, pol15),
    polcnt16 = recode_cnt(polcnt16, pol16),
    polcnt17 = recode_cnt(polcnt17, pol17),
    across(
      starts_with("polwrn"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      )
    ),
    across(
      starts_with("polars"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      )
    ),
    across(
      starts_with("polcau"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      )
    ),
    across(
      starts_with("polglt"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      )
    ),
    across(
      starts_with("polpnd"),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x < 0 ~ .x,
        TRUE ~ -3
      )
    )
  ) %>%
  mutate(
    pol14 = case_when(
      pol14 %in% c(1, 3) ~ 1,
      pol14 == 2 ~ 0,
      pol14 %in% c(-97, -92) ~ -9,
      pol14 == -91 ~ -1,
      pol14 %in% c(-96, -1) ~ -8,
      pol14 %in% c(-99) ~ -3,
      TRUE ~ -3
    ),
    pol15 = recode_pol(pol15),
    pol16 = recode_pol(pol16),
    pol17 = case_when(
      pol17 %in% c(1, 3) ~ 1,
      pol17 == 2 ~ 0,
      pol17 %in% c(-97, -92) ~ -9,
      pol17 == -91 ~ -1,
      pol17 %in% c(-96, -1) ~ -8,
      pol17 %in% c(-99) ~ -3,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    across(
      c(
        starts_with("polwrn"),
        starts_with("polars"),
        starts_with("polcau"),
        starts_with("polglt"),
        starts_with("polpnd")
      ),
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
    ),
    across(
      c(pol14, pol15, pol16, pol17),
      ~ factor(
        .x,
        levels = c(0, 1, -1, -2, -3, -8, -9),
        labels = c(
          "No",
          "Yes/not in last 3 years",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(starts_with("polcnt")),
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
  ) %>%
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
  select(NSID, bul14, bul15, bul16, bul17, bul20, bul25)
