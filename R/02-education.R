# Own current qualification studied (educaim) --------------------------------------------------------------------

# Current qualification studied indicates which qualification a cohort member is currently pursuing.

# Import education variables from relevant sweeps
educaim_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, educaim17_raw = w4saim),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, educaim19_raw = W6Saim),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, educaim20_raw = W7SAim),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, W8ACTIVITY05, starts_with("W8ACQUC"), starts_with("W8VCQUC")),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, W9ECONACT2, starts_with("W9ACQUC"), starts_with("W9VCQUC"))
)

# Merge by ID
educaim_all <- reduce(educaim_vars, full_join, by = "NSID")

# Define target values and their labels
educaim_labels <- c(
  "NVQ 4-5" = 0L,
  "NVQ 1-3" = 1L,
  "None/entry" = 2L,
  "Other" = 3L,
  "None of these qualifications" = 4L,
  "Not studying" = 5L,
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/participated/interviewed" = -3L,
  "Don't know/insufficient information" = -8L,
  "Refusal" = -9L
)

## Sweeps 4, 6, 7  --------------------------------------------------------------------

# Recode missing values and response categories
educaim_rec_s4_s6_s7 <- educaim_all %>%
  mutate(
    # Sweep 4
    educaim17 = case_when(
      educaim17_raw %in% c(1:9, 10, 11) ~ 1, # NVQ 1-3
      educaim17_raw == 14 ~ 5, # not studying
      educaim17_raw %in% 12 ~ 3, # other
      educaim17_raw == 13 ~ 4, # none of these
      educaim17_raw == -94 ~ -2,
      educaim17_raw == -91 ~ -1,
      TRUE ~ -3 # Not interviewed/present
    ),

    # Sweep 6
    educaim19 = case_when(
      educaim19_raw %in% 1:4 ~ 0,
      educaim19_raw %in% 5:13 ~ 1,
      educaim19_raw == 14 ~ 3,
      educaim19_raw == 15 ~ 4,
      educaim19_raw == 16 ~ 5,
      TRUE ~ -3
    ),

    # Sweep 7
    educaim20 = case_when(
      educaim20_raw %in% 10:13 ~ 0,
      educaim20_raw %in% 1:9 ~ 1,
      educaim20_raw == 14 ~ 3,
      educaim20_raw == -94 ~ -8,
      educaim20_raw == -91 ~ 5,
      TRUE ~ -3
    )
  ) %>%
  # Add value labels
  mutate(
    across(
      c(educaim17, educaim19, educaim20),
      ~ labelled(.x, labels = educaim_labels)
    )
  )

## Checks
educaim_rec_s4_s6_s7 %>%
  count(educaim17_raw, educaim17)

educaim_rec_s4_s6_s7 %>%
  count(educaim19_raw, educaim19)

educaim_rec_s4_s6_s7 %>%
  count(educaim20_raw, educaim20)

## Sweeps 8 and 9 --------------------------------------------------------------------

# S8 and S9 include a variable per each qualification option that is currently being pursued.
# These will be collapsed into a single variable, which indicates the highest qualification currently pursued.

### 1. Define variable groups ###

## Note: 'Not studying' will be derived in the next step.

## Groups for S8
educaim_groups_s8 <- list(
  nvq45 = c(
    "W8ACQUC0A",
    "W8ACQUC0B",
    "W8ACQUC0C",
    "W8ACQUC0D",
    "W8ACQUC0E",
    "W8VCQUC0J",
    "W8VCQUC0K"
  ),
  nvq13 = c(
    "W8ACQUC0F",
    "W8ACQUC0G",
    "W8ACQUC0H",
    "W8ACQUC0I",
    "W8ACQUC0J",
    "W8ACQUC0K",
    "W8ACQUC0L",
    "W8ACQUC0M",
    "W8VCQUC0A",
    "W8VCQUC0B",
    "W8VCQUC0C",
    "W8VCQUC0E",
    "W8VCQUC0F",
    "W8VCQUC0G",
    "W8VCQUC0H",
    "W8VCQUC0I",
    "W8VCQUC0L",
    "W8VCQUC0M",
    "W8VCQUC0N"
  ),
  entry_none = c("W8VCQUC0D"),
  other = c("W8ACQUC0N", "W8VCQUC0O"),
  none_of_these = c("W8ACQUC0O", "W8VCQUC0P"),
  dont_know = c("W8ACQUC0P", "W8VCQUC0Q"),
  refusal = c("W8ACQUC0Q", "W8VCQUC0R")
)

## Check if original S8 variable labels match targets
s8_map <- make_group_map(ns_data[["S8maininterview"]], educaim_groups_s8)
print(s8_map, n = Inf)

## Groups for S9
educaim_groups_s9 <- list(
  nvq45 = c(
    "W9ACQUC0A",
    "W9ACQUC0B",
    "W9ACQUC0C",
    "W9ACQUC0D",
    "W9ACQUC0E",
    "W9ACQUC0F",
    "W9VCQUC0A",
    "W9VCQUC0B",
    "W9VCQUC0C",
    "W9VCQUC0S",
    "W9VCQUC0V",
    "W9VCQUCAC"
  ),
  nvq13 = c(
    "W9ACQUC0G",
    "W9ACQUC0H",
    "W9ACQUC0I",
    "W9ACQUC0J",
    "W9ACQUC0K",
    "W9ACQUC0L",
    "W9ACQUC0M",
    "W9ACQUC0O",
    "W9ACQUC0P",
    "W9ACQUC0Q",
    "W9VCQUC0D",
    "W9VCQUC0E",
    "W9VCQUC0F",
    "W9VCQUC0G",
    "W9VCQUC0H",
    "W9VCQUC0I",
    "W9VCQUC0L",
    "W9VCQUC0M",
    "W9VCQUC0N",
    "W9VCQUC0O",
    "W9VCQUC0P",
    "W9VCQUC0Q",
    "W9VCQUC0R",
    "W9VCQUC0T",
    "W9VCQUC0U",
    "W9VCQUC0W",
    "W9VCQUC0X",
    "W9VCQUC0Y",
    "W9VCQUC0Z",
    "W9VCQUCAA",
    "W9VCQUCAB",
    "W9VCQUCAD",
    "W9VCQUCAE"
  ),
  entry_none = c("W9ACQUC0N"),
  other = c("W9ACQUC0R", "W9VCQUCAF"),
  none_of_these = c("W9ACQUC0S", "W9VCQUCAG"),
  dont_know = c("W9ACQUC0T", "W9VCQUCAH"),
  refusal = c("W9ACQUC0U", "W9VCQUCAI")
)

## Check if original S9 variable labels match targets
s9_map <- make_group_map(ns_data[["S9maininterview"]], educaim_groups_s9)
print(s9_map, n = Inf)

### 2. Derive helpers ###

# Function: Return TRUE if a person responded 'yes' (1) to any of the variables (qualifications)
has_any_tick <- function(vars) {
  dplyr::if_any(dplyr::all_of(vars), ~ .x == 1)
}

# This code will derive each qualification category as a separate TRUE/FALSE helper.
educaim_rec_s8_s9 <- educaim_rec_s4_s6_s7 %>%
  # Sweep 8 (age 25): activity-derived flags, plus tick-box flags
  # Activity derived flags: This uses derived variable 'Current activity: Education: School/College/University',
  # Which indicates whether a person is currently studying (1) or no (0).
  dplyr::mutate(
    s8_not_studying = W8ACTIVITY05 == 0,
    s8_not_applicable_from_activity = W8ACTIVITY05 == -1,
    s8_not_asked_from_activity = W8ACTIVITY05 == -3,
    s8_dk_from_activity = W8ACTIVITY05 == -8,
    s8_refusal_from_activity = W8ACTIVITY05 == -9,
    s8_has_nvq45 = has_any_tick(educaim_groups_s8$nvq45),
    s8_has_nvq13 = has_any_tick(educaim_groups_s8$nvq13),
    s8_has_entry_none = has_any_tick(educaim_groups_s8$entry_none),
    s8_has_other = has_any_tick(educaim_groups_s8$other),
    s8_has_none_of_these = has_any_tick(educaim_groups_s8$none_of_these),
    s8_dk = has_any_tick(educaim_groups_s8$dont_know),
    s8_refusal = has_any_tick(educaim_groups_s8$refusal)
  ) |>
  # Sweep 9 (age 32): econ-derived flags, plus tick-box flags
  # Econ derived flags: This uses derived variable 'Current economic activity (Derived)'.
  # This is used to identify those not studying, which is confirmed if:
  # 1) W9ECONACT2 is not missing.
  # 2) W9ECONACT2 is not 6 (In time edu) or 7 (In part-time edu)
  dplyr::mutate(
    s9_not_studying = W9ECONACT2 %in% c(1:5, 8:14),
    s9_not_applicable_from_econ = W9ECONACT2 == -1,
    s9_not_asked_from_econ = W9ECONACT2 == -3,
    s9_dk_from_econ = W9ECONACT2 == -8,
    s9_refusal_from_econ = W9ECONACT2 == -9,
    s9_has_nvq45 = has_any_tick(educaim_groups_s9$nvq45),
    s9_has_nvq13 = has_any_tick(educaim_groups_s9$nvq13),
    s9_has_entry_none = has_any_tick(educaim_groups_s9$entry_none),
    s9_has_other = has_any_tick(educaim_groups_s9$other),
    s9_has_none_of_these = has_any_tick(educaim_groups_s9$none_of_these),
    s9_dk = has_any_tick(educaim_groups_s9$dont_know),
    s9_refusal = has_any_tick(educaim_groups_s9$refusal)
  )

### 3. Collapse helpers into derived categories ###
educaim_rec_s8_s9 <- educaim_rec_s8_s9 %>%
  mutate(
    educaim25 = dplyr::case_when(
      s8_not_studying ~ 5L,
      s8_has_nvq45 ~ 0L,
      s8_has_nvq13 ~ 1L,
      s8_has_entry_none ~ 2L,
      s8_has_other ~ 3L,
      s8_has_none_of_these ~ 4L,
      s8_not_applicable_from_activity ~ -1L,
      s8_not_asked_from_activity ~ -3L,
      s8_dk | s8_dk_from_activity ~ -8L,
      s8_refusal | s8_refusal_from_activity ~ -9L,
      .default = -3L
    ),
    educaim32 = dplyr::case_when(
      s9_not_studying ~ 5L,
      s9_has_nvq45 ~ 0L,
      s9_has_nvq13 ~ 1L,
      s9_has_entry_none ~ 2L,
      s9_has_other ~ 3L,
      s9_has_none_of_these ~ 4L,
      s9_not_applicable_from_econ ~ -1L,
      s9_not_asked_from_econ ~ -3L,
      s9_dk | s9_dk_from_econ ~ -8L,
      s9_refusal | s9_refusal_from_econ ~ -9L,
      .default = -3L
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(educaim25, educaim32),
      ~ labelled::labelled(.x, labels = educaim_labels)
    )
  )

# Extract derived variables
education_all <- educaim_rec_s8_s9 %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Education Own --------------------------------------------------------------------
# Load education variables from relevant sweeps
educ_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>%
    select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID),
  S8dv = ns_data[["S8derivedvariable"]] %>%
    select(NSID, W8DHANVQH),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, starts_with("W8VCQU")),
  S9dv = ns_data[["S9derivedvariable"]] %>%
    select(NSID, W9DANVQH, W9DVNVQH),
  S9 = ns_data[["S9maininterview"]] %>%
    select(NSID, starts_with("W9ACQU"), starts_with("W9VCQU"))
)

# Merge by ID
educ_all <- reduce(educ_vars, full_join, by = "NSID")

# recode missing valuse and response categories
educ_all_rec <- educ_all %>%
  # Sweep 8
  mutate(
    educ25 = case_when(
      W8DHANVQH %in% c(4, 5) ~ 0,
      W8VCQU0J == 1 | W8VCQU0K == 1 ~ 0,
      W8DHANVQH %in% c(1, 2, 3) ~ 1,
      W8VCQU0A == 1 |
        W8VCQU0B == 1 |
        W8VCQU0C == 1 |
        W8VCQU0E == 1 |
        W8VCQU0F == 1 |
        W8VCQU0G == 1 |
        W8VCQU0H == 1 |
        W8VCQU0I == 1 |
        W8VCQU0L == 1 |
        W8VCQU0M == 1 |
        W8VCQU0N == 1 ~ 1,
      W8VCQU0D == 1 | W8VCQU0P == 1 ~ 2,
      W8DHANVQH == 95 ~ 3,
      W8VCQU0O == 1 ~ 3,
      W8DHANVQH == 96 ~ 4,
      W8DHANVQH < 0 ~ W8DHANVQH,
      W8VCQU0R == 1 ~ -9,
      W8VCQU0Q == 1 ~ -8,
      TRUE ~ -3
    ),

    # Sweep 9
    educ32 = case_when(
      W9DANVQH %in% c(4, 5) | W9DVNVQH %in% c(4, 5) ~ 0,
      W9DANVQH %in% c(1, 2, 3) | W9DVNVQH %in% c(1, 2, 3) ~ 1,
      W9DANVQH == 0 | W9DVNVQH == 0 ~ 2,
      W9DANVQH == 95 | W9DVNVQH == 95 ~ 3,
      W9DANVQH == 96 | W9DVNVQH == 96 ~ 4,
      W9DANVQH < 0 ~ W9DANVQH,
      W9DVNVQH < 0 ~ W9DVNVQH,
      TRUE ~ -3
    ),
    educadtl32 = case_when(
      W9ACQU0A == 1 ~ 1,
      W9ACQU0B == 1 ~ 2,
      W9ACQU0C == 1 ~ 3,
      W9ACQU0D == 1 ~ 4,
      W9ACQU0E == 1 ~ 5,
      W9ACQU0F == 1 ~ 6,
      W9ACQU0G == 1 ~ 7,
      W9ACQU0H == 1 ~ 8,
      W9ACQU0I == 1 ~ 9,
      W9ACQU0J == 1 ~ 10,
      W9ACQU0K == 1 ~ 11,
      W9ACQU0L == 1 ~ 12,
      W9ACQU0M == 1 ~ 13,
      W9ACQU0N == 1 ~ 14,
      W9ACQU0O == 1 ~ 15,
      W9ACQU0P == 1 ~ 16,
      W9ACQU0Q == 1 ~ 17,
      W9ACQU0R == 1 ~ 18,
      W9ACQU0S == 1 ~ 19,
      W9ACQU0T == 1 ~ -8,
      W9ACQU0U == 1 ~ -9,
      W9ACQU0V == 1 ~ -2,
      if_all(starts_with("W9ACQU0"), ~ .x == -1) ~ -1,
      if_all(starts_with("W9ACQU0"), ~ .x == 2) ~ 19,
      if_all(starts_with("W9ACQU0"), ~ is.na(.x) | .x == -3) ~ -3
    ),
    educvdtl32 = case_when(
      W9VCQU0A == 1 ~ 1,
      W9VCQU0B == 1 ~ 2,
      W9VCQU0C == 1 ~ 3,
      W9VCQU0D == 1 ~ 4,
      W9VCQU0E == 1 ~ 5,
      W9VCQU0F == 1 ~ 6,
      W9VCQU0G == 1 ~ 7,
      W9VCQU0H == 1 ~ 8,
      W9VCQU0I == 1 ~ 9,
      W9VCQU0J == 1 ~ 10,
      W9VCQU0K == 1 ~ 11,
      W9VCQU0L == 1 ~ 12,
      W9VCQU0M == 1 ~ 13,
      W9VCQU0N == 1 ~ 14,
      W9VCQU0O == 1 ~ 15,
      W9VCQU0P == 1 ~ 16,
      W9VCQU0Q == 1 ~ 17,
      W9VCQU0R == 1 ~ 18,
      W9VCQU0S == 1 ~ 19,
      W9VCQU0T == 1 ~ 20,
      W9VCQU0U == 1 ~ 21,
      W9VCQU0V == 1 ~ 22,
      W9VCQU0W == 1 ~ 23,
      W9VCQU0X == 1 ~ 24,
      W9VCQU0Y == 1 ~ 25,
      W9VCQU0Z == 1 ~ 26,
      W9VCQUAA == 1 ~ 27,
      W9VCQUAB == 1 ~ 28,
      W9VCQUAC == 1 ~ 29,
      W9VCQUAD == 1 ~ 30,
      W9VCQUAE == 1 ~ 31,
      W9VCQUAF == 1 ~ 32,
      W9VCQUAG == 1 ~ 33,
      W9VCQUAH == 1 ~ -8,
      W9VCQUAI == 1 ~ -9,
      if_all(starts_with("W9VCQU"), ~ .x == -1) ~ -1,
      if_all(starts_with("W9VCQU"), ~ .x == 2) ~ 33,
      if_all(starts_with("W9VCQU"), ~ is.na(.x) | .x == -3) ~ -3
    )
  ) %>%
  mutate(
    across(
      c(educ25, educ32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
        labels = c(
          "NVQ 4-5",
          "NVQ 1-3",
          "None/entry",
          "Other",
          "None of these qualifications",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    educadtl32 = factor(
      educadtl32,
      levels = c(1:19, -1, -2, -3, -8, -9),
      labels = c(
        "Doctorate or equivalent",
        "Masters or equivalent",
        "Undergraduate or equivalent",
        "Post-graduate Diplomas and Certificates",
        "Diplomas in higher education and other higher education qualifications",
        "Teaching qualifications for schools or further education",
        "A/AS Levels or equivalent",
        "GCSE - Grade A-C, Level 4-9",
        "GCSE - Grade D-G, Level 1-3",
        "Scottish Qualifications - SCE Higher",
        "Scottish Qualifications - Scottish Certificate Sixth Year Studies",
        "Scottish Qualifications - SCE Standard",
        "Scottish Qualifications - National 4 and 5",
        "Scottish Qualifications - National 2 and 3",
        "Irish Qualifications - Leaving Certificate",
        "Irish Qualifications - Junior Certificate grade A-C",
        "Irish Qualifications - Junior Certificate grade D and below",
        "Other academic qualifications (including overseas)",
        "None of these qualifications",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    ),
    educvdtl32 = factor(
      educvdtl32,
      levels = c(1:33, -1, -2, -3, -8, -9),
      labels = c(
        "Professional qualifications at degree level",
        "Nursing or other medical qualifications",
        "NVQ or SVQ - Level 4 or 5",
        "NVQ or SVQ -  Level 3",
        "NVQ or SVQ - Level 2",
        "NVQ or SVQ - Level 1",
        "GNVQ - GNVQ Advanced",
        "GNVQ - GNVQ Intermediate",
        "GNVQ - Level 3",
        "GNVQ - Level 2",
        "GNVQ - Level Foundation",
        "City & Guilds - advanced craft, Part III",
        "City & Guilds - craft, Part II",
        "City & Guilds - craft, Part I",
        "City & Guilds - Level 3",
        "City & Guilds - Level 2",
        "City & Guilds - Level 1",
        "RSA - Advanced Diploma",
        "RSA - Higher Diploma",
        "RSA - Diploma",
        "RSA - RSA Stage I, II,III",
        "BTEC - Higher Level BTEC",
        "BTEC - BTEC National",
        "BTEC - BTEC First",
        "SCOTVEC-  SCOTVEC National Certificate",
        "SCOTVEC-  SCOTVEC first or general diploma",
        "SCOTVEC - SCOTVEC general diploma",
        "SCOTVEC - SCOTVEC modules",
        "HND or HNC",
        "OND or ONCM",
        "Junior certificate",
        "Other vocational qualifications (including some overseas)",
        "None of these qualifications",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )

# Select

educ_all <- educ_all_rec %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Education Parents --------------------------------------------------------------------

# Load and rename relevant variables from each sweep
parent_edu_vars <- list(
  S1 = ns_data[["S1familybackground"]] %>%
    select(NSID, educma_S1 = W1hiqualmum, educpa_S1 = W1hiqualdad),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, educma_S2 = W2hiqualmum, educpa_S2 = W2hiqualdad),
  S4 = ns_data[["S4familybackground"]] %>%
    select(NSID, educma_S4 = w4hiqualmum, educpa_S4 = w4hiqualdad)
)

#  Merge all sweeps by NSID
parent_edu_all <- reduce(parent_edu_vars, full_join, by = "NSID")

# Recode missing values and response categories
parent_edu_all <- parent_edu_all %>%
  mutate(
    across(
      matches("educ(ma|pa)_S[1-4]"),
      ~ case_when(
        .x == -92 ~ -9,
        .x == -91 ~ -1,
        .x %in% c(-98) ~ -3,
        .x %in% c(-999, -99, -94) ~ -2,
        TRUE ~ .x
      )
    )
  )

parent_edu_detailed_labels <- c(
  "Higher Degree" = 1L,
  "First degree" = 2L,
  "HE Diploma" = 3L,
  "HNC/HND/NVQ4" = 4L,
  "Teaching qualification, non-degree" = 5L,
  "Nursing qualification, non-degree" = 6L,
  "A Levels" = 7L,
  "OND/ONC" = 8L,
  "City and guilds part III, NVQ3" = 9L,
  "CSYS" = 10L,
  "Scottish Higher Grade" = 11L,
  "AS Level" = 12L,
  "Trade apprenticeship" = 13L,
  "City and guilds part II, NVQ2" = 14L,
  "GCSE grade A-C and equivalent" = 15L,
  "GCSE grade D-E and equivalent" = 16L,
  "City and guilds part I, NVQ1" = 17L,
  "Youth training, skill seekers" = 18L,
  "Qualification, level unspecified" = 19L,
  "No qualification mentioned" = 20L,
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/participated/interviewed" = -3L,
  "Don't know/insufficient information" = -8L,
  "Refusal" = -9L
)

parent_edu_simple_labels <- c(
  "NVQ 4-5" = 0L,
  "NVQ 1-3" = 1L,
  "None/entry" = 2L,
  "Other" = 3L,
  "None of these qualifications" = 4L,
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/participated/interviewed" = -3L,
  "Don't know/insufficient information" = -8L,
  "Refusal" = -9L
)

# Derive full education and transform to simple education
parent_edu_rec <- parent_edu_all %>%
  mutate(
    # mother full education (aggregate the information from sweeps 1-4)
    educdtlma = case_when(
      educma_S1 > 0 ~ educma_S1,
      educma_S2 > 0 ~ educma_S2,
      educma_S4 > 0 ~ educma_S4,
      educma_S1 < 0 ~ educma_S1,
      educma_S2 < 0 ~ educma_S2,
      educma_S4 < 0 ~ educma_S4,
      .default = -3 # Not interviewed / present
    ),
    # transform to 3-level education (mother)
    educma = case_when(
      educdtlma %in% 1:4 ~ 0,
      educdtlma %in% 5:17 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3, # other
      educdtlma == 20 ~ 4, # none of these qualifications
      .default = educdtlma # keep negatives as-is
    ),
    # father full education (aggregate the information from sweeps 1-4)
    educdtlpa = case_when(
      educpa_S1 > 0 ~ educpa_S1,
      educpa_S2 > 0 ~ educpa_S2,
      educpa_S4 > 0 ~ educpa_S4,
      educpa_S1 < 0 ~ educpa_S1,
      educpa_S2 < 0 ~ educpa_S2,
      educpa_S4 < 0 ~ educpa_S4,
      .default = -3
    ),
    # transform to 3-level education (father)
    educpa = case_when(
      educdtlpa %in% 1:4 ~ 0,
      educdtlpa %in% 5:17 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      .default = educdtlpa # keep negatives as-is
    )
  ) %>%
  mutate(
    across(
      c(educma, educpa),
      ~ labelled(
        .x,
        labels = parent_edu_simple_labels
      )
    ),
    across(
      c(educdtlma, educdtlpa),
      ~ labelled(
        .x,
        labels = parent_edu_detailed_labels
      )
    )
  )

parent_edu_all <- parent_edu_rec %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)
