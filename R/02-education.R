# Current Aim Education Own --------------------------------------------------------------------
# Load education variables from relevant sweeps
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

# recode missing valuse and response categories
educaim_all <- educaim_all %>%
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
    )
  ) %>%

  # Sweep 8
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5, #not studying
      W8ACQUC0A == 1 |
        W8ACQUC0B == 1 |
        W8ACQUC0C == 1 |
        W8ACQUC0D == 1 |
        W8ACQUC0E == 1 |
        W8VCQUC0J == 1 |
        W8VCQUC0K == 1 ~ 0,
      W8ACQUC0F == 1 |
        W8ACQUC0G == 1 |
        W8ACQUC0H == 1 |
        W8ACQUC0I == 1 |
        W8ACQUC0J == 1 |
        W8ACQUC0K == 1 |
        W8ACQUC0L == 1 |
        W8ACQUC0M == 1 |
        W8VCQUC0A == 1 |
        W8VCQUC0B == 1 |
        W8VCQUC0C == 1 |
        W8VCQUC0E == 1 |
        W8VCQUC0F == 1 |
        W8VCQUC0G == 1 |
        W8VCQUC0H == 1 |
        W8VCQUC0I == 1 |
        W8VCQUC0L == 1 |
        W8VCQUC0M == 1 |
        W8VCQUC0N == 1 ~ 1,
      W8VCQUC0D == 1 | W8VCQUC0P == 1 ~ 2,
      W8ACQUC0N == 1 |
        W8VCQUC0O == 1 ~ 3,
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
      W9ACQUC0A == 1 |
        W9ACQUC0B == 1 |
        W9ACQUC0C == 1 |
        W9ACQUC0D == 1 |
        W9ACQUC0E == 1 |
        W9ACQUC0F == 1 |
        W9VCQUC0A == 1 |
        W9VCQUC0B == 1 |
        W9VCQUC0C == 1 |
        W9VCQUC0S == 1 |
        W9VCQUC0V == 1 |
        W9VCQUCAC == 1 ~ 0,
      W9ACQUC0G == 1 |
        W9ACQUC0H == 1 |
        W9ACQUC0I == 1 |
        W9ACQUC0J == 1 |
        W9ACQUC0K == 1 |
        W9ACQUC0L == 1 |
        W9ACQUC0M == 1 |
        W9ACQUC0O == 1 |
        W9ACQUC0P == 1 |
        W9ACQUC0Q == 1 |
        W9VCQUC0D == 1 |
        W9VCQUC0E == 1 |
        W9VCQUC0F == 1 |
        W9VCQUC0G == 1 |
        W9VCQUC0H == 1 |
        W9VCQUC0I == 1 |
        W9VCQUC0L == 1 |
        W9VCQUC0M == 1 |
        W9VCQUC0N == 1 |
        W9VCQUC0O == 1 |
        W9VCQUC0P == 1 |
        W9VCQUC0Q == 1 |
        W9VCQUC0R == 1 |
        W9VCQUC0T == 1 |
        W9VCQUC0U == 1 |
        W9VCQUC0W == 1 |
        W9VCQUC0X == 1 |
        W9VCQUC0Y == 1 |
        W9VCQUC0Z == 1 |
        W9VCQUCAA == 1 |
        W9VCQUCAB == 1 |
        W9VCQUCAD == 1 |
        W9VCQUCAE == 1 ~ 1,
      W9ACQUC0N == 1 ~ 2,
      W9ACQUC0R == 1 |
        W9VCQUCAF == 1 ~ 3,
      W9ACQUC0S == 1 |
        W9VCQUCAG == 1 ~ 4,
      W9ACQUC0T == 1 |
        W9VCQUCAH == 1 ~ -8,
      W9ACQUC0U == 1 |
        W9VCQUCAI == 1 ~ -9,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(
    c(educaim17, educaim19, educaim20, educaim25, educaim32),
    ~ factor(
      .x,
      levels = c(0, 1, 2, 3, 4, 5, -1, -2, -3, -8, -9),
      labels = c(
        "NVQ 4-5",
        "NVQ 1-3",
        "None/entry",
        "Other",
        "None of these qualifications",
        "Not studying",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
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
educ_all <- educ_all %>%
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
  ) %>%
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
    educdtlma = case_when(
      !is.na(educma_S4) & educma_S4 > 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 > 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 > 0 ~ educma_S1,
      !is.na(educma_S4) & educma_S4 < 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 < 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 < 0 ~ educma_S1,
      TRUE ~ -3 # Not interviewed / present
    ),
    #transform to 3-level education (mother)
    educma = case_when(
      educdtlma %in% 1:4 ~ 0,
      educdtlma %in% 5:17 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3, # other
      educdtlma == 20 ~ 4, # none of these qualifications
      TRUE ~ educdtlma # keep negatives as-is
    ),
    #father full education (aggregate the information from sweeps 1-4)
    educdtlpa = case_when(
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
      educdtlpa %in% 1:4 ~ 0,
      educdtlpa %in% 5:17 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ educdtlpa # keep negatives as-is
    )
  ) %>%
  mutate(
    across(
      c(educma, educpa),
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
    across(
      c(educdtlma, educdtlpa),
      ~ factor(
        .x,
        levels = c(1:20, -1, -2, -3, -8, -9),
        labels = c(
          "Higher Degree",
          "First degree",
          "HE Diploma",
          "HNC/HND/NVQ4",
          "Teaching qualification, non-degree",
          "Nursing qualification, non-degree",
          "A Levels",
          "OND/ONC",
          "City and guilds part III, NVQ3",
          "CSYS",
          "Scottish Higher Grade",
          "AS Level",
          "Trade apprenticeship",
          "City and guilds part II, NVQ2",
          "GCSE grade A-C and equivalent",
          "GCSE grade D-E and equivalent",
          "City and guilds part I, NVQ1",
          "Youth training, skill seekers",
          "Qualification, level unspecified",
          "No qualification mentioned",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)
