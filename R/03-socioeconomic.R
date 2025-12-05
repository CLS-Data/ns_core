# Economic Activity --------------------------------------------------------------------
ecoact_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, ecoact17 = W4empsYP),
  S5 = ns_data[["S5youngperson"]] %>%
    select(NSID, ecoact18 = W5mainactYP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, ecoact19 = W6TCurrentAct),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, ecoact20 = W7TCurrentAct),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, ecoactadu25 = W8DACTIVITYC),
  S9 = ns_data[["S9derivedvariable"]] %>%
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
  mutate(
    across(
      c(ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
        labels = c(
          "In paid work",
          "Apprenticeship/government training scheme/training",
          "Education",
          "Unemployed",
          "Looking after home",
          "Other",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(ecoactadu25, ecoactadu32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1, -2, -3, -8, -9),
        labels = c(
          "employee – in paid work",
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
          "Refusal"
        )
      )
    )
  ) %>%
  select(
    NSID,
    ecoact17,
    ecoact18,
    ecoact19,
    ecoact20,
    ecoact25,
    ecoact32,
    ecoactadu25,
    ecoactadu32
  )

# Economic Activity Parents --------------------------------------------------------------------
# Load & select parental employment variables for Sweeps 1–4
ecoactDT_parents_vars <- list(
  S1 = ns_data[["S1familybackground"]] %>%
    select(NSID, ecoactdtma14 = W1empsmum, ecoactdtpa14 = W1empsdad),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, ecoactdtma15 = W2empsmum, ecoactdtpa15 = W2empsdad),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, ecoactdtma16 = W3empsmum, ecoactdtpa16 = W3empsdad),
  S4 = ns_data[["S4familybackground"]] %>%
    select(NSID, ecoactdtma17 = w4empsmum, ecoactdtpa17 = w4empsdad)
)

# Merge all
ecoactDT_parents_all <- reduce(ecoactDT_parents_vars, full_join, by = "NSID")

# Recode helper function
recode_detailed <- function(x) {
  case_when(
    x == 1 ~ 1, # FT
    x == 2 ~ 2, # PT
    x == 3 ~ 3, # Unemployed
    x == 4 ~ 4, # Training
    x == 5 ~ 5, # Education
    x == 6 ~ 6, # Home
    x == 7 ~ 7, # Retired
    x == 8 ~ 8, # Sick/disabled
    x == 9 ~ 9, # Other
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
  ) %>%
  mutate(across(
    c(starts_with("ecoactdtma"), starts_with("ecoactdtpa")),
    ~ factor(
      .x,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -2, -3, -8, -9),
      labels = c(
        "FT paid work",
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
        "Refusal"
      )
    )
  )) %>%
  select(NSID, starts_with("ecoactdtma"), starts_with("ecoactdtpa"))

# NS-SEC Own --------------------------------------------------------------------
# Load NS-SEC variables from relevant sweeps
nssec_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, nssec17 = W4nsseccatYP),
  S5 = ns_data[["S5youngperson"]] %>%
    select(NSID, nssec18 = W5nsseccatYP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, nssec19 = w6nsseccatYP),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, nssec20 = W7NSSECCat),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, nssec25 = W8DNSSEC17, ecoactadu25 = W8DACTIVITYC),
  S9 = ns_data[["S9maininterview"]] %>%
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
  mutate(across(
    starts_with("nssec"),
    ~ factor(
      .x,
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
        17,
        -1,
        -2,
        -3,
        -8,
        -9
      ),
      labels = c(
        "Employers in large organisations",
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
        "Refusal"
      )
    )
  )) %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# NS-SEC Parents --------------------------------------------------------------------
# Load and select parental NS-SEC variables from Sweeps 1–5
nssec_parents_vars <- list(
  S1 = ns_data[["S1familybackground"]] %>%
    select(NSID, nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad),
  S4 = ns_data[["S4familybackground"]] %>%
    select(NSID, nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad),
  S5 = ns_data[["S5familybackground"]] %>%
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
  mutate(across(
    c(starts_with("nssecma"), starts_with("nssecpa")),
    ~ factor(
      .x,
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
        17,
        -1,
        -2,
        -3,
        -8,
        -9
      ),
      labels = c(
        "Employers in large organisations",
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
        "Refusal"
      )
    )
  )) %>%
  select(
    NSID,
    nssecma14,
    nssecpa14,
    nssecma15,
    nssecpa15,
    nssecma16,
    nssecpa16,
    nssecma17,
    nssecpa17,
    nssecma18,
    nssecpa18
  )


# House Ownership --------------------------------------------------------------------
# Load and select house ownership variables from relevant sweeps
housing_vars <- list(
  S1 = ns_data[["S1familybackground"]] %>%
    select(NSID, hown14 = W1hous12HH),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, hown15 = W2Hous12HH),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, hown16 = W3hous12HH),
  S4 = ns_data[["S4familybackground"]] %>%
    select(NSID, hown17 = W4Hous12HH),
  S5 = ns_data[["S5familybackground"]] %>%
    select(NSID, W5Hous12HH, W5Hous12BHH, W5Hous12CHH),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, W6Hous12YP, W6Hous12bYP, W6Hous12cYP),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, W7Hous12YP, W7Hous12bYP, W7Hous12cYP),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, hown25 = W8TENURE),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, hown32 = W9DTENURE)
)

# Merge all datasets
hown_all <- reduce(housing_vars, full_join, by = "NSID")

# Derive harmonised variables
hown_all <- hown_all %>%
  mutate(
    # Detailed versions for S1-S7
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
      hown15 %in% c(-998, -997, -995, -99) ~ -2,
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
      hown17 %in% c(-999, -997) ~ -2,
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
    )
  ) %>%
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
      hown15 %in% c(-998, -997, -995, -99) ~ -2,
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
      hown17 %in% c(-999, -997) ~ -2,
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
    )
  ) %>%
  mutate(
    across(
      c(
        hownteen14,
        hownteen15,
        hownteen16,
        hownteen17,
        hownteen18,
        hownteen19,
        hownteen20
      ),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
        labels = c(
          "Owned outright",
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
          "Refusal"
        )
      )
    ),
    across(
      c(hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
        labels = c(
          "Owned outright",
          "Owned, buying with help of mortgage/loan",
          "Spart rent, part mortgage",
          "Rent it",
          "live rent-free",
          "Other",
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
    hown14,
    hown15,
    hown16,
    hown17,
    hown18,
    hown19,
    hown20,
    hown25,
    hown32,
    hownteen14,
    hownteen15,
    hownteen16,
    hownteen17,
    hownteen18,
    hownteen19,
    hownteen20
  )


# Income Own + Partner --------------------------------------------------------------------
# Load and select income variables from relevant sweeps
income_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S4 = ns_data[["S4youngperson"]] %>% select(NSID),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, inc25 = W8DINCB),
  S9 = ns_data[["S9derivedvariable"]] %>%
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
  mutate(across(
    c(inc25, inc32),
    ~ factor(
      .x,
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
        "less than £25 per week",
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
        "Refusal"
      )
    )
  )) %>%
  select(NSID, inc25, inc32)

# Income Parents --------------------------------------------------------------------
# Load and select household income variables
hh_income_vars <- list(
  S1 = ns_data[["S1familybackground"]] %>%
    select(NSID, incwhh14 = W1GrsswkHH),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, incwhh15 = W2GrsswkHH),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, incwhh16 = W3incestw),
  S4 = ns_data[["S4familybackground"]] %>%
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
      incwhh16 == -1 ~ -8,
      incwhh16 >= 1 & incwhh16 <= 12 ~ incwhh16
    ),

    # Sweep 4
    incwhh17 = case_when(
      is.na(incwhh17) ~ -3,
      incwhh17 %in% c(-996, -99) ~ -3,
      incwhh17 == -92 ~ -9,
      incwhh17 == -1 ~ -8,
      incwhh17 >= 1 & incwhh17 <= 12 ~ incwhh17
    )
  ) %>%
  mutate(
    across(
      c(incwhh14, incwhh15),
      ~ factor(
        .x,
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
          "less than £25 per week",
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
          "Refusal"
        )
      )
    ),
    across(
      c(incwhhcnt14, incwhhcnt15),
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
    ),
    across(
      c(incwhh16, incwhh17),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -1, -2, -3, -8, -9),
        labels = c(
          "up to 49",
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
          "Refusal"
        )
      )
    )
  ) %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# IMD --------------------------------------------------------------------
# Load IMD variables from relevant sweeps
imd_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S4 = ns_data[["S4youngperson"]] %>% select(NSID),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, imd15 = IMDRSCORE),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, imd16 = IMDRSCORE),
  S9 = ns_data[["S9derivedvariable"]] %>%
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
  mutate(across(
    c(imd15, imd16, imd32),
    ~ labelled(
      .x,
      labels = c(
        "Item not applicable" = -1,
        "Script error/information lost" = -2,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8
      )
    )
  )) %>%
  select(NSID, imd15, imd16, imd32)
