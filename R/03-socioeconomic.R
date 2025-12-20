# Prerequisite: 00-load-raw-data.R (this script also sources helpers.R)
#
# If you are running this script on its own, please run the following first
# from the project root:
#
# source(here::here("R", "00-load-raw-data.R"))
#
# or manually run 00-load-raw-data.R before this script.

# Economic Activity --------------------------------------------------------------------

ecoact_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S4 = ns_data[["S4youngperson"]] %>%
    select(NSID, ecoact17_raw = W4empsYP),
  S5 = ns_data[["S5youngperson"]] %>%
    select(NSID, ecoact18_raw = W5mainactYP),
  S6 = ns_data[["S6youngperson"]] %>%
    select(NSID, ecoact19_raw = W6TCurrentAct),
  S7 = ns_data[["S7youngperson"]] %>%
    select(NSID, ecoact20_raw = W7TCurrentAct),
  S8 = ns_data[["S8derivedvariable"]] %>%
    select(NSID, ecoactadu25_raw = W8DACTIVITYC),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, ecoactadu32_raw = W9DACTIVITYC)
)

# Merge by NSID
ecoact_all <- reduce(ecoact_vars, full_join, by = "NSID")

# Harmonise missing values and derive economic activity variables
ecoact_rec <- ecoact_all %>%
  mutate(
    ## Sweep 4
    ecoact17 = case_when(
      ecoact17_raw %in% 1:2 ~ 1, # In paid work
      ecoact17_raw == 4 ~ 2, # Apprenticeship/government training scheme/training
      ecoact17_raw == 5 | ecoact17_raw == -91 ~ 3, # Education
      ecoact17_raw == 3 ~ 4, # Unemployed
      ecoact17_raw == 6 ~ 5, # Looking after home/family
      ecoact17_raw %in% c(7, 8, 9) ~ 6, # Sick/disabled, other, doing something else
      ecoact17_raw == -92 ~ -9,
      ecoact17_raw == -999 ~ -2,
      ecoact17_raw == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 5
    ecoact18 = case_when(
      ecoact18_raw == 3 ~ 1,
      ecoact18_raw %in% c(1, 5, 6) ~ 2,
      ecoact18_raw %in% c(2, 4) ~ 3,
      ecoact18_raw == 7 ~ 4,
      ecoact18_raw == 8 ~ 5,
      ecoact18_raw %in% 9:11 ~ 6,
      ecoact18_raw == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 6
    ecoact19 = case_when(
      ecoact19_raw == 3 ~ 1,
      ecoact19_raw %in% c(4, 5) ~ 2,
      ecoact19_raw %in% c(1, 2, 10) ~ 3,
      ecoact19_raw == 8 ~ 4,
      ecoact19_raw == 7 ~ 5,
      ecoact19_raw %in% c(6, 9, 11) ~ 6,
      ecoact19_raw == -91 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 7
    ecoact20 = case_when(
      ecoact20_raw == 3 ~ 1,
      ecoact20_raw %in% c(4, 5, 11) ~ 2,
      ecoact20_raw %in% c(1, 2, 9) ~ 3,
      ecoact20_raw == 8 ~ 4,
      ecoact20_raw == 7 ~ 5,
      ecoact20_raw %in% c(6, 10, 12:15) ~ 6,
      ecoact20_raw == -91 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 8
    ecoact25 = case_when(
      ecoactadu25_raw %in% c(1, 2) ~ 1,
      ecoactadu25_raw %in% c(6, 7) ~ 2,
      ecoactadu25_raw == 5 ~ 3,
      ecoactadu25_raw == 4 ~ 4,
      ecoactadu25_raw == 9 ~ 5,
      ecoactadu25_raw %in% c(3, 8, 10) ~ 6,
      ecoactadu25_raw == -9 ~ -9,
      ecoactadu25_raw == -8 ~ -8,
      ecoactadu25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 9
    ecoact32 = case_when(
      ecoactadu32_raw %in% c(1, 2) ~ 1,
      ecoactadu32_raw %in% c(6, 7) ~ 2,
      ecoactadu32_raw == 5 ~ 3,
      ecoactadu32_raw == 4 ~ 4,
      ecoactadu32_raw == 9 ~ 5,
      ecoactadu32_raw %in% c(3, 8, 10) ~ 6,
      ecoactadu32_raw == -9 ~ -9,
      ecoactadu32_raw == -8 ~ -8,
      ecoactadu32_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Detailed versions (S8, S9 only)
    ecoactadu25 = case_when(
      !is.na(ecoactadu25_raw) ~ ecoactadu25_raw,
      is.na(ecoactadu25_raw) ~ -3
    ),
    ecoactadu32 = case_when(
      !is.na(ecoactadu32_raw) ~ ecoactadu32_raw,
      is.na(ecoactadu32_raw) ~ -3
    )
  ) %>%
  mutate(
    across(
      c(ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32),
      ~ labelled(
        .x,
        labels = c(
          "In paid work" = 1L,
          "Apprenticeship/government training scheme/training" = 2L,
          "Education" = 3L,
          "Unemployed" = 4L,
          "Looking after home" = 5L,
          "Other" = 6L,
          common_missing_labels
        )
      )
    ),
    across(
      c(ecoactadu25, ecoactadu32),
      ~ labelled(
        .x,
        labels = c(
          "employee – in paid work" = 1L,
          "self employed" = 2L,
          "voluntary work" = 3L,
          "Unemployed" = 4L,
          "Education" = 5L,
          "Apprenticeship" = 6L,
          "government employment scheme" = 7L,
          "sick/disabled" = 8L,
          "Looking after home/family" = 9L,
          "Something else" = 10L,
          common_missing_labels
        )
      )
    )
  )

# Checks
ecoact_rec %>%
  count(ecoact17_raw, ecoact17)

ecoact_rec %>%
  count(ecoact18_raw, ecoact18)

ecoact_rec %>%
  count(ecoact19_raw, ecoact19)

ecoact_rec %>%
  count(ecoact20_raw, ecoact20)

ecoact_rec %>%
  count(ecoactadu25_raw, ecoact25)

# Extract variables
ecoact_all <- ecoact_rec %>%
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

ecoactDT_parents_all <- ecoactDT_parents_vars %>%
  # Merge all
  reduce(full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'ecoact*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("ecoact")
  )

# Recode helper function
recode_ecoactDT <- function(x) {
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
ecoactDT_parents_rec <- ecoactDT_parents_all %>%
  mutate(
    ecoactdtma14 = recode_ecoactDT(ecoactdtma14_raw),
    ecoactdtpa14 = recode_ecoactDT(ecoactdtpa14_raw),
    ecoactdtma15 = recode_ecoactDT(ecoactdtma15_raw),
    ecoactdtpa15 = recode_ecoactDT(ecoactdtpa15_raw),
    ecoactdtma16 = recode_ecoactDT(ecoactdtma16_raw),
    ecoactdtpa16 = recode_ecoactDT(ecoactdtpa16_raw),
    ecoactdtma17 = recode_ecoactDT(ecoactdtma17_raw),
    ecoactdtpa17 = recode_ecoactDT(ecoactdtpa17_raw)
  ) %>%
  mutate(
    across(
      c(starts_with("ecoactdt") & !ends_with("raw")),
      ~ labelled(
        .x,
        labels = c(
          "FT paid work" = 1,
          "PT paid work" = 2,
          "Unemployed" = 3,
          "Training" = 4,
          "Education" = 5,
          "Looking after home/family" = 6,
          "Retired from work altogether" = 7,
          "Sick/disabled" = 8,
          "Other" = 9,
          common_missing_labels
        )
      )
    )
  )

# Checks

ecoactDT_names <- ecoactDT_parents_rec %>%
  dplyr::select(!ends_with("raw"), -NSID) %>%
  names()

ecoactDT_pairs <- tibble(
  y = ecoactDT_names,
  x = str_c(ecoactDT_names, "_raw")
)

make_crosstab <- function(data, x, y) {
  data |>
    count(
      across(all_of(c(x, y))),
      name = "n"
    )
}

ecoactDT_crosstabs <- ecoactDT_pairs |>
  mutate(
    crosstab = map2(
      x,
      y,
      ~ make_crosstab(ecoactDT_parents_rec, .x, .y)
    )
  )

ecoactDT_crosstabs %>%
  pull(crosstab)

# Extract variables
ecoactDT_parents_all <- ecoactDT_parents_rec %>%
  select(NSID, starts_with("ecoactdtma"), starts_with("ecoactdtpa"))

# NS-SEC Own --------------------------------------------------------------------

# Import NS-SEC variables from relevant sweeps
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
nssec_all <- reduce(nssec_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'nssec*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("nssec")
  )

## Fix source variable labels --------------------------------------------------------------------

# This is done for simpler validation.
nssec_s4_s5_s6_s7_missing <- c(
  `YP Not interviewed` = -99,
  `Not applicable` = -91
)

nssec_s8_missing <- c(
  `Refused` = -9,
  `Insufficient information` = -8,
  `Not applicable` = -1
)

nssec_labels_core <- c(
  `Employers in large organisations` = 1,
  `Higher managerial occupations` = 2,
  `Higher professional traditional employee` = 3.1,
  `Higher professional new employee` = 3.2,
  `Higher professional traditional self emp` = 3.3,
  `Higher professional new self emp` = 3.4,
  `Lower professional traditional employee` = 4.1,
  `Lower professional new employee` = 4.2,
  `Lower professional traditional self emp` = 4.3,
  `Lower professional new self emp` = 4.4,
  `Lower managerial occupations` = 5,
  `Higher supervisory occupations` = 6,
  `Intermediate clerical and administrative` = 7.1,
  `Intermediate sales and service` = 7.2,
  `Intermediate technical and auxiliary` = 7.3,
  `Intermediate engineering` = 7.4,
  `Employers in small orgs non-professional` = 8.1,
  `Employers in small orgs agriculture` = 8.2,
  `Own account workers non professional` = 9.1,
  `Own account workers agriculture` = 9.2,
  `Lower supervisory occupations` = 10,
  `Lower technical craft` = 11.1,
  `Lower technical process operative` = 11.2,
  `Semi routine sales` = 12.1,
  `Semi routine services` = 12.2,
  `Semi routine technical` = 12.3,
  `Semi routine operative` = 12.4,
  `Semi routine agricultural` = 12.5,
  `Semi routine clerical` = 12.6,
  `Semi routine childcare` = 12.7,
  `Routine sales and service` = 13.1,
  `Routine production` = 13.2,
  `Routine technical` = 13.3,
  `Routine operative` = 13.4,
  `Routine agricultural` = 13.5,
  `Never worked` = 14.1,
  `Long-term unemployed` = 14.2,
  `Not working` = 14.3,
  `Full-time students` = 15,
  `Not classified or inadequately stated` = 16,
  `Not classifiable for other reasons` = 17
)

# Apply common labels & sweep-specific features
nssec_all <- nssec_all %>%
  mutate(
    # Sweeps with common labels
    across(
      c(nssec17_raw, nssec18_raw, nssec19_raw, nssec20_raw),
      ~ labelled(.x, labels = c(nssec_s4_s5_s6_s7_missing, nssec_labels_core))
    ),
    # S8: Same occupation labels & values, different missing values
    nssec25_raw = labelled(
      nssec25_raw,
      labels = c(nssec_s8_missing, nssec_labels_core)
    )
  )

## Recode --------------------------------------------------------------------

# Harmonise NS-SEC values and derive categories
nssec_rec <- nssec_all %>%
  mutate(
    ## Sweep 4 (age 17)
    nssec17 = case_when(
      is.na(nssec17_raw) ~ -3,
      floor(nssec17_raw) %in% 1:17 ~ floor(nssec17_raw),
      nssec17_raw == -91 ~ -1,
      nssec17_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 5 (age 18)
    nssec18 = case_when(
      is.na(nssec18_raw) ~ -3,
      floor(nssec18_raw) %in% 1:17 ~ floor(nssec18_raw),
      nssec18_raw == -91 ~ -1,
      nssec18_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 6 (age 19)
    nssec19 = case_when(
      is.na(nssec19_raw) ~ -3,
      floor(nssec19_raw) %in% 1:17 ~ floor(nssec19_raw),
      nssec19_raw == -91 ~ -1,
      nssec19_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 7 (age 20)
    nssec20 = case_when(
      is.na(nssec20_raw) ~ -3,
      floor(nssec20_raw) %in% 1:17 ~ floor(nssec20_raw),
      nssec20_raw == -91 ~ -1,
      nssec20_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 8 (age 25)
    nssec25 = case_when(
      is.na(nssec25_raw) ~ -3,
      floor(nssec25_raw) %in% 1:14 ~ floor(nssec25_raw),
      ecoactadu25 == 5 ~ 15, # full-time student
      nssec25_raw == -9 ~ -9,
      nssec25_raw == -8 ~ -8,
      nssec25_raw == -1 ~ -1,
    ),
    ## Sweep 9 (age 32)
    nssec32 = case_when(
      is.na(nssec32_raw) ~ -3,
      nssec32_raw %in% 1:17 ~ nssec32_raw,
      nssec32_raw == -9 ~ -9,
      nssec32_raw == -8 ~ -8,
      nssec32_raw == -1 ~ -1
    )
  ) %>%
  mutate(
    across(
      starts_with("nssec") & !ends_with("raw"),
      ~ labelled(
        .x,
        labels = c(
          "Employers in large organisations" = 1,
          "Higher managerial and administrative occupations" = 2,
          "Higher professional occupations" = 3,
          "Lower professional and higher technical occupations" = 4,
          "Lower managerial and administrative occupations" = 5,
          "Higher supervisory occupations" = 6,
          "Intermediate occupations" = 7,
          "Employers in small establishments" = 8,
          "Own account workers" = 9,
          "Lower supervisory occupations" = 10,
          "Lower technical occupations" = 11,
          "Semi-routine occupations" = 12,
          "Routine occupations" = 13,
          "Never worked and long-term unemployed" = 14,
          "Full-time student" = 15,
          "Not classified or inadequately stated" = 16,
          "Not classifiable for other reasons" = 17,
          common_missing_labels
        )
      )
    )
  )

## Checks --------------------------------------------------------------------

nssec_names <- nssec_rec %>%
  dplyr::select(starts_with("nssec") & !ends_with("raw")) %>%
  names()

nssec_pairs <- tibble(
  y = nssec_names,
  x = str_c(nssec_names, "_raw")
)

nssec_crosstabs <- nssec_pairs |>
  mutate(
    crosstab = map2(
      x,
      y,
      ~ make_crosstab(nssec_rec, .x, .y)
    )
  )

nssec_crosstabs %>%
  pull(crosstab) %>%
  purrr::walk(~ print(.x, n = Inf))

nssec_all <- nssec_rec %>%
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
nssec_parents_all <- reduce(nssec_parents_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'nssec*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("nssec")
  )

# Fix labels
## Missing labels
nssec_parents_labels_missing <- c(
  "Missing - household data lost" = -999,
  "Parent not interviewed" = -99,
  "Parent not present" = -98,
  "Insufficient information" = -94
)

nssec_parents_all <- nssec_parents_all %>%
  mutate(
    # Sweeps with common labels
    across(
      -NSID,
      ~ labelled(
        .x,
        ## Re-using nssec_labels_core as the occupation codes/labels are the same for parent variables as well.
        labels = c(nssec_parents_labels_missing, nssec_labels_core)
      )
    )
  )

# Function to harmonise values (handle decimals, recode missing codes)
recode_nssec_parents <- function(x) {
  case_when(
    floor(x) %in% 1:17 ~ floor(x),
    x == -999 ~ -2,
    x %in% c(-99, -98) | is.na(x) ~ -3,
    x == -94 ~ -8,
    TRUE ~ x
  )
}

# Apply recode and assign to derived variables
nssec_parents_rec <- nssec_parents_all %>%
  mutate(
    nssecma14 = recode_nssec_parents(nssecma14_raw),
    nssecpa14 = recode_nssec_parents(nssecpa14_raw),
    nssecma15 = recode_nssec_parents(nssecma15_raw),
    nssecpa15 = recode_nssec_parents(nssecpa15_raw),
    nssecma16 = recode_nssec_parents(nssecma16_raw),
    nssecpa16 = recode_nssec_parents(nssecpa16_raw),
    nssecma17 = recode_nssec_parents(nssecma17_raw),
    nssecpa17 = recode_nssec_parents(nssecpa17_raw),
    nssecma18 = recode_nssec_parents(nssecma18_raw),
    nssecpa18 = recode_nssec_parents(nssecpa18_raw)
  ) %>%
  mutate(
    across(
      c(starts_with("nssec") & !ends_with("raw")),
      ~ labelled(
        .x,
        labels = c(
          "Employers in large organisations" = 1,
          "Higher managerial and administrative occupations" = 2,
          "Higher professional occupations" = 3,
          "Lower professional and higher technical occupations" = 4,
          "Lower managerial and administrative occupations" = 5,
          "Higher supervisory occupations" = 6,
          "Intermediate occupations" = 7,
          "Employers in small establishments" = 8,
          "Own account workers" = 9,
          "Lower supervisory occupations" = 10,
          "Lower technical occupations" = 11,
          "Semi-routine occupations" = 12,
          "Routine occupations" = 13,
          "Never worked and long-term unemployed" = 14,
          "Full-time student" = 15,
          "Not classified or inadequately stated" = 16,
          "Not classifiable for other reasons" = 17,
          common_missing_labels
        )
      )
    )
  )

# Cross-checks
nssec_parents_names <- nssec_parents_rec %>%
  dplyr::select(starts_with("nssec") & !ends_with("raw")) %>%
  names()

nssec_parents_pairs <- tibble(
  y = nssec_parents_names,
  x = str_c(nssec_parents_names, "_raw")
)

nssec_parents_crosstabs <- nssec_parents_pairs |>
  mutate(
    crosstab = map2(
      x,
      y,
      ~ make_crosstab(nssec_parents_rec, .x, .y)
    )
  )

nssec_parents_crosstabs %>%
  pull(crosstab) %>%
  purrr::walk(~ print(.x, n = Inf))

# Extract derived variables
nssec_parents_all <- nssec_parents_rec %>%
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
    select(
      NSID,
      s5_tenure_type = W5Hous12HH,
      s5_tenure_owned = W5Hous12BHH,
      s5_tenure_rented = W5Hous12CHH
    ),
  S6 = ns_data[["S6youngperson"]] %>%
    select(
      NSID,
      s6_tenure_type = W6Hous12YP,
      s6_tenure_owned = W6Hous12bYP,
      s6_tenure_rented = W6Hous12cYP
    ),
  S7 = ns_data[["S7youngperson"]] %>%
    select(
      NSID,
      s7_tenure_type = W7Hous12YP,
      s7_tenure_owned = W7Hous12bYP,
      s7_tenure_rented = W7Hous12cYP
    ),
  S8 = ns_data[["S8maininterview"]] %>%
    select(NSID, hown25 = W8TENURE),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, hown32 = W9DTENURE)
)

# Merge all datasets
hown_all <- reduce(housing_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'hown*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("hown")
  )

# Helpers --------------------------------------------------------------------

# Note:
## For most sweeps, tenure is derived from a single source variable.
## This is different for sweeps 5-7, where each sweep provides three indicators:
## 1) Indicates whether owned/rented.
## 2) Specifies further tenure if owned (e.g. if owned outright, mortgage, ...).
## 3) Specifies further tenure if rented (e.g. e.g. rented privately, from a council, rent free, ...)

# Helper for detailed tenure
recode_tenure_detailed <- function(tenure_type, tenure_owned, tenure_rented) {
  case_when(
    tenure_owned == 1 ~ 1,
    tenure_owned == 2 ~ 2,
    tenure_owned == 3 ~ 3,
    tenure_rented == 1 ~ 4,
    tenure_rented == 2 ~ 5,
    tenure_rented == 3 ~ 6,
    tenure_rented == 4 ~ 7,
    tenure_type == 3 | tenure_owned == 4 | tenure_rented == 5 ~ 8, # Other
    tenure_owned == -999 | tenure_rented == -999 ~ -2, # Information loss
    tenure_owned == -92 | tenure_rented == -92 ~ -9, # Refusal
    tenure_owned == -91 | tenure_rented == -91 ~ -1, # Not applicable
    tenure_owned == -1 | tenure_rented == -1 ~ -8, # Don't know
    .default = -3 # Anything else -> Not asked or similar
  )
}

# Helper for collapsed tenure
recode_tenure_collapsed <- function(tenure_type, tenure_owned, tenure_rented) {
  dplyr::case_when(
    tenure_owned == 1 ~ 1, # own outright
    tenure_owned == 2 ~ 2, # mortgage/loan
    tenure_owned == 3 ~ 3, # shared ownership
    tenure_rented %in% 1:3 ~ 4, # rent it (any rent source collapsed)
    tenure_rented == 4 ~ 5, # live rent free
    tenure_type == 3 | tenure_owned == 4 | tenure_rented == 5 ~ 6, # other
    tenure_owned == -999 | tenure_rented == -999 ~ -2, # information loss
    tenure_owned == -92 | tenure_rented == -92 ~ -9, # refusal
    tenure_owned == -91 | tenure_rented == -91 ~ -1, # not applicable
    tenure_owned == -1 | tenure_rented == -1 ~ -8, # don't know
    .default = -3
  )
}

## Recode: detailed tenure --------------------------------------------------------------------

hown_rec_detailed <- hown_all %>%
  mutate(
    hownteen14 = case_when(
      hown14_raw > 0 ~ hown14_raw,
      hown14_raw == -999 ~ -2,
      hown14_raw == -92 ~ -9,
      hown14_raw == -91 ~ -1,
      hown14_raw == -1 ~ -8,
      is.na(hown14_raw) ~ -3
    ),
    hownteen15 = case_when(
      hown15_raw > 0 ~ hown15_raw,
      hown15_raw %in% c(-998, -997, -995, -99) ~ -2,
      hown15_raw == -92 ~ -9,
      hown15_raw == -91 ~ -1,
      hown15_raw == -1 ~ -8,
      is.na(hown15_raw) ~ -3
    ),
    hownteen16 = case_when(
      hown16_raw > 0 ~ hown16_raw,
      hown16_raw == -999 ~ -2,
      hown16_raw == -92 ~ -9,
      hown16_raw == -91 ~ -1,
      hown16_raw == -1 ~ -8,
      is.na(hown16_raw) ~ -3
    ),
    hownteen17 = case_when(
      hown17_raw > 0 ~ hown17_raw,
      hown17_raw %in% c(-999, -997) ~ -2,
      hown17_raw == -92 ~ -9,
      hown17_raw == -91 ~ -1,
      hown17_raw == -1 ~ -8,
      is.na(hown17_raw) ~ -3
    ),
    hownteen18 = recode_tenure_detailed(
      tenure_type = s5_tenure_type,
      tenure_owned = s5_tenure_owned,
      tenure_rented = s5_tenure_rented
    ),
    hownteen19 = recode_tenure_detailed(
      tenure_type = s6_tenure_type,
      tenure_owned = s6_tenure_owned,
      tenure_rented = s6_tenure_rented
    ),
    hownteen20 = recode_tenure_detailed(
      tenure_type = s7_tenure_type,
      tenure_owned = s7_tenure_owned,
      tenure_rented = s7_tenure_rented
    )
  )

# Add labels
hown_rec_detailed <- hown_rec_detailed %>%
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
      ~ labelled(
        .x,
        labels = c(
          "Owned outright" = 1,
          "Being bought on a mortgage/bank loan" = 2,
          "Shared ownership (owns & rents property)" = 3,
          "Rented from a Council or New Town" = 4,
          "Rented from a Housing Association" = 5,
          "Rented privately" = 6,
          "Rent free" = 7,
          "Some other arrangement" = 8,
          common_missing_labels
        )
      )
    )
  )

## Recode: simple tenure --------------------------------------------------------------------

hown_rec_simple <- hown_rec_detailed %>%
  mutate(
    hown14 = case_when(
      hown14_raw == 1 ~ 1, # own outright
      hown14_raw == 2 ~ 2, # own, buying with help of mortgage/loan
      hown14_raw == 3 ~ 3, # part rent, part mortgage
      hown14_raw %in% 4:6 ~ 4, # rent it
      hown14_raw == 7 ~ 5, # live-in rent free
      hown14_raw == 8 ~ 6, # other
      hown14_raw == -999 ~ -2,
      hown14_raw == -92 ~ -9,
      hown14_raw == -91 ~ -1,
      hown14_raw == -1 ~ -8,
      is.na(hown14_raw) ~ -3
    ),
    hown15 = case_when(
      hown15_raw == 1 ~ 1,
      hown15_raw == 2 ~ 2,
      hown15_raw == 3 ~ 3,
      hown15_raw %in% 4:6 ~ 4,
      hown15_raw == 7 ~ 5,
      hown15_raw == 8 ~ 6,
      hown15_raw %in% c(-998, -997, -995, -99) ~ -2,
      hown15_raw == -92 ~ -9,
      hown15_raw == -91 ~ -1,
      hown15_raw == -1 ~ -8,
      is.na(hown15_raw) ~ -3
    ),
    hown16 = case_when(
      hown16_raw == 1 ~ 1,
      hown16_raw == 2 ~ 2,
      hown16_raw == 3 ~ 3,
      hown16_raw %in% 4:6 ~ 4,
      hown16_raw == 7 ~ 5,
      hown16_raw == 8 ~ 6,
      hown16_raw == -999 ~ -2,
      hown16_raw == -92 ~ -9,
      hown16_raw == -91 ~ -1,
      hown16_raw == -1 ~ -8,
      is.na(hown16_raw) ~ -3
    ),
    hown17 = case_when(
      hown17_raw == 1 ~ 1,
      hown17_raw == 2 ~ 2,
      hown17_raw == 3 ~ 3,
      hown17_raw %in% 4:6 ~ 4,
      hown17_raw == 7 ~ 5,
      hown17_raw == 8 ~ 6,
      hown17_raw %in% c(-999, -997) ~ -2,
      hown17_raw == -92 ~ -9,
      hown17_raw == -91 ~ -1,
      hown17_raw == -1 ~ -8,
      is.na(hown17_raw) ~ -3
    ),
    hown18 = recode_tenure_collapsed(
      tenure_type = s5_tenure_type,
      tenure_owned = s5_tenure_owned,
      tenure_rented = s5_tenure_rented
    ),
    hown19 = recode_tenure_collapsed(
      tenure_type = s6_tenure_type,
      tenure_owned = s6_tenure_owned,
      tenure_rented = s6_tenure_rented
    ),
    hown20 = recode_tenure_collapsed(
      tenure_type = s7_tenure_type,
      tenure_owned = s7_tenure_owned,
      tenure_rented = s7_tenure_rented
    ),
    hown25 = case_when(
      hown25_raw == 1 ~ 1,
      hown25_raw == 2 ~ 2,
      hown25_raw == 3 ~ 3,
      hown25_raw == 4 ~ 4,
      hown25_raw == 5 ~ 5,
      hown25_raw %in% 6:7 ~ 6,
      hown25_raw == -9 ~ -9,
      hown25_raw == -8 ~ -8,
      hown25_raw == -1 ~ -1,
      is.na(hown25_raw) ~ -3
    ),
    hown32 = case_when(
      hown32_raw == 1 ~ 1,
      hown32_raw == 2 ~ 2,
      hown32_raw == 3 ~ 3,
      hown32_raw == 4 ~ 4,
      hown32_raw == 5 ~ 5,
      hown32_raw %in% 6:7 ~ 6,
      hown32_raw == -9 ~ -9,
      hown32_raw == -8 ~ -8,
      hown32_raw == -1 ~ -1,
      is.na(hown32_raw) ~ -3
    )
  )

# Recode labels
hown_rec_simple <- hown_rec_simple %>%
  mutate(
    across(
      c(hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32),
      ~ labelled(
        .x,
        labels = c(
          "Owned outright" = 1,
          "Owned, buying with help of mortgage/loan" = 2,
          "Part rent, part mortgage" = 3,
          "Rent it" = 4,
          "live rent-free" = 5,
          "Other" = 6,
          common_missing_labels
        )
      )
    )
  )


## Cross-checks for single-variable re-codes --------------------------------------------------------------------

hown_names <- hown_rec_simple %>%
  dplyr::select(starts_with("hown") & !ends_with(c("raw", 18:20))) %>%
  names()

hown_pairs <- tibble(
  y = hown_names,
  x = str_c(hown_names, "_raw")
) %>%
  # 'hownteen' must be cross-tabulated against 'hown_raw'
  mutate(x = str_replace(x, "hownteen", "hown"))

hown_crosstabs <- hown_pairs |>
  mutate(
    crosstab = map2(
      x,
      y,
      ~ make_crosstab(hown_rec_simple, .x, .y)
    )
  )

hown_crosstabs %>%
  pull(crosstab) %>%
  purrr::walk(~ print(.x, n = Inf))

# Extract derived variables

hown_all <- hown_rec_simple %>%
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
    select(NSID, inc25_raw = W8DINCB),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, inc32_raw = W9DINCB)
)

# Merge all income variables by NSID
income_all <- reduce(income_vars, full_join, by = "NSID")

# Recode
income_rec <- income_all %>%
  mutate(
    inc25 = case_when(
      is.na(inc25_raw) ~ -3,
      TRUE ~ inc25_raw
    ),
    inc32 = case_when(
      is.na(inc32_raw) ~ -3,
      TRUE ~ inc32_raw
    )
  ) %>%
  mutate(
    across(
      c(inc25, inc32),
      ~ labelled(
        .x,
        labels = c(
          "less than £25 per week" = 1,
          "25-50" = 2,
          "50-90" = 3,
          "90-140" = 4,
          "140-240" = 5,
          "240-300" = 6,
          "300-350" = 7,
          "350-400" = 8,
          "400-500" = 9,
          "500-600" = 10,
          "600-700" = 11,
          "700-800" = 12,
          "800-900" = 13,
          "900-1200" = 14,
          "1200-1400" = 15,
          "more than 1400" = 16,
          common_missing_labels
        )
      )
    )
  )

# Checks
income_rec %>%
  count(inc25_raw, inc25)

income_rec %>%
  count(inc32_raw, inc32)

# Extract derived variables
income_all <- income_rec %>%
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
hh_income_all <- reduce(hh_income_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'incwhh*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("incwhh")
  )

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

hh_income_rec <- hh_income_all %>%
  mutate(
    # Sweep 1
    incwhh14 = case_when(
      is.na(incwhh14_raw) ~ -3,
      incwhh14_raw %in% c(-92, -992) ~ -9,
      incwhh14_raw %in% c(-999, -94) ~ -2,
      incwhh14_raw == -99 ~ -3,
      incwhh14_raw == -91 ~ -1,
      incwhh14_raw == -1 ~ -8,
      incwhh14_raw == -3 ~ -1,
      TRUE ~ convert_to_band(incwhh14_raw)
    ),
    incwhhcnt14 = case_when(
      is.na(incwhh14_raw) ~ -3,
      incwhh14_raw %in% c(-92, -992) ~ -9,
      incwhh14_raw %in% c(-999, -94) ~ -2,
      incwhh14_raw == -99 ~ -3,
      incwhh14_raw == -91 ~ -1,
      incwhh14_raw == -1 ~ -8,
      incwhh14_raw == -3 ~ -1,
      TRUE ~ incwhh14_raw
    ),

    # Sweep 2
    incwhh15 = case_when(
      is.na(incwhh15_raw) ~ -3,
      incwhh15_raw %in% c(-92, -992) ~ -9,
      incwhh15_raw == -999 ~ -2,
      incwhh15_raw == -99 ~ -3,
      incwhh15_raw == -91 ~ -1,
      incwhh15_raw %in% c(-1, -94) ~ -8,
      incwhh15_raw == -3 ~ -1,
      TRUE ~ convert_to_band(incwhh15_raw)
    ),
    incwhhcnt15 = case_when(
      is.na(incwhh15_raw) ~ -3,
      incwhh15_raw %in% c(-92, -992) ~ -9,
      incwhh15_raw == -999 ~ -2,
      incwhh15_raw == -99 ~ -3,
      incwhh15_raw == -91 ~ -1,
      incwhh15_raw %in% c(-1, -94) ~ -8,
      incwhh15_raw == -3 ~ -1,
      TRUE ~ incwhh15_raw
    ),

    # Sweep 3
    incwhh16 = case_when(
      is.na(incwhh16_raw) ~ -3,
      incwhh16_raw == -99 ~ -3,
      incwhh16_raw == -92 ~ -9,
      incwhh16_raw == -1 ~ -8,
      incwhh16_raw >= 1 & incwhh16_raw <= 12 ~ incwhh16_raw
    ),

    # Sweep 4
    incwhh17 = case_when(
      is.na(incwhh17_raw) ~ -3,
      incwhh17_raw %in% c(-996, -99) ~ -3,
      incwhh17_raw == -92 ~ -9,
      incwhh17_raw == -1 ~ -8,
      incwhh17_raw >= 1 & incwhh17_raw <= 12 ~ incwhh17_raw
    )
  ) %>%
  mutate(
    across(
      c(incwhh14, incwhh15),
      ~ labelled(
        .x,
        labels = c(
          "less than £25 per week" = 1,
          "25-50" = 2,
          "50-90" = 3,
          "90-140" = 4,
          "140-240" = 5,
          "240-300" = 6,
          "300-350" = 7,
          "350-400" = 8,
          "400-500" = 9,
          "500-600" = 10,
          "600-700" = 11,
          "700-800" = 12,
          "800-900" = 13,
          "900-1200" = 14,
          "1200-1400" = 15,
          "more than 1400" = 16,
          common_missing_labels
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
      ~ labelled(
        .x,
        labels = c(
          "up to 49" = 1,
          "50-99" = 2,
          "100-199" = 3,
          "200-299" = 4,
          "300-399" = 5,
          "400-499" = 6,
          "500-599" = 7,
          "600-699" = 8,
          "700-799" = 9,
          "800-899" = 10,
          "900-999" = 11,
          "1000 or more" = 12,
          common_missing_labels
        )
      )
    )
  )

hh_income_all <- hh_income_rec %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# IMD --------------------------------------------------------------------
# Load IMD variables from relevant sweeps
imd_vars <- list(
  S1 = ns_data[["S1youngperson"]] %>% select(NSID),
  S4 = ns_data[["S4youngperson"]] %>% select(NSID),
  S2 = ns_data[["S2familybackground"]] %>%
    select(NSID, imd15_raw = IMDRSCORE),
  S3 = ns_data[["S3familybackground"]] %>%
    select(NSID, imd16_raw = IMDRSCORE),
  S9 = ns_data[["S9derivedvariable"]] %>%
    select(NSID, imd32_raw = W9DIMDD)
)

# Merge all IMD variables by NSID
imd_all <- reduce(imd_vars, full_join, by = "NSID")

# Recode derived variables
imd_rec <- imd_all %>%
  mutate(
    imd15 = case_when(
      is.na(imd15_raw) ~ -3,
      imd15_raw == -94 ~ -8,
      TRUE ~ imd15_raw
    ),

    imd16 = case_when(
      is.na(imd16_raw) ~ -3,
      imd16_raw == -94 ~ -8,
      TRUE ~ imd16_raw
    ),

    imd32 = case_when(
      is.na(imd32_raw) ~ -3,
      imd32_raw == -8 ~ -8,
      TRUE ~ imd32_raw
    )
  ) %>%
  mutate(
    across(
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
    )
  )

imd_rec %>%
  count(imd15_raw, imd15)

imd_rec %>%
  count(imd16_raw, imd16)

imd_rec %>%
  count(imd32_raw, imd32)

imd_all <- imd_rec %>%
  select(NSID, imd15, imd16, imd32)
