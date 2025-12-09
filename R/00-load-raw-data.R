# Packages
library(haven) # for reading Stata files
library(dplyr) # for data manipulation
library(purrr) # for functional programming (map, reduce)
library(here) # for file paths
library(labelled) # for handling labelled data

# Set folder path
data_path <- here(
  "data",
  "UKDA-5545-stata",
  "stata",
  "stata13",
  "safeguarded_eul"
)

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
  longitudinal = "ns9_2022_longitudinal_file.dta"
)

# Load all datasets
ns_data <- map(sweeps, ~ read_dta(file.path(data_path, .x)))

# Missing value labels (re-used across all variables & scripts)
common_missing_labels <- c(
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/did not participate at specific wave/was not surveyed" = -3L,
  "Data not available" = -5L,
  "Prefer not to say" = -7L,
  "Don’t know/insufficient information" = -8L,
  "Refusal" = -9L
)

# Re-coding via look up tables
val_table <- function(x) {
  labs <- labelled::val_labels(x)

  tibble(
    old_value = unname(labs),
    old_label = names(labs)
  )
}

recode_labelled_by_labels <- function(x,
                                      lookup,
                                      unmatched = c("NA", "keep"),
                                      includes = FALSE) {
  unmatched <- match.arg(unmatched)

  # basic checks for lookup structure
  needed_cols <- c("old_label", "new_value", "new_label")
  missing_cols <- setdiff(needed_cols, names(lookup))
  if (length(missing_cols) > 0) {
    stop(
      "lookup is missing these columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  labs <- labelled::val_labels(x)

  label_df <-
    tibble::tibble(
      old_label = names(labs),
      old_value = unname(labs)
    )

  # build recode_spec: link lookup$old_label to numeric codes in x
  if (isFALSE(includes)) {
    # exact matching (original behaviour)
    recode_spec <-
      lookup |>
      dplyr::left_join(label_df, by = "old_label")
  } else {
    # "includes" matching: lookup$old_label is treated as a substring
    match_idx <-
      purrr::map_int(
        lookup$old_label,
        \(pat) {
          if (is.na(pat) || pat == "") {
            return(NA_integer_)
          }

          hits <- which(grepl(pat, label_df$old_label, ignore.case = TRUE))

          if (length(hits) == 0L) {
            NA_integer_
          } else if (length(hits) == 1L) {
            hits
          } else {
            stop(
              "Pattern '", pat,
              "' matched multiple labels:\n  ",
              paste(label_df$old_label[hits], collapse = "\n  "),
              "\nPlease make the pattern more specific.",
              call. = FALSE
            )
          }
        }
      )

    recode_spec <-
      lookup |>
      dplyr::mutate(
        old_value = label_df$old_value[match_idx]
      )
  }

  # warn and drop any lookup rows that did not match a label on x
  missing_rows <- is.na(recode_spec$old_value)
  if (any(missing_rows)) {
    missing_labels <- recode_spec$old_label[missing_rows]
    warning(
      "These old_label values were not found and will be ignored: ",
      paste(unique(missing_labels), collapse = ", "),
      call. = FALSE
    )

    recode_spec <- recode_spec[!missing_rows, , drop = FALSE]
  }

  # mapping from old numeric values -> new numeric values
  recode_map <- recode_spec$new_value
  names(recode_map) <- as.character(recode_spec$old_value)

  x_num <- as.numeric(x)

  # recode each element
  x_new <-
    x_num |>
    purrr::map_dbl(
      \(v) {
        if (is.na(v)) {
          return(NA_real_)
        }

        key <- as.character(v)

        if (key %in% names(recode_map)) {
          recode_map[[key]]   # can itself be NA
        } else if (unmatched == "keep") {
          v
        } else {
          NA_real_
        }
      }
    )

  # build new value labels from lookup (only matched rows)
  label_rows <-
    recode_spec |>
    dplyr::filter(!is.na(new_value), !is.na(new_label)) |>
    dplyr::distinct(new_value, .keep_all = TRUE)

  new_labels <- label_rows$new_value
  names(new_labels) <- label_rows$new_label

  x_new <- labelled::labelled(
    x_new,
    labels = new_labels
  )

  labelled::var_label(x_new) <- labelled::var_label(x)

  x_new
}

