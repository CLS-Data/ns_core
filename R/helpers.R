# Recode a labelled variable by labels --------------------------------------------------------------------

# - x:            a labelled vector (e.g. from Stata via haven)
# - lookup:       tibble/data frame with columns
#                 old_label = (parts of) the original value labels
#                 new_value = numeric codes to recode to
#                 new_label = labels for the new_value codes
# - unmatched:    what to do with values not mentioned in lookup:
#                 "keep" = keep their original numeric values
#                 "NA"   = set them to NA
# - includes:     if FALSE, old_label in lookup must match labels exactly;
#                 if TRUE, old_label is treated as a substring to search for
# - na_to:        what to recode true NA values in x to (default: stay NA)
# - na_label:     optional label for na_to in the output value labels
recode_labelled_by_labels <- function(
  x,
  lookup,
  unmatched = c("NA", "keep"),
  includes = FALSE,
  na_to = NA_real_,
  na_label = NULL
) {
  # Make sure unmatched is one of the allowed options
  unmatched <- match.arg(unmatched)

  # basic checks for lookup structure
  # Ensure lookup has exactly the columns we expect to work with
  needed_cols <- c("old_label", "new_value", "new_label")
  missing_cols <- setdiff(needed_cols, names(lookup))
  if (length(missing_cols) > 0) {
    stop(
      "lookup is missing these columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  # Extract value labels from the labelled vector
  # labs is a named vector: names = label text, values = numeric codes
  labs <- labelled::val_labels(x)

  # Turn labels into a tibble so we can join/match against the lookup
  label_df <-
    tibble::tibble(
      old_label = names(labs),
      old_value = unname(labs)
    )

  # exact vs "includes" matching for old_label
  if (isFALSE(includes)) {
    # If includes = FALSE (default):
    # we do a straightforward join by old_label text
    recode_spec <-
      lookup |>
      dplyr::left_join(label_df, by = "old_label")
  } else {
    # If includes = TRUE:
    # we treat each old_label in lookup as a pattern to search for
    # inside the labels of x (case-insensitive "contains").
    # This allows writing e.g. "Married" instead of the full long label.
    match_idx <-
      purrr::map_int(
        lookup$old_label,
        \(pat) {
          # Empty or NA patterns cannot match anything
          if (is.na(pat) || pat == "") {
            return(NA_integer_)
          }
          # Find all labels that contain the pattern (case-insensitive)
          hits <- which(grepl(pat, label_df$old_label, ignore.case = TRUE))

          if (length(hits) == 0L) {
            # No match found: mark as NA (will warn and drop later)
            NA_integer_
          } else if (length(hits) == 1L) {
            # Exactly one match: this is what we want
            hits
          } else {
            # Pattern is ambiguous: better to stop than silently pick one
            stop(
              "Pattern '",
              pat,
              "' matched multiple labels:\n  ",
              paste(label_df$old_label[hits], collapse = "\n  "),
              "\nPlease make the pattern more specific.",
              call. = FALSE
            )
          }
        }
      )
    # Attach the matched old_value (numeric code) to each lookup row
    recode_spec <-
      lookup |>
      dplyr::mutate(
        old_value = label_df$old_value[match_idx]
      )
  }

  # warn and drop lookup rows that did not match a label on x
  # (e.g. typos in lookup$old_label, or patterns that matched nothing)
  missing_rows <- is.na(recode_spec$old_value)
  if (any(missing_rows)) {
    missing_labels <- recode_spec$old_label[missing_rows]
    warning(
      "These old_label values were not found and will be ignored: ",
      paste(unique(missing_labels), collapse = ", "),
      call. = FALSE
    )

    # mapping from old numeric values -> new numeric values
    # names(recode_map) are character versions of the old numeric codes
    # so we can quickly look up the recode based on the underlying value
    recode_spec <- recode_spec[!missing_rows, , drop = FALSE]
  }

  # mapping from old numeric values -> new numeric values
  recode_map <- recode_spec$new_value
  names(recode_map) <- as.character(recode_spec$old_value)

  # Extract the underlying numeric values from the labelled vector
  x_num <- as.numeric(x)

  # Recode each numeric value using the mapping and options
  x_new <-
    x_num |>
    purrr::map_dbl(
      \(v) {
        # handle true NA in x here
        # these are missing values that had no label at all
        if (is.na(v)) {
          return(na_to)
        }

        # Convert value to character to match names(recode_map)
        key <- as.character(v)
        # If this code is mentioned in the recode_map,
        # return the corresponding new_value (which can itself be NA)
        if (key %in% names(recode_map)) {
          recode_map[[key]] # can itself be NA
        } else if (unmatched == "keep") {
          # If unmatched = "keep", we leave codes not mentioned in lookup
          # as their original numeric values
          v
        } else {
          # Otherwise we set them to NA_real_
          NA_real_
        }
      }
    )

  # build new value labels from the matched spec
  label_rows <-
    recode_spec |>
    dplyr::filter(!is.na(new_value), !is.na(new_label)) |>
    dplyr::distinct(new_value, .keep_all = TRUE)

  new_labels <- label_rows$new_value
  names(new_labels) <- label_rows$new_label

  # optionally add a label for 'na_to' (if not NA and label supplied)
  if (!is.na(na_to) && !is.null(na_label)) {
    idx <- which(new_labels == na_to)

    if (length(idx) == 0L) {
      extra <- na_to
      names(extra) <- na_label
      new_labels <- c(new_labels, extra)
    } else {
      names(new_labels)[idx] <- na_label
    }
  }

  x_new <- labelled::labelled(
    x_new,
    labels = new_labels
  )

  labelled::var_label(x_new) <- labelled::var_label(x)

  x_new
}

# Checks --------------------------------------------------------------------

# Check batch-recoded variables against original variable labels
# Safely extract a variable-level label (if present) from a vector.
# - x: a vector that may have a "label" attribute (from labelled/haven)
# Returns a length-1 character vector; NA_character_ if no label.
get_var_label <- function(x) {
  lab <- attr(x, "label")
  if (is.null(lab)) NA_character_ else as.character(lab)
}

# Build a tibble mapping each named "group" to the variables it contains,
# and attach the human-readable variable label for each variable from `data`.
# - data: a data.frame/tibble holding the variables referenced in `groups`
# - groups: a named list where each element name is a group, and the value
#           is a character vector of column names in `data`
# Returns a tibble with columns:
#   group: group name (list element name)
#   var:   variable name (column in `data`)
#   label: variable label (or NA if absent)
make_group_map <- function(data, groups) {
  dplyr::bind_rows(lapply(names(groups), function(g) {
    tibble::tibble(
      group = g,
      var = groups[[g]]
    )
  })) |>
    dplyr::mutate(
      # Look up each variable's label in `data`; uses NA if no label
      label = vapply(
        var,
        \(v) get_var_label(data[[v]]),
        character(1)
      )
    )
}

# Example:
# s8_groups <- list(
#  nvq45 = s8_vars_nvq45,
#  nvq13 = s8_vars_nvq13,
#  entry_none = s8_vars_entry_none,
#  other = s8_vars_other,
#  none_of_these = s8_vars_none_of_these,
#  dk = s8_vars_dk,
#  refusal = s8_vars_refusal
# )
#
# s8_map <- make_group_map(ns_data[["S8maininterview"]], s8_groups)

# Make crosstabs of counts for two variables (x by y).
# - data: tibble/data.frame containing columns `x` and `y`
# - x, y: character scalars naming columns in `data`
# Returns a tibble with columns x, y, and n = count of rows for each combination.
# Notes:
# - Uses dplyr::count with across(all_of(...)) to evaluate by name.
# - NA values are counted in their own cells (default dplyr behavior).
make_crosstab <- function(data, x, y) {
  data |>
    count(
      across(all_of(c(x, y))),
      name = "n"
    )
}
