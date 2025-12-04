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
