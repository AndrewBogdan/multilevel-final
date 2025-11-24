# Data Pliers; we use it for %>% and many other things
# see https://dplyr.tidyverse.org/authors.html for citation
library(dplyr)

# Haven; data interoperability, especially labelled data.
# see https://haven.tidyverse.org/authors.html for citation
# library(haven)

# Load the data
# This data was already loaded & saved from the IPUMS website.
# It's a different script to use their raw data.
data <- readRDS("data/ipums 2025-11-20.rds")

# --- Rename Columns -----------------------------------------------------------
# Use this pattern /  ([a-z_\d]+) = any_of\("([A-Z\d]+)"\)/ to do manipulations
#  on this block. I'm using this as a source-of-truth.
# Manual renames; I'm using tidyselect::any_of to make this idempotent
data <- data %>% rename(
  # Outcome (Primary)
  job_satis = any_of("JOBSATIS"),

  # Groupers
  person_id = any_of("PERSONID"),
  survey_year = any_of("YEAR"),

  # Education Covariates
  mismatch = any_of("OCEDRLP"),
  highest_degree = any_of("DGRDG"),
  recent_degree = any_of("MRDG"),
  major = any_of("NDGMED"),
  major_group = any_of("NDGMEMG"),

  # Employment Covariates
  salary = any_of("SALARY"),
  employment_status = any_of("LFSTAT"),
  weekly_hours = any_of("HRSWKGR"),
  job_type = any_of("NOCPR"),
  job_type_group = any_of("NOCPRMG"),
  technical_natural_science = any_of("MGRNAT"),
  technical_other = any_of("MGROTH"),
  technical_social_science = any_of("MGRSOC"),

  # Demographic Covariates
  gender = any_of("GENDER"),
  age = any_of("AGE"),
  birth_year = any_of("BIRYR"),
  minority = any_of("MINRTY"),
  race = any_of("RACETH"),

  # Reasoning Covariates
  reason_1_for_mismatch = any_of("NRREA"),
  reason_2_for_mismatch = any_of("NRSEC"),

  # Other Outcomes
  satisfied_advancement = any_of("SATADV"),
  satisfied_benefits = any_of("SATBEN"),
  satisfied_challenge = any_of("SATCHAL"),
  satisfied_independence = any_of("SATIND"),
  satisfied_location = any_of("SATLOC"),
  satisfied_responsibility = any_of("SATRESP"),
  satisfied_salary = any_of("SATSAL"),
  satisfied_security = any_of("SATSEC"),
  satisfied_contribution = any_of("SATSOC"),

  # Meta
  cpi_2009_conversion = any_of("CPI2009C"),

  # Redundant indicator variables
  reason_career_change = any_of("NRCHG"),
  reason_conditions = any_of("NRCON"),
  reason_family = any_of("NRFAM"),
  reason_location = any_of("NRLOC"),
  reason_job_unavailable = any_of("NROCNA"),
  reason_pay = any_of("NRPAY"),
  reason_other = any_of("NROT"),

  # Unused
  ipums_weight = any_of("WEIGHT"),
  ipums_cohort = any_of("COHORT"),
  survey_mode = any_of("SRVMODE"),
  survey_id = any_of("SAMPLE"),
  reference_id = any_of("REFID"),
  survey = any_of("SURID"),
  ipums_seqnum = any_of("SEQNUM")
)

# --- Reorder Columns ----------------------------------------------------------
# This is the same order as above; use regex to make this block.
data <- data %>% relocate(
  # Outcome (Primary)
  job_satis,

  # Groupers
  person_id,
  survey_year,

  # Education Covariates
  mismatch,
  highest_degree,
  recent_degree,
  major,
  major_group,

  # Employment Covariates
  salary,
  employment_status,
  weekly_hours,
  job_type,
  job_type_group,
  technical_natural_science,
  technical_other,
  technical_social_science,

  # Demographic Covariates
  gender,
  age,
  birth_year,
  minority,
  race,

  # Reasoning Covariates
  reason_1_for_mismatch,
  reason_2_for_mismatch,

  # Other Outcomes
  satisfied_advancement,
  satisfied_benefits,
  satisfied_challenge,
  satisfied_independence,
  satisfied_location,
  satisfied_responsibility,
  satisfied_salary,
  satisfied_security,
  satisfied_contribution,

  # Meta
  cpi_2009_conversion,

  # Redundant indicator variables
  reason_career_change,
  reason_conditions,
  reason_family,
  reason_location,
  reason_job_unavailable,
  reason_pay,
  reason_other,

  # Unused
  ipums_weight,
  ipums_cohort,
  survey_mode,
  survey_id,
  reference_id,
  survey,
  ipums_seqnum
)


# --- Null Missing Data --------------------------------------------------------

#' In a haven-labelled column, replace the values with the supplied label with
#' a tagged NA.
null_missing_codes <- function(column, label, tag) {
  # Find the bad rows
  mask <- haven::as_factor(column) == label

  # Do a quick sanity check
  # This ruins the idempotence of the operation
  # stopifnot(any(mask))

  # Here I change the labels to keep legibility
  # This has to go after we make the mask
  attr(column, "labels")[label] <- haven::tagged_na(tag)

  # This won't work without the haven and dplyr functions, so I was explicit
  dplyr::if_else(mask, haven::tagged_na(tag), column)
}

# Replace "logical skip", "missing", "blank", etc. with NA
# Any column which is listed here has been inspected by me (Andrew)
# haven::zap_missing doesn't work here because we're not using tagged NAs.
data <- data %>% mutate(
  # Outcome
  job_satis = null_missing_codes(job_satis, label = "Logical Skip", tag = "s"),
  # Main Covariate
  mismatch = null_missing_codes(mismatch, label = "Logical Skip", tag = "s"),
  # Other Covariates
  salary = null_missing_codes(salary, label = "Logical Skip", tag = "s"),
  weekly_hours = null_missing_codes(weekly_hours, label = "Logical Skip", tag = "s"),
  job_type = null_missing_codes(job_type, label = "Logical Skip", tag = "s"),
  job_type_group = null_missing_codes(job_type_group, label = "Logical Skip", tag = "s"),

  # It loses the distinction between the different types of NA here, but I can't
  #  spend time debugging it now.
  technical_natural_science = technical_natural_science %>%
    null_missing_codes(label = "Logical Skip", tag = "s") %>%
    null_missing_codes(label = "Missing", tag = ""),
  technical_other = technical_other %>%
    null_missing_codes(label = "Logical Skip", tag = "s") %>%
    null_missing_codes(label = "Missing", tag = ""),
  technical_social_science = technical_social_science %>%
    null_missing_codes(label = "Logical Skip", tag = "s") %>%
    null_missing_codes(label = "Missing", tag = ""),
  reason_1_for_mismatch = null_missing_codes(reason_1_for_mismatch, label = "Logical Skip", tag = "s"),
  reason_2_for_mismatch = null_missing_codes(reason_2_for_mismatch, label = "Logical Skip", tag = "s")
)


# --- Write --------------------------------------------------------------------
saveRDS(data, "data/clean_data.rds")
