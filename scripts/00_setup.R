# ============================================================
# 00_setup.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Define project structure, package checks, shared paths,
#          and general helper functions for the replication pipeline
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Project root
# ------------------------------------------------------------

# This script assumes that it is run from the root of the GitHub repository.
#
# Expected repository structure:
#
# economic-security-mental-health-chile/
#   scripts/
#   data_raw/
#   data_processed/
#   output/
#   docs/
#
# Raw data are not included in the repository. Users should download the
# official EBS 2023 data and place the required file in data_raw/.

project_dir <- getwd()

message("Project directory: ", project_dir)


# ------------------------------------------------------------
# 02. Required packages
# ------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "haven",
  "labelled",
  "janitor",
  "openxlsx",
  "stringr",
  "purrr",
  "forcats",
  "survey",
  "broom",
  "scales"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "The following required packages are missing: ",
      paste(missing_packages, collapse = ", "),
      ".\nPlease install them manually or run renv::restore() if using renv."
    )
  )
}

invisible(
  lapply(required_packages, library, character.only = TRUE)
)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)


# ------------------------------------------------------------
# 03. Folder structure
# ------------------------------------------------------------

data_raw_dir        <- file.path(project_dir, "data_raw")
data_processed_dir  <- file.path(project_dir, "data_processed")
output_dir          <- file.path(project_dir, "output")
docs_dir            <- file.path(project_dir, "docs")

output_tables_dir   <- file.path(output_dir, "tables")
output_figures_dir  <- file.path(output_dir, "figures")
output_csv_dir      <- file.path(output_dir, "csv")
output_rds_dir      <- file.path(output_dir, "rds")
output_logs_dir     <- file.path(output_dir, "logs")

dirs_to_create <- c(
  data_raw_dir,
  data_processed_dir,
  output_dir,
  docs_dir,
  output_tables_dir,
  output_figures_dir,
  output_csv_dir,
  output_rds_dir,
  output_logs_dir
)

invisible(
  purrr::walk(
    dirs_to_create,
    ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
  )
)


# ------------------------------------------------------------
# 04. Expected raw data file
# ------------------------------------------------------------

# Expected raw data file:
#
#   data_raw/Base de datos EBS 2023.RData
#
# The file is not included in this repository. Users should download the
# original EBS 2023 data from the official Observatorio Social source.
#
# If the raw data file has a different local name, either rename it to match
# the expected file name or edit `raw_data_file` below.

raw_data_file <- file.path(data_raw_dir, "Base de datos EBS 2023.RData")

if (!file.exists(raw_data_file)) {
  warning(
    paste0(
      "Raw data file not found: ", raw_data_file, "\n",
      "Download the EBS 2023 data from the official source and place it in data_raw/."
    )
  )
}


# ------------------------------------------------------------
# 05. General helper functions
# ------------------------------------------------------------

safe_label <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.null(lab)) {
    return(NA_character_)
  }
  as.character(lab)
}

safe_class <- function(x) {
  paste(class(x), collapse = ", ")
}

n_unique_safe <- function(x) {
  dplyr::n_distinct(x, na.rm = TRUE)
}

is_labelled_safe <- function(x) {
  inherits(x, "haven_labelled") || inherits(x, "labelled")
}

get_value_labels <- function(x) {
  labs <- tryCatch(
    labelled::val_labels(x),
    error = function(e) NULL
  )
  
  if (is.null(labs) || length(labs) == 0) {
    return(
      tibble::tibble(
        value = NA_character_,
        value_label = NA_character_
      )
    )
  }
  
  tibble::tibble(
    value = as.character(unname(labs)),
    value_label = names(labs)
  )
}

as_plain_for_table <- function(x) {
  if (is_labelled_safe(x)) {
    return(as.character(haven::as_factor(x, levels = "default")))
  }
  
  if (is.factor(x)) {
    return(as.character(x))
  }
  
  as.character(x)
}

weighted_mean_approx <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  
  if (sum(valid) == 0) {
    return(NA_real_)
  }
  
  stats::weighted.mean(x[valid], w[valid], na.rm = TRUE)
}

weighted_sd_approx <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  
  if (sum(valid) <= 1) {
    return(NA_real_)
  }
  
  x_valid <- x[valid]
  w_valid <- w[valid]
  
  mean_x <- stats::weighted.mean(x_valid, w_valid, na.rm = TRUE)
  
  sqrt(
    stats::weighted.mean(
      (x_valid - mean_x)^2,
      w_valid,
      na.rm = TRUE
    )
  )
}

weighted_frequency <- function(data, variable, weight_var = NULL, max_levels = 60) {
  if (!variable %in% names(data)) {
    return(tibble::tibble())
  }
  
  x <- data[[variable]]
  x_plain <- as_plain_for_table(x)
  
  temp <- tibble::tibble(
    value = dplyr::if_else(is.na(x_plain), "(Missing)", x_plain),
    weight = if (!is.null(weight_var) && weight_var %in% names(data)) {
      suppressWarnings(as.numeric(data[[weight_var]]))
    } else {
      rep(1, nrow(data))
    }
  )
  
  out <- temp %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      unweighted_pct = unweighted_n / sum(unweighted_n, na.rm = TRUE) * 100,
      weighted_pct = weighted_n / sum(weighted_n, na.rm = TRUE) * 100
    ) %>%
    dplyr::arrange(dplyr::desc(weighted_n)) %>%
    dplyr::mutate(variable = variable, .before = 1)
  
  if (nrow(out) > max_levels) {
    out <- out %>%
      dplyr::slice_head(n = max_levels) %>%
      dplyr::mutate(note = paste0("Top ", max_levels, " categories only"))
  } else {
    out <- out %>%
      dplyr::mutate(note = NA_character_)
  }
  
  out
}

numeric_summary <- function(data, variable, weight_var = NULL) {
  if (!variable %in% names(data)) {
    return(tibble::tibble())
  }
  
  x <- suppressWarnings(as.numeric(data[[variable]]))
  
  if (all(is.na(x))) {
    return(tibble::tibble())
  }
  
  w <- if (!is.null(weight_var) && weight_var %in% names(data)) {
    suppressWarnings(as.numeric(data[[weight_var]]))
  } else {
    rep(1, length(x))
  }
  
  valid <- !is.na(x) & !is.na(w)
  
  if (sum(valid) == 0) {
    return(tibble::tibble())
  }
  
  tibble::tibble(
    variable = variable,
    unweighted_n_valid = sum(valid),
    unweighted_n_missing = sum(is.na(x)),
    pct_missing = mean(is.na(x)) * 100,
    mean_unweighted = mean(x, na.rm = TRUE),
    sd_unweighted = stats::sd(x, na.rm = TRUE),
    p10 = as.numeric(stats::quantile(x, 0.10, na.rm = TRUE, names = FALSE)),
    p25 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
    median = as.numeric(stats::quantile(x, 0.50, na.rm = TRUE, names = FALSE)),
    p75 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
    p90 = as.numeric(stats::quantile(x, 0.90, na.rm = TRUE, names = FALSE)),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    weighted_mean = weighted_mean_approx(x, w),
    weighted_sd = weighted_sd_approx(x, w)
  )
}

find_variables_by_keywords <- function(data, keywords) {
  tibble::tibble(variable = names(data)) %>%
    dplyr::mutate(variable_lower = stringr::str_to_lower(variable)) %>%
    dplyr::filter(
      purrr::map_lgl(
        variable_lower,
        ~ any(stringr::str_detect(.x, stringr::str_to_lower(keywords)))
      )
    ) %>%
    dplyr::select(variable)
}


# ------------------------------------------------------------
# 06. Survey-design helper
# ------------------------------------------------------------

make_survey_design <- function(data,
                               weight_var = "fexp",
                               strata_var = "estrato_ebs",
                               psu_var = "varunit") {
  
  required_design_vars <- c(weight_var, strata_var, psu_var)
  
  missing_design_vars <- required_design_vars[
    !required_design_vars %in% names(data)
  ]
  
  if (length(missing_design_vars) > 0) {
    stop(
      paste0(
        "The following survey design variables are missing: ",
        paste(missing_design_vars, collapse = ", ")
      )
    )
  }
  
  survey::svydesign(
    ids = stats::as.formula(paste0("~", psu_var)),
    strata = stats::as.formula(paste0("~", strata_var)),
    weights = stats::as.formula(paste0("~", weight_var)),
    data = data,
    nest = TRUE
  )
}


# ------------------------------------------------------------
# 07. Save setup metadata and session information
# ------------------------------------------------------------

setup_metadata <- list(
  project_dir = project_dir,
  data_raw_dir = data_raw_dir,
  data_processed_dir = data_processed_dir,
  output_dir = output_dir,
  docs_dir = docs_dir,
  output_tables_dir = output_tables_dir,
  output_figures_dir = output_figures_dir,
  output_csv_dir = output_csv_dir,
  output_rds_dir = output_rds_dir,
  output_logs_dir = output_logs_dir,
  raw_data_file = raw_data_file,
  required_packages = required_packages,
  setup_time = Sys.time()
)

saveRDS(
  setup_metadata,
  file.path(output_rds_dir, "00_setup_metadata.rds")
)

writeLines(
  capture.output(sessionInfo()),
  con = file.path(output_logs_dir, "00_session_info.txt")
)


# ------------------------------------------------------------
# 08. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("00_setup.R completed")
message("============================================================")
message("Project directory: ", project_dir)
message("Raw data directory: ", data_raw_dir)
message("Processed data directory: ", data_processed_dir)
message("Output directory: ", output_dir)
message("Raw data file expected: ", raw_data_file)
message("Raw data file exists: ", file.exists(raw_data_file))
message("Required packages loaded: ", paste(required_packages, collapse = ", "))
message("============================================================\n")
