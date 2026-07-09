# ============================================================
# 06_configurational_diagnostics_expected_residual.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Build transparent configurational diagnostics from
#          objective-position expected distress and residual PHQ-4,
#          then describe subjective economic-security appraisals
#          within each configuration
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Setup
# ------------------------------------------------------------

source("scripts/00_setup.R")

options(survey.lonely.psu = "adjust")

script_name <- "06_configurational_diagnostics_expected_residual"

script_output_dir  <- file.path(output_dir, script_name)
script_tables_dir  <- file.path(script_output_dir, "tables")
script_figures_dir <- file.path(script_output_dir, "figures")
script_csv_dir     <- file.path(script_output_dir, "csv")
script_rds_dir     <- file.path(script_output_dir, "rds")
script_logs_dir    <- file.path(script_output_dir, "logs")

dirs_to_create <- c(
  script_output_dir,
  script_tables_dir,
  script_figures_dir,
  script_csv_dir,
  script_rds_dir,
  script_logs_dir
)

invisible(
  purrr::walk(
    dirs_to_create,
    ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
  )
)

message("Running: ", script_name)
message("Output folder: ", script_output_dir)


# ------------------------------------------------------------
# 02. Load dataset from script 05
# ------------------------------------------------------------

input_rds <- file.path(
  output_dir,
  "05_within_gradient_heterogeneity",
  "rds",
  "05_within_gradient_dataset.rds"
)

if (!file.exists(input_rds)) {
  stop(
    paste0(
      "Input dataset not found: ", input_rds, "\n",
      "Run scripts/05_within_gradient_heterogeneity.R first."
    )
  )
}

analytic <- readRDS(input_rds) %>%
  janitor::clean_names()

required_design_vars <- c("weight", "strata", "psu")
missing_design_vars <- setdiff(required_design_vars, names(analytic))

if (length(missing_design_vars) > 0) {
  stop(
    "Missing survey design variables: ",
    paste(missing_design_vars, collapse = ", ")
  )
}

analytic <- analytic %>%
  dplyr::mutate(
    weight = as.numeric(weight),
    strata = as.factor(strata),
    psu = as.factor(psu)
  )


# ------------------------------------------------------------
# 03. Required variables
# ------------------------------------------------------------

required_configuration_vars <- c(
  "phq4_score",
  "phq4_expected_objective",
  "phq4_residual_objective",
  "phq4_residual_objective_z",
  "phq4_expected_tercile_f",
  "phq4_residual_position_f",
  "higher_than_expected",
  "lower_than_expected",
  "subjective_insecurity_level_f",
  "prospective_insecurity_level_f",
  "current_financial_strain_level_f",
  "health_financial_protection_3cat_f",
  "no_emergency_money_support_f",
  "employment_risk_position_f",
  "debt_status_f",
  "making_ends_meet_3cat_f",
  "weight",
  "strata",
  "psu"
)

missing_configuration_vars <- setdiff(required_configuration_vars, names(analytic))

if (length(missing_configuration_vars) > 0) {
  stop(
    "Missing required variables for script 06: ",
    paste(missing_configuration_vars, collapse = ", "),
    "\nRun scripts 01, 03, 04 and 05 before running script 06."
  )
}

expected_levels_full <- c(
  "Low expected distress",
  "Intermediate expected distress",
  "High expected distress"
)

subjective_levels_full <- c(
  "Low subjective insecurity",
  "Moderate subjective insecurity",
  "High subjective insecurity"
)

prospective_levels_full <- c(
  "No prospective insecurity",
  "One prospective insecurity",
  "Multiple prospective insecurities"
)

current_strain_levels_full <- c(
  "No current financial strain",
  "One current financial strain",
  "Multiple current financial strains"
)

residual_position_levels <- c(
  "Lower than expected distress",
  "Close to expected distress",
  "Higher than expected distress"
)

configuration_levels <- c(
  "Low/intermediate expected, not higher distress",
  "Higher-than-expected distress",
  "Not higher distress under objective risk",
  "Accumulated expected and residual distress"
)

configuration_short_levels <- c(
  "Low/intermediate expected,\nnot higher distress",
  "Higher-than-\nexpected distress",
  "Not higher distress\nunder objective risk",
  "Accumulated expected\nand residual distress"
)


# ------------------------------------------------------------
# 04. Plot style
# ------------------------------------------------------------

article_palette <- c(
  purple_dark   = "#5B2A86",
  purple_mid    = "#8E44AD",
  purple_light  = "#C77DCC",
  mustard_dark  = "#D4A000",
  mustard_mid   = "#E0B43B",
  mustard_light = "#F3E6B3",
  grey_dark     = "#4D4D4D",
  grey_mid      = "#8A8A8A",
  grey_light    = "#D9D9D9"
)

configuration_palette <- c(
  "Low/intermediate expected, not higher distress" =
    unname(article_palette["mustard_light"]),
  "Higher-than-expected distress" =
    unname(article_palette["purple_mid"]),
  "Not higher distress under objective risk" =
    unname(article_palette["purple_light"]),
  "Accumulated expected and residual distress" =
    unname(article_palette["purple_dark"])
)

subjective_diagnostic_palette <- c(
  "High subjective insecurity" =
    unname(article_palette["mustard_dark"]),
  "Multiple prospective insecurities" =
    unname(article_palette["purple_dark"]),
  "Multiple current financial strains" =
    unname(article_palette["mustard_mid"]),
  "Financially unprotected in health emergency" =
    unname(article_palette["purple_mid"]),
  "No emergency monetary support" =
    unname(article_palette["purple_light"]),
  "High or uncertain job-loss risk" =
    unname(article_palette["grey_dark"]),
  "Debt payment problems" =
    unname(article_palette["mustard_dark"]),
  "Difficulty making ends meet" =
    unname(article_palette["mustard_mid"])
)

subjective_diagnostic_compact_palette <- c(
  "High subjective insecurity" =
    unname(article_palette["mustard_dark"]),
  "Debt payment problems" =
    unname(article_palette["mustard_dark"]),
  "Difficulty making ends meet" =
    unname(article_palette["mustard_mid"]),
  "Financially unprotected in health emergency" =
    unname(article_palette["purple_mid"])
)

main_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 3,
        color = article_palette["grey_dark"]
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size,
        color = article_palette["grey_dark"]
      ),
      plot.caption = ggplot2::element_text(
        size = base_size - 3,
        color = article_palette["grey_mid"],
        hjust = 0
      ),
      axis.title = ggplot2::element_text(
        size = base_size,
        color = article_palette["grey_dark"]
      ),
      axis.text = ggplot2::element_text(
        size = base_size - 1,
        color = article_palette["grey_dark"]
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = base_size - 1,
        color = article_palette["grey_dark"]
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


# ------------------------------------------------------------
# 05. Helper functions
# ------------------------------------------------------------

weighted_distribution <- function(data, group_var, weight_var = "weight") {
  data %>%
    dplyr::filter(
      !is.na(.data[[group_var]]),
      !is.na(.data[[weight_var]])
    ) %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(group = 1) %>%
    dplyr::mutate(
      variable = group_var,
      weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE)
    ) %>%
    dplyr::relocate(variable, group)
}

weighted_summary_by_group <- function(data, group_var, weight_var = "weight") {
  data %>%
    dplyr::filter(
      !is.na(.data[[group_var]]),
      !is.na(.data[[weight_var]]),
      !is.na(phq4_score),
      !is.na(phq4_expected_objective),
      !is.na(phq4_residual_objective)
    ) %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      
      mean_observed_phq4 = weighted_mean_approx(phq4_score, .data[[weight_var]]),
      sd_observed_phq4 = weighted_sd_approx(phq4_score, .data[[weight_var]]),
      
      mean_expected_phq4 = weighted_mean_approx(phq4_expected_objective, .data[[weight_var]]),
      sd_expected_phq4 = weighted_sd_approx(phq4_expected_objective, .data[[weight_var]]),
      
      mean_residual_phq4 = weighted_mean_approx(phq4_residual_objective, .data[[weight_var]]),
      sd_residual_phq4 = weighted_sd_approx(phq4_residual_objective, .data[[weight_var]]),
      
      mean_residual_phq4_z = weighted_mean_approx(phq4_residual_objective_z, .data[[weight_var]]),
      
      pct_higher_than_expected = 100 * weighted_mean_approx(higher_than_expected, .data[[weight_var]]),
      pct_lower_than_expected = 100 * weighted_mean_approx(lower_than_expected, .data[[weight_var]]),
      
      pct_high_subjective_insecurity =
        100 * weighted_mean_approx(high_subjective_insecurity, .data[[weight_var]]),
      pct_multiple_prospective_insecurities =
        100 * weighted_mean_approx(multiple_prospective_insecurities, .data[[weight_var]]),
      pct_multiple_current_financial_strains =
        100 * weighted_mean_approx(multiple_current_financial_strains, .data[[weight_var]]),
      
      pct_financially_unprotected_health =
        100 * weighted_mean_approx(financially_unprotected_health, .data[[weight_var]]),
      pct_no_emergency_money_support =
        100 * weighted_mean_approx(no_emergency_money_support_binary, .data[[weight_var]]),
      pct_high_or_uncertain_job_loss_risk =
        100 * weighted_mean_approx(high_or_uncertain_job_loss_risk, .data[[weight_var]]),
      pct_debt_payment_problems =
        100 * weighted_mean_approx(debt_payment_problems_binary, .data[[weight_var]]),
      pct_difficulty_making_ends_meet =
        100 * weighted_mean_approx(difficulty_making_ends_meet_binary, .data[[weight_var]]),
      
      .groups = "drop"
    ) %>%
    dplyr::rename(group = 1) %>%
    dplyr::mutate(
      variable = group_var,
      weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE)
    ) %>%
    dplyr::relocate(variable, group)
}

weighted_crosstab <- function(data, row_var, col_var, weight_var = "weight") {
  data %>%
    dplyr::filter(
      !is.na(.data[[row_var]]),
      !is.na(.data[[col_var]]),
      !is.na(.data[[weight_var]])
    ) %>%
    dplyr::group_by(.data[[row_var]], .data[[col_var]]) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(row = 1, column = 2) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(
      row_weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(column) %>%
    dplyr::mutate(
      column_weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      total_weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE),
      row_var = row_var,
      col_var = col_var
    ) %>%
    dplyr::relocate(row_var, col_var, row, column)
}


# ------------------------------------------------------------
# 06. Construct configurational diagnostic variables
# ------------------------------------------------------------

analytic_06 <- analytic %>%
  dplyr::mutate(
    phq4_expected_tercile_f = factor(
      as.character(phq4_expected_tercile_f),
      levels = expected_levels_full
    ),
    
    phq4_residual_position_f = factor(
      as.character(phq4_residual_position_f),
      levels = residual_position_levels
    ),
    
    subjective_insecurity_level_f = factor(
      as.character(subjective_insecurity_level_f),
      levels = subjective_levels_full
    ),
    
    prospective_insecurity_level_f = factor(
      as.character(prospective_insecurity_level_f),
      levels = prospective_levels_full
    ),
    
    current_financial_strain_level_f = factor(
      as.character(current_financial_strain_level_f),
      levels = current_strain_levels_full
    ),
    
    expected_position_2cat = dplyr::case_when(
      phq4_expected_tercile_f %in% c(
        "Low expected distress",
        "Intermediate expected distress"
      ) ~ "Low/intermediate expected distress",
      phq4_expected_tercile_f == "High expected distress" ~ "High expected distress",
      TRUE ~ NA_character_
    ),
    
    expected_position_2cat_f = factor(
      expected_position_2cat,
      levels = c(
        "Low/intermediate expected distress",
        "High expected distress"
      )
    ),
    
    residual_excess_2cat = dplyr::case_when(
      higher_than_expected == 1 ~ "Higher than expected",
      higher_than_expected == 0 ~ "Not higher than expected",
      TRUE ~ NA_character_
    ),
    
    residual_excess_2cat_f = factor(
      residual_excess_2cat,
      levels = c(
        "Not higher than expected",
        "Higher than expected"
      )
    ),
    
    diagnostic_configuration = dplyr::case_when(
      expected_position_2cat_f == "Low/intermediate expected distress" &
        residual_excess_2cat_f == "Not higher than expected" ~
        "Low/intermediate expected, not higher distress",
      
      expected_position_2cat_f == "Low/intermediate expected distress" &
        residual_excess_2cat_f == "Higher than expected" ~
        "Higher-than-expected distress",
      
      expected_position_2cat_f == "High expected distress" &
        residual_excess_2cat_f == "Not higher than expected" ~
        "Not higher distress under objective risk",
      
      expected_position_2cat_f == "High expected distress" &
        residual_excess_2cat_f == "Higher than expected" ~
        "Accumulated expected and residual distress",
      
      TRUE ~ NA_character_
    ),
    
    diagnostic_configuration_f = factor(
      diagnostic_configuration,
      levels = configuration_levels
    ),
    
    diagnostic_configuration_short = dplyr::recode(
      diagnostic_configuration,
      "Low/intermediate expected, not higher distress" =
        "Low/intermediate expected,\nnot higher distress",
      "Higher-than-expected distress" =
        "Higher-than-\nexpected distress",
      "Not higher distress under objective risk" =
        "Not higher distress\nunder objective risk",
      "Accumulated expected and residual distress" =
        "Accumulated expected\nand residual distress",
      .default = NA_character_
    ),
    
    diagnostic_configuration_short_f = factor(
      diagnostic_configuration_short,
      levels = configuration_short_levels
    ),
    
    high_subjective_insecurity = dplyr::case_when(
      subjective_insecurity_level_f == "High subjective insecurity" ~ 1,
      subjective_insecurity_level_f %in% c(
        "Low subjective insecurity",
        "Moderate subjective insecurity"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    multiple_prospective_insecurities = dplyr::case_when(
      prospective_insecurity_level_f == "Multiple prospective insecurities" ~ 1,
      prospective_insecurity_level_f %in% c(
        "No prospective insecurity",
        "One prospective insecurity"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    multiple_current_financial_strains = dplyr::case_when(
      current_financial_strain_level_f == "Multiple current financial strains" ~ 1,
      current_financial_strain_level_f %in% c(
        "No current financial strain",
        "One current financial strain"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    financially_unprotected_health = dplyr::case_when(
      health_financial_protection_3cat_f ==
        "Financially unprotected in health emergency" ~ 1,
      health_financial_protection_3cat_f %in% c(
        "Financially protected in health emergency",
        "Neither protected nor unprotected in health emergency"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    no_emergency_money_support_binary = dplyr::case_when(
      no_emergency_money_support_f == "No emergency money support" ~ 1,
      no_emergency_money_support_f == "Has emergency money support" ~ 0,
      TRUE ~ NA_real_
    ),
    
    high_or_uncertain_job_loss_risk = dplyr::case_when(
      employment_risk_position_f %in% c(
        "Employed, high perceived job-loss risk",
        "Employed, uncertain perceived job-loss risk"
      ) ~ 1,
      employment_risk_position_f %in% c(
        "Employed, low perceived job-loss risk",
        "Outside current employment context"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    debt_payment_problems_binary = dplyr::case_when(
      debt_status_f %in% c(
        "Debt, some payment problems",
        "Debt, no payments on time"
      ) ~ 1,
      debt_status_f %in% c(
        "No debt",
        "Debt, all payments on time"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    difficulty_making_ends_meet_binary = dplyr::case_when(
      making_ends_meet_3cat_f == "Difficulty making ends meet" ~ 1,
      making_ends_meet_3cat_f %in% c(
        "Neither difficulty nor ease",
        "Ease making ends meet"
      ) ~ 0,
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 07. Sample diagnostics and rule documentation
# ------------------------------------------------------------

sample_summary <- tibble::tibble(
  sample_definition = c(
    "Full Script 06 input data",
    "Valid observed, expected and residual PHQ-4",
    "Valid expected-position category",
    "Valid residual-excess category",
    "Valid diagnostic configuration",
    "Valid diagnostic configuration and subjective insecurity",
    "Valid diagnostic configuration and all subjective diagnostic indicators"
  ),
  unweighted_n = c(
    nrow(analytic_06),
    sum(
      !is.na(analytic_06$phq4_score) &
        !is.na(analytic_06$phq4_expected_objective) &
        !is.na(analytic_06$phq4_residual_objective)
    ),
    sum(!is.na(analytic_06$expected_position_2cat_f)),
    sum(!is.na(analytic_06$residual_excess_2cat_f)),
    sum(!is.na(analytic_06$diagnostic_configuration_f)),
    sum(
      !is.na(analytic_06$diagnostic_configuration_f) &
        !is.na(analytic_06$subjective_insecurity_level_f)
    ),
    sum(
      !is.na(analytic_06$diagnostic_configuration_f) &
        !is.na(analytic_06$high_subjective_insecurity) &
        !is.na(analytic_06$multiple_prospective_insecurities) &
        !is.na(analytic_06$multiple_current_financial_strains) &
        !is.na(analytic_06$financially_unprotected_health) &
        !is.na(analytic_06$no_emergency_money_support_binary) &
        !is.na(analytic_06$high_or_uncertain_job_loss_risk) &
        !is.na(analytic_06$debt_payment_problems_binary) &
        !is.na(analytic_06$difficulty_making_ends_meet_binary)
    )
  ),
  weighted_n = c(
    sum(analytic_06$weight, na.rm = TRUE),
    sum(
      analytic_06$weight[
        !is.na(analytic_06$phq4_score) &
          !is.na(analytic_06$phq4_expected_objective) &
          !is.na(analytic_06$phq4_residual_objective)
      ],
      na.rm = TRUE
    ),
    sum(analytic_06$weight[!is.na(analytic_06$expected_position_2cat_f)], na.rm = TRUE),
    sum(analytic_06$weight[!is.na(analytic_06$residual_excess_2cat_f)], na.rm = TRUE),
    sum(analytic_06$weight[!is.na(analytic_06$diagnostic_configuration_f)], na.rm = TRUE),
    sum(
      analytic_06$weight[
        !is.na(analytic_06$diagnostic_configuration_f) &
          !is.na(analytic_06$subjective_insecurity_level_f)
      ],
      na.rm = TRUE
    ),
    sum(
      analytic_06$weight[
        !is.na(analytic_06$diagnostic_configuration_f) &
          !is.na(analytic_06$high_subjective_insecurity) &
          !is.na(analytic_06$multiple_prospective_insecurities) &
          !is.na(analytic_06$multiple_current_financial_strains) &
          !is.na(analytic_06$financially_unprotected_health) &
          !is.na(analytic_06$no_emergency_money_support_binary) &
          !is.na(analytic_06$high_or_uncertain_job_loss_risk) &
          !is.na(analytic_06$debt_payment_problems_binary) &
          !is.na(analytic_06$difficulty_making_ends_meet_binary)
      ],
      na.rm = TRUE
    )
  )
)

variable_check <- tibble::tibble(
  variable = c(
    required_configuration_vars,
    "expected_position_2cat_f",
    "residual_excess_2cat_f",
    "diagnostic_configuration_f",
    "high_subjective_insecurity",
    "multiple_prospective_insecurities",
    "multiple_current_financial_strains",
    "financially_unprotected_health",
    "no_emergency_money_support_binary",
    "high_or_uncertain_job_loss_risk",
    "debt_payment_problems_binary",
    "difficulty_making_ends_meet_binary"
  ),
  available = variable %in% names(analytic_06),
  n_valid = purrr::map_int(
    variable,
    ~ if (.x %in% names(analytic_06)) sum(!is.na(analytic_06[[.x]])) else NA_integer_
  ),
  n_missing = purrr::map_int(
    variable,
    ~ if (.x %in% names(analytic_06)) sum(is.na(analytic_06[[.x]])) else NA_integer_
  ),
  pct_missing = n_missing / nrow(analytic_06) * 100
)

diagnostic_rules <- tibble::tribble(
  ~configuration, ~expected_position, ~residual_position, ~interpretation,
  "Low/intermediate expected, not higher distress",
  "Low/intermediate objective-position expected PHQ-4",
  "Not higher than expected",
  "Distress does not exceed what would be expected from low/intermediate objective-position risk.",
  
  "Higher-than-expected distress",
  "Low/intermediate objective-position expected PHQ-4",
  "Higher than expected",
  "Distress exceeds what would be expected from low/intermediate objective-position risk.",
  
  "Not higher distress under objective risk",
  "High objective-position expected PHQ-4",
  "Not higher than expected",
  "Objective-position risk is high, but distress does not exceed expectation.",
  
  "Accumulated expected and residual distress",
  "High objective-position expected PHQ-4",
  "Higher than expected",
  "Objective-position risk and residual excess distress converge."
) %>%
  dplyr::mutate(
    configuration = factor(configuration, levels = configuration_levels)
  )


# ------------------------------------------------------------
# 08. Configuration summaries
# ------------------------------------------------------------

diagnostic_distribution <- weighted_distribution(
  analytic_06,
  "diagnostic_configuration_f",
  "weight"
) %>%
  dplyr::mutate(
    group = factor(as.character(group), levels = configuration_levels),
    group_short = dplyr::recode(
      as.character(group),
      "Low/intermediate expected, not higher distress" =
        "Low/intermediate expected,\nnot higher distress",
      "Higher-than-expected distress" =
        "Higher-than-\nexpected distress",
      "Not higher distress under objective risk" =
        "Not higher distress\nunder objective risk",
      "Accumulated expected and residual distress" =
        "Accumulated expected\nand residual distress"
    ),
    group_short = factor(group_short, levels = configuration_short_levels)
  )

diagnostic_summary <- weighted_summary_by_group(
  analytic_06,
  "diagnostic_configuration_f",
  "weight"
) %>%
  dplyr::mutate(
    group = factor(as.character(group), levels = configuration_levels),
    group_short = dplyr::recode(
      as.character(group),
      "Low/intermediate expected, not higher distress" =
        "Low/intermediate expected,\nnot higher distress",
      "Higher-than-expected distress" =
        "Higher-than-\nexpected distress",
      "Not higher distress under objective risk" =
        "Not higher distress\nunder objective risk",
      "Accumulated expected and residual distress" =
        "Accumulated expected\nand residual distress"
    ),
    group_short = factor(group_short, levels = configuration_short_levels)
  )

diagnostic_metric_long <- diagnostic_summary %>%
  dplyr::select(
    group,
    group_short,
    weighted_pct,
    mean_observed_phq4,
    mean_expected_phq4,
    mean_residual_phq4,
    pct_higher_than_expected,
    pct_lower_than_expected
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      mean_observed_phq4,
      mean_expected_phq4,
      mean_residual_phq4,
      pct_higher_than_expected,
      pct_lower_than_expected
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    metric = dplyr::recode(
      metric,
      mean_observed_phq4 = "Observed PHQ-4",
      mean_expected_phq4 = "Expected PHQ-4",
      mean_residual_phq4 = "Residual PHQ-4",
      pct_higher_than_expected = "Higher-than-expected distress (%)",
      pct_lower_than_expected = "Lower-than-expected distress (%)"
    ),
    metric = factor(
      metric,
      levels = c(
        "Observed PHQ-4",
        "Expected PHQ-4",
        "Residual PHQ-4",
        "Higher-than-expected distress (%)",
        "Lower-than-expected distress (%)"
      )
    )
  )

subjective_diagnostics_long <- diagnostic_summary %>%
  dplyr::select(
    group,
    group_short,
    weighted_pct,
    pct_high_subjective_insecurity,
    pct_multiple_prospective_insecurities,
    pct_multiple_current_financial_strains,
    pct_financially_unprotected_health,
    pct_no_emergency_money_support,
    pct_high_or_uncertain_job_loss_risk,
    pct_debt_payment_problems,
    pct_difficulty_making_ends_meet
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      pct_high_subjective_insecurity,
      pct_multiple_prospective_insecurities,
      pct_multiple_current_financial_strains,
      pct_financially_unprotected_health,
      pct_no_emergency_money_support,
      pct_high_or_uncertain_job_loss_risk,
      pct_debt_payment_problems,
      pct_difficulty_making_ends_meet
    ),
    names_to = "diagnostic",
    values_to = "weighted_pct_indicator"
  ) %>%
  dplyr::mutate(
    diagnostic_label = dplyr::recode(
      diagnostic,
      pct_high_subjective_insecurity = "High subjective insecurity",
      pct_multiple_prospective_insecurities = "Multiple prospective insecurities",
      pct_multiple_current_financial_strains = "Multiple current financial strains",
      pct_financially_unprotected_health = "Financially unprotected in health emergency",
      pct_no_emergency_money_support = "No emergency monetary support",
      pct_high_or_uncertain_job_loss_risk = "High or uncertain job-loss risk",
      pct_debt_payment_problems = "Debt payment problems",
      pct_difficulty_making_ends_meet = "Difficulty making ends meet"
    ),
    diagnostic_label = factor(
      diagnostic_label,
      levels = c(
        "High subjective insecurity",
        "Multiple prospective insecurities",
        "Multiple current financial strains",
        "Financially unprotected in health emergency",
        "No emergency monetary support",
        "High or uncertain job-loss risk",
        "Debt payment problems",
        "Difficulty making ends meet"
      )
    )
  )

subjective_diagnostics_compact_long <- subjective_diagnostics_long %>%
  dplyr::filter(
    diagnostic_label %in% c(
      "High subjective insecurity",
      "Debt payment problems",
      "Difficulty making ends meet",
      "Financially unprotected in health emergency"
    )
  ) %>%
  dplyr::mutate(
    diagnostic_label = factor(
      diagnostic_label,
      levels = c(
        "High subjective insecurity",
        "Debt payment problems",
        "Difficulty making ends meet",
        "Financially unprotected in health emergency"
      )
    )
  )

expected_residual_matrix_summary <- analytic_06 %>%
  dplyr::filter(
    !is.na(expected_position_2cat_f),
    !is.na(residual_excess_2cat_f),
    !is.na(weight),
    !is.na(phq4_score),
    !is.na(phq4_expected_objective),
    !is.na(phq4_residual_objective)
  ) %>%
  dplyr::group_by(expected_position_2cat_f, residual_excess_2cat_f) %>%
  dplyr::summarise(
    unweighted_n = dplyr::n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_observed_phq4 = weighted_mean_approx(phq4_score, weight),
    mean_expected_phq4 = weighted_mean_approx(phq4_expected_objective, weight),
    mean_residual_phq4 = weighted_mean_approx(phq4_residual_objective, weight),
    pct_high_subjective_insecurity = 100 * weighted_mean_approx(high_subjective_insecurity, weight),
    pct_multiple_prospective_insecurities =
      100 * weighted_mean_approx(multiple_prospective_insecurities, weight),
    pct_multiple_current_financial_strains =
      100 * weighted_mean_approx(multiple_current_financial_strains, weight),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    weighted_pct_total = 100 * weighted_n / sum(weighted_n, na.rm = TRUE),
    matrix_text_color = dplyr::case_when(
      mean_residual_phq4 >= 1 ~ "white",
      TRUE ~ unname(article_palette["grey_dark"])
    )
  )

configuration_by_subjective_insecurity <- weighted_crosstab(
  analytic_06,
  "diagnostic_configuration_f",
  "subjective_insecurity_level_f",
  "weight"
)

configuration_by_prospective_insecurity <- weighted_crosstab(
  analytic_06,
  "diagnostic_configuration_f",
  "prospective_insecurity_level_f",
  "weight"
)

configuration_by_current_strain <- weighted_crosstab(
  analytic_06,
  "diagnostic_configuration_f",
  "current_financial_strain_level_f",
  "weight"
)

configuration_by_expected_tercile <- weighted_crosstab(
  analytic_06,
  "diagnostic_configuration_f",
  "phq4_expected_tercile_f",
  "weight"
)

configuration_by_residual_position <- weighted_crosstab(
  analytic_06,
  "diagnostic_configuration_f",
  "phq4_residual_position_f",
  "weight"
)


# ------------------------------------------------------------
# 09. Figures
# ------------------------------------------------------------

p_diagnostic_distribution <- ggplot2::ggplot(
  diagnostic_distribution,
  ggplot2::aes(
    x = weighted_pct,
    y = forcats::fct_rev(group_short),
    fill = group
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f%%", weighted_pct)),
    hjust = -0.10,
    size = 4.2,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::scale_fill_manual(
    values = configuration_palette,
    drop = FALSE,
    na.value = unname(article_palette["grey_light"])
  ) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = ggplot2::expansion(mult = c(0, 0.14))
  ) +
  ggplot2::labs(
    title = "Distribution of expected-residual diagnostic configurations",
    subtitle = "Configurations are defined only by objective-position expected PHQ-4 and residual PHQ-4",
    x = "Weighted percentage",
    y = NULL
  ) +
  main_theme(base_size = 13)

plot_oe_residual <- diagnostic_metric_long %>%
  dplyr::filter(
    metric %in% c(
      "Observed PHQ-4",
      "Expected PHQ-4",
      "Residual PHQ-4"
    )
  )

p_observed_expected_residual_by_diagnostic <- ggplot2::ggplot(
  plot_oe_residual,
  ggplot2::aes(
    x = value,
    y = forcats::fct_rev(group_short),
    fill = group
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_vline(
    data = plot_oe_residual %>%
      dplyr::filter(metric == "Residual PHQ-4"),
    ggplot2::aes(xintercept = 0),
    linewidth = 0.75,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::facet_wrap(~ metric, scales = "free_x", nrow = 1) +
  ggplot2::scale_fill_manual(
    values = configuration_palette,
    drop = FALSE,
    na.value = unname(article_palette["grey_light"])
  ) +
  ggplot2::labs(
    title = "Observed, expected and residual PHQ-4 by diagnostic configuration",
    subtitle = "Residual PHQ-4 is observed PHQ-4 minus objective-position expected PHQ-4",
    x = "Weighted mean",
    y = NULL
  ) +
  main_theme(base_size = 12)

p_subjective_diagnostics <- ggplot2::ggplot(
  subjective_diagnostics_long,
  ggplot2::aes(
    x = weighted_pct_indicator,
    y = forcats::fct_rev(diagnostic_label),
    color = diagnostic_label
  )
) +
  ggplot2::geom_point(size = 3.4) +
  ggplot2::facet_wrap(
    ~ group_short,
    nrow = 2
  ) +
  ggplot2::scale_color_manual(
    values = subjective_diagnostic_palette,
    drop = FALSE,
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, NA)
  ) +
  ggplot2::labs(
    title = "Subjective economic-security diagnostics by expected-residual configuration",
    subtitle = "Subjective appraisals characterize configurations but do not define them",
    x = "Weighted percentage within configuration",
    y = NULL
  ) +
  main_theme(base_size = 12)

p_subjective_diagnostics_compact <- ggplot2::ggplot(
  subjective_diagnostics_compact_long,
  ggplot2::aes(
    x = weighted_pct_indicator,
    y = forcats::fct_rev(diagnostic_label),
    color = diagnostic_label
  )
) +
  ggplot2::geom_point(size = 3.8) +
  ggplot2::facet_wrap(
    ~ group_short,
    nrow = 1
  ) +
  ggplot2::scale_color_manual(
    values = subjective_diagnostic_compact_palette,
    drop = FALSE,
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, NA)
  ) +
  ggplot2::labs(
    title = "Selected subjective-security diagnostics by expected-residual configuration",
    subtitle = "Selected indicators characterize configurations but do not define them",
    x = "Weighted percentage within configuration",
    y = NULL
  ) +
  main_theme(base_size = 12)

p_expected_residual_matrix <- ggplot2::ggplot(
  expected_residual_matrix_summary,
  ggplot2::aes(
    x = residual_excess_2cat_f,
    y = expected_position_2cat_f,
    fill = mean_residual_phq4
  )
) +
  ggplot2::geom_tile(color = "white", linewidth = 1.1) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(
        sprintf("%.1f%%", weighted_pct_total),
        "\nResidual: ",
        sprintf("%.2f", mean_residual_phq4),
        "\nHigh subj. ins.: ",
        sprintf("%.1f%%", pct_high_subjective_insecurity)
      ),
      color = matrix_text_color
    ),
    size = 4.2,
    lineheight = 0.95
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_gradient2(
    low = unname(article_palette["mustard_light"]),
    mid = "white",
    high = unname(article_palette["purple_dark"]),
    midpoint = 0,
    name = "Mean\nresidual"
  ) +
  ggplot2::labs(
    title = "Objective-position expected PHQ-4 by residual excess",
    subtitle = "Cells show sample share, mean residual PHQ-4 and high subjective insecurity",
    x = "Residual PHQ-4 position",
    y = "Objective-position expected PHQ-4"
  ) +
  main_theme(base_size = 13) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank()
  )

diagnostic_table_plot_data <- diagnostic_summary %>%
  dplyr::select(
    group,
    group_short,
    weighted_pct,
    mean_observed_phq4,
    mean_expected_phq4,
    mean_residual_phq4,
    pct_high_subjective_insecurity,
    pct_debt_payment_problems,
    pct_difficulty_making_ends_meet,
    pct_no_emergency_money_support,
    pct_financially_unprotected_health
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      weighted_pct,
      mean_observed_phq4,
      mean_expected_phq4,
      mean_residual_phq4,
      pct_high_subjective_insecurity,
      pct_debt_payment_problems,
      pct_difficulty_making_ends_meet,
      pct_no_emergency_money_support,
      pct_financially_unprotected_health
    ),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    indicator_label = dplyr::recode(
      indicator,
      weighted_pct = "Sample share (%)",
      mean_observed_phq4 = "Observed PHQ-4",
      mean_expected_phq4 = "Expected PHQ-4",
      mean_residual_phq4 = "Residual PHQ-4",
      pct_high_subjective_insecurity = "High subjective insecurity (%)",
      pct_debt_payment_problems = "Debt payment problems (%)",
      pct_difficulty_making_ends_meet = "Difficulty making ends meet (%)",
      pct_no_emergency_money_support = "No emergency monetary support (%)",
      pct_financially_unprotected_health = "Financially unprotected health shock (%)"
    ),
    indicator_label = factor(
      indicator_label,
      levels = c(
        "Sample share (%)",
        "Observed PHQ-4",
        "Expected PHQ-4",
        "Residual PHQ-4",
        "High subjective insecurity (%)",
        "Debt payment problems (%)",
        "Difficulty making ends meet (%)",
        "No emergency monetary support (%)",
        "Financially unprotected health shock (%)"
      )
    ),
    value_label = dplyr::case_when(
      stringr::str_detect(as.character(indicator_label), "\\(\\%\\)") |
        indicator_label == "Sample share (%)" ~ sprintf("%.1f", value),
      TRUE ~ sprintf("%.2f", value)
    )
  )

p_diagnostic_table_heatmap <- ggplot2::ggplot(
  diagnostic_table_plot_data,
  ggplot2::aes(
    x = indicator_label,
    y = forcats::fct_rev(group_short),
    fill = group
  )
) +
  ggplot2::geom_tile(
    color = "white",
    linewidth = 0.8,
    alpha = 0.22
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = value_label),
    size = 3.4,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::scale_fill_manual(
    values = configuration_palette,
    guide = "none",
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Configurational diagnostics summary",
    subtitle = "Values describe expected-residual configurations and their subjective-security profile",
    x = NULL,
    y = NULL
  ) +
  main_theme(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    panel.grid = ggplot2::element_blank()
  )

p_high_subjective_by_configuration <- ggplot2::ggplot(
  diagnostic_summary,
  ggplot2::aes(
    x = pct_high_subjective_insecurity,
    y = forcats::fct_rev(group_short),
    fill = group
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f%%", pct_high_subjective_insecurity)),
    hjust = -0.10,
    size = 4.2,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::scale_fill_manual(
    values = configuration_palette,
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = ggplot2::expansion(mult = c(0, 0.14))
  ) +
  ggplot2::labs(
    title = "High subjective insecurity by expected-residual configuration",
    subtitle = "Subjective insecurity is used as a diagnostic, not as a configuration rule",
    x = "Weighted percentage within configuration",
    y = NULL
  ) +
  main_theme(base_size = 13)


# ------------------------------------------------------------
# 10. Export outputs
# ------------------------------------------------------------

output_excel <- file.path(
  script_tables_dir,
  "06_configurational_diagnostics_tables.xlsx"
)

openxlsx::write.xlsx(
  list(
    sample_summary = sample_summary,
    variable_check = variable_check,
    diagnostic_rules = diagnostic_rules,
    config_distribution = diagnostic_distribution,
    config_summary = diagnostic_summary,
    config_metric_long = diagnostic_metric_long,
    subjective_diag_long = subjective_diagnostics_long,
    subjective_diag_compact = subjective_diagnostics_compact_long,
    expected_resid_matrix = expected_residual_matrix_summary,
    by_subj_insecurity = configuration_by_subjective_insecurity,
    by_prosp_insecurity = configuration_by_prospective_insecurity,
    by_current_strain = configuration_by_current_strain,
    by_expected_tercile = configuration_by_expected_tercile,
    by_residual_position = configuration_by_residual_position
  ),
  file = output_excel,
  overwrite = TRUE
)

readr::write_csv(
  sample_summary,
  file.path(script_csv_dir, "06_sample_summary.csv")
)

readr::write_csv(
  variable_check,
  file.path(script_csv_dir, "06_variable_check.csv")
)

readr::write_csv(
  diagnostic_rules,
  file.path(script_csv_dir, "06_diagnostic_rules.csv")
)

readr::write_csv(
  diagnostic_distribution,
  file.path(script_csv_dir, "06_diagnostic_distribution.csv")
)

readr::write_csv(
  diagnostic_summary,
  file.path(script_csv_dir, "06_diagnostic_summary.csv")
)

readr::write_csv(
  diagnostic_metric_long,
  file.path(script_csv_dir, "06_diagnostic_metric_long.csv")
)

readr::write_csv(
  subjective_diagnostics_long,
  file.path(script_csv_dir, "06_subjective_diagnostics_long.csv")
)

readr::write_csv(
  subjective_diagnostics_compact_long,
  file.path(script_csv_dir, "06_subjective_diagnostics_compact_long.csv")
)

readr::write_csv(
  expected_residual_matrix_summary,
  file.path(script_csv_dir, "06_expected_residual_matrix_summary.csv")
)

readr::write_csv(
  configuration_by_subjective_insecurity,
  file.path(script_csv_dir, "06_configuration_by_subjective_insecurity.csv")
)

readr::write_csv(
  configuration_by_prospective_insecurity,
  file.path(script_csv_dir, "06_configuration_by_prospective_insecurity.csv")
)

readr::write_csv(
  configuration_by_current_strain,
  file.path(script_csv_dir, "06_configuration_by_current_strain.csv")
)

readr::write_csv(
  configuration_by_expected_tercile,
  file.path(script_csv_dir, "06_configuration_by_expected_tercile.csv")
)

readr::write_csv(
  configuration_by_residual_position,
  file.path(script_csv_dir, "06_configuration_by_residual_position.csv")
)

analytic_06_path <- file.path(
  script_rds_dir,
  "06_configurational_diagnostics_dataset.rds"
)

saveRDS(
  analytic_06,
  analytic_06_path
)

metadata_06 <- list(
  script_name = script_name,
  input_rds = input_rds,
  configuration_levels = configuration_levels,
  configuration_short_levels = configuration_short_levels,
  expected_levels_full = expected_levels_full,
  subjective_levels_full = subjective_levels_full,
  prospective_levels_full = prospective_levels_full,
  current_strain_levels_full = current_strain_levels_full,
  residual_position_levels = residual_position_levels,
  diagnostic_rules = diagnostic_rules,
  note = paste(
    "Configurations are defined only by objective-position expected PHQ-4",
    "and residual-excess status. Subjective-security variables are used as",
    "diagnostics, not as classification criteria. The not-higher-than-expected",
    "category combines respondents close to expected PHQ-4 and respondents",
    "lower than expected."
  )
)

metadata_06_path <- file.path(
  script_rds_dir,
  "06_configurational_diagnostics_metadata.rds"
)

saveRDS(
  metadata_06,
  metadata_06_path
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "06_diagnostic_configuration_distribution.png"),
  plot = p_diagnostic_distribution,
  width = 13,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "06_observed_expected_residual_by_diagnostic_configuration.png"),
  plot = p_observed_expected_residual_by_diagnostic,
  width = 16,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "06_subjective_security_diagnostics_by_configuration.png"),
  plot = p_subjective_diagnostics,
  width = 15,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "06_subjective_security_diagnostics_by_configuration_compact.png"),
  plot = p_subjective_diagnostics_compact,
  width = 16,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "06_expected_residual_matrix.png"),
  plot = p_expected_residual_matrix,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "06_diagnostic_summary_table.png"),
  plot = p_diagnostic_table_heatmap,
  width = 17,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "06_high_subjective_insecurity_by_configuration.png"),
  plot = p_high_subjective_by_configuration,
  width = 13,
  height = 7,
  dpi = 300,
  bg = "white"
)

manifest <- tibble::tibble(
  output = c(
    "Excel diagnostics",
    "Script 06 analytic dataset",
    "Script 06 metadata",
    "Diagnostic configuration distribution figure",
    "Observed expected residual by diagnostic configuration figure",
    "Subjective-security diagnostics by configuration figure",
    "Compact subjective-security diagnostics by configuration figure",
    "Expected-residual matrix figure",
    "Diagnostic summary table figure",
    "High subjective insecurity by configuration figure",
    "Diagnostic summary CSV",
    "Subjective diagnostics CSV",
    "Compact subjective diagnostics CSV"
  ),
  path = c(
    output_excel,
    analytic_06_path,
    metadata_06_path,
    file.path(script_figures_dir, "06_diagnostic_configuration_distribution.png"),
    file.path(script_figures_dir, "06_observed_expected_residual_by_diagnostic_configuration.png"),
    file.path(script_figures_dir, "06_subjective_security_diagnostics_by_configuration.png"),
    file.path(script_figures_dir, "06_subjective_security_diagnostics_by_configuration_compact.png"),
    file.path(script_figures_dir, "06_expected_residual_matrix.png"),
    file.path(script_figures_dir, "06_diagnostic_summary_table.png"),
    file.path(script_figures_dir, "06_high_subjective_insecurity_by_configuration.png"),
    file.path(script_csv_dir, "06_diagnostic_summary.csv"),
    file.path(script_csv_dir, "06_subjective_diagnostics_long.csv"),
    file.path(script_csv_dir, "06_subjective_diagnostics_compact_long.csv")
  )
)

readr::write_csv(
  manifest,
  file.path(script_csv_dir, "06_output_manifest.csv")
)


# ------------------------------------------------------------
# 11. Save session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(script_logs_dir, "06_session_info.txt")
)


# ------------------------------------------------------------
# 12. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("06_configurational_diagnostics_expected_residual.R completed")
message("============================================================")
message("Input file: ", input_rds)
message("Output folder: ", script_output_dir)
message(
  "Valid diagnostic configuration cases: ",
  sample_summary$unweighted_n[
    sample_summary$sample_definition == "Valid diagnostic configuration"
  ]
)
message("Configurations are defined by expected PHQ-4 and residual-excess status only.")
message("Subjective-security variables are used as diagnostics, not as classification rules.")
message("The not-higher-than-expected category combines close-to-expected and lower-than-expected residual positions.")
message("Excel diagnostics: ", output_excel)
message("Main dataset: ", analytic_06_path)
message("Figures saved in: ", script_figures_dir)
message("CSV outputs saved in: ", script_csv_dir)
message("RDS outputs saved in: ", script_rds_dir)
message("============================================================\n")
