# ============================================================
# 07_robustness_sensitivity_checks.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Run robustness and sensitivity checks for the
#          expected-residual strategy
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Setup
# ------------------------------------------------------------

source("scripts/00_setup.R")

options(survey.lonely.psu = "adjust")

script_name <- "07_robustness_sensitivity_checks"

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
# 02. Load dataset from script 06
# ------------------------------------------------------------

input_rds <- file.path(
  output_dir,
  "06_configurational_diagnostics_expected_residual",
  "rds",
  "06_configurational_diagnostics_dataset.rds"
)

if (!file.exists(input_rds)) {
  stop(
    paste0(
      "Input dataset not found: ", input_rds, "\n",
      "Run scripts/06_configurational_diagnostics_expected_residual.R first."
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

required_core_vars <- c(
  "phq4_score",
  "phq4_expected_objective",
  "phq4_residual_objective",
  "phq4_residual_objective_z",
  "phq4_expected_tercile_f",
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

missing_core_vars <- setdiff(required_core_vars, names(analytic))

if (length(missing_core_vars) > 0) {
  stop(
    "Missing required variables for script 07: ",
    paste(missing_core_vars, collapse = ", "),
    "\nRun scripts 01, 03, 04 and 06 before running script 07."
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

threshold_palette <- c(
  "0.50 SD" = unname(article_palette["purple_dark"]),
  "0.75 SD" = unname(article_palette["purple_mid"]),
  "1.00 SD" = unname(article_palette["purple_light"])
)

subjective_block_palette <- c(
  "Prospective protection/threat" = unname(article_palette["purple_dark"]),
  "Current financial strain" = unname(article_palette["mustard_dark"]),
  "Full subjective-security model" = unname(article_palette["grey_dark"]),
  "Full model excluding making ends meet" = unname(article_palette["mustard_mid"])
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

make_formula <- function(outcome, predictors, data_names = names(analytic_07)) {
  predictors <- predictors[!is.na(predictors)]
  predictors <- predictors[predictors %in% data_names]
  
  if (length(predictors) == 0) {
    stats::as.formula(paste0(outcome, " ~ 1"))
  } else {
    stats::as.formula(
      paste0(outcome, " ~ ", paste(predictors, collapse = " + "))
    )
  }
}

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

tidy_svyglm <- function(model, model_label, exponentiate = FALSE) {
  if (is.null(model)) {
    return(tibble::tibble())
  }
  
  output <- broom::tidy(model) %>%
    dplyr::mutate(
      model = model_label,
      conf_low = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error
    )
  
  if (exponentiate) {
    output <- output %>%
      dplyr::mutate(
        estimate_exp = exp(estimate),
        conf_low_exp = exp(conf_low),
        conf_high_exp = exp(conf_high)
      )
  }
  
  output %>%
    dplyr::relocate(model)
}

safe_svyglm <- function(formula, design, family = gaussian()) {
  tryCatch(
    survey::svyglm(
      formula,
      design = design,
      family = family
    ),
    error = function(e) {
      message("Model failed: ", deparse(formula), "\nReason: ", e$message)
      return(NULL)
    }
  )
}

model_fit_summary <- function(model, data, outcome_var, model_label) {
  if (is.null(model)) {
    return(
      tibble::tibble(
        model = model_label,
        n = NA_integer_,
        weighted_n = NA_real_,
        r2_weighted = NA_real_
      )
    )
  }
  
  model_frame <- stats::model.frame(model)
  included_rows <- as.integer(rownames(model_frame))
  
  y <- data[[outcome_var]]
  yhat <- rep(NA_real_, nrow(data))
  yhat[included_rows] <- as.numeric(stats::predict(model, type = "response"))
  w <- data$weight
  
  ok <- !is.na(y) & !is.na(yhat) & !is.na(w)
  
  if (sum(ok) == 0) {
    return(
      tibble::tibble(
        model = model_label,
        n = 0L,
        weighted_n = NA_real_,
        r2_weighted = NA_real_
      )
    )
  }
  
  weighted_y_mean <- stats::weighted.mean(y[ok], w[ok], na.rm = TRUE)
  
  sse <- sum(w[ok] * (y[ok] - yhat[ok])^2, na.rm = TRUE)
  sst <- sum(w[ok] * (y[ok] - weighted_y_mean)^2, na.rm = TRUE)
  
  tibble::tibble(
    model = model_label,
    n = sum(ok),
    weighted_n = sum(w[ok], na.rm = TRUE),
    r2_weighted = 1 - sse / sst
  )
}

model_fit_summary_with_resid_sd <- function(model, data, outcome_var, model_label) {
  if (is.null(model)) {
    return(
      tibble::tibble(
        model = model_label,
        n = NA_integer_,
        weighted_n = NA_real_,
        r2_weighted = NA_real_,
        residual_sd_weighted = NA_real_
      )
    )
  }
  
  model_frame <- stats::model.frame(model)
  included_rows <- as.integer(rownames(model_frame))
  
  y <- data[[outcome_var]]
  yhat <- rep(NA_real_, nrow(data))
  yhat[included_rows] <- as.numeric(stats::predict(model, type = "response"))
  w <- data$weight
  
  ok <- !is.na(y) & !is.na(yhat) & !is.na(w)
  
  if (sum(ok) == 0) {
    return(
      tibble::tibble(
        model = model_label,
        n = 0L,
        weighted_n = NA_real_,
        r2_weighted = NA_real_,
        residual_sd_weighted = NA_real_
      )
    )
  }
  
  weighted_y_mean <- stats::weighted.mean(y[ok], w[ok], na.rm = TRUE)
  
  sse <- sum(w[ok] * (y[ok] - yhat[ok])^2, na.rm = TRUE)
  sst <- sum(w[ok] * (y[ok] - weighted_y_mean)^2, na.rm = TRUE)
  
  resid <- y[ok] - yhat[ok]
  
  tibble::tibble(
    model = model_label,
    n = sum(ok),
    weighted_n = sum(w[ok], na.rm = TRUE),
    r2_weighted = 1 - sse / sst,
    residual_sd_weighted = weighted_sd_approx(resid, w[ok])
  )
}

keep_economic_security_terms <- function(data) {
  economic_term_pattern <- paste(
    c(
      "health_financial_protection_3cat_f",
      "no_emergency_money_support_f",
      "employment_risk_position_f",
      "debt_status_f",
      "making_ends_meet_3cat_f"
    ),
    collapse = "|"
  )
  
  data %>%
    dplyr::filter(
      term != "(Intercept)",
      stringr::str_detect(term, economic_term_pattern)
    )
}

construct_expected_residual_configuration <- function(data, threshold_value) {
  threshold_label <- paste0(sprintf("%.2f", threshold_value), " SD")
  
  data %>%
    dplyr::mutate(
      threshold = threshold_label,
      
      expected_position_2cat = dplyr::case_when(
        phq4_expected_tercile_f %in% c(
          "Low expected distress",
          "Intermediate expected distress"
        ) ~ "Low/intermediate expected distress",
        phq4_expected_tercile_f == "High expected distress" ~
          "High expected distress",
        TRUE ~ NA_character_
      ),
      
      expected_position_2cat_f = factor(
        expected_position_2cat,
        levels = c(
          "Low/intermediate expected distress",
          "High expected distress"
        )
      ),
      
      higher_than_expected_threshold = dplyr::case_when(
        phq4_residual_objective_z >= threshold_value ~ 1,
        !is.na(phq4_residual_objective_z) ~ 0,
        TRUE ~ NA_real_
      ),
      
      lower_than_expected_threshold = dplyr::case_when(
        phq4_residual_objective_z <= -threshold_value ~ 1,
        !is.na(phq4_residual_objective_z) ~ 0,
        TRUE ~ NA_real_
      ),
      
      residual_excess_2cat = dplyr::case_when(
        higher_than_expected_threshold == 1 ~ "Higher than expected",
        higher_than_expected_threshold == 0 ~ "Not higher than expected",
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
      )
    )
}


# ------------------------------------------------------------
# 06. Construct robustness variables
# ------------------------------------------------------------

analytic_07 <- analytic %>%
  dplyr::mutate(
    phq4_expected_tercile_f = factor(
      as.character(phq4_expected_tercile_f),
      levels = expected_levels_full
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
    
    high_subjective_insecurity = dplyr::case_when(
      subjective_insecurity_level_f == "High subjective insecurity" ~ 1,
      subjective_insecurity_level_f %in% c(
        "Low subjective insecurity",
        "Moderate subjective insecurity"
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
    
    high_job_loss_risk_only = dplyr::case_when(
      employment_risk_position_f == "Employed, high perceived job-loss risk" ~ 1,
      employment_risk_position_f %in% c(
        "Employed, low perceived job-loss risk",
        "Employed, uncertain perceived job-loss risk",
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
  ) %>%
  dplyr::mutate(
    subjective_insecurity_valid_no_meet = rowSums(
      !is.na(
        dplyr::across(
          c(
            financially_unprotected_health,
            no_emergency_money_support_binary,
            high_or_uncertain_job_loss_risk,
            debt_payment_problems_binary
          )
        )
      )
    ),
    
    subjective_insecurity_count_no_meet_raw = rowSums(
      dplyr::across(
        c(
          financially_unprotected_health,
          no_emergency_money_support_binary,
          high_or_uncertain_job_loss_risk,
          debt_payment_problems_binary
        )
      ),
      na.rm = TRUE
    ),
    
    subjective_insecurity_count_no_meet = dplyr::if_else(
      subjective_insecurity_valid_no_meet >= 3,
      subjective_insecurity_count_no_meet_raw,
      NA_real_
    ),
    
    subjective_insecurity_level_no_meet = dplyr::case_when(
      subjective_insecurity_count_no_meet <= 1 ~
        "Low insecurity excluding making ends meet",
      subjective_insecurity_count_no_meet == 2 ~
        "Moderate insecurity excluding making ends meet",
      subjective_insecurity_count_no_meet >= 3 ~
        "High insecurity excluding making ends meet",
      TRUE ~ NA_character_
    ),
    
    subjective_insecurity_level_no_meet_f = factor(
      subjective_insecurity_level_no_meet,
      levels = c(
        "Low insecurity excluding making ends meet",
        "Moderate insecurity excluding making ends meet",
        "High insecurity excluding making ends meet"
      )
    ),
    
    high_subjective_insecurity_no_meet = dplyr::case_when(
      subjective_insecurity_level_no_meet_f ==
        "High insecurity excluding making ends meet" ~ 1,
      subjective_insecurity_level_no_meet_f %in% c(
        "Low insecurity excluding making ends meet",
        "Moderate insecurity excluding making ends meet"
      ) ~ 0,
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 07. Sample diagnostics
# ------------------------------------------------------------

sample_summary <- tibble::tibble(
  sample_definition = c(
    "Full Script 07 input data",
    "Valid PHQ-4 observed, expected and residual",
    "Valid residual z-score",
    "Valid high subjective insecurity",
    "Valid no-meet subjective insecurity",
    "Valid all specific subjective/economic-security dimensions"
  ),
  unweighted_n = c(
    nrow(analytic_07),
    sum(
      !is.na(analytic_07$phq4_score) &
        !is.na(analytic_07$phq4_expected_objective) &
        !is.na(analytic_07$phq4_residual_objective)
    ),
    sum(!is.na(analytic_07$phq4_residual_objective_z)),
    sum(!is.na(analytic_07$high_subjective_insecurity)),
    sum(!is.na(analytic_07$high_subjective_insecurity_no_meet)),
    sum(
      !is.na(analytic_07$financially_unprotected_health) &
        !is.na(analytic_07$no_emergency_money_support_binary) &
        !is.na(analytic_07$high_or_uncertain_job_loss_risk) &
        !is.na(analytic_07$debt_payment_problems_binary) &
        !is.na(analytic_07$difficulty_making_ends_meet_binary)
    )
  ),
  weighted_n = c(
    sum(analytic_07$weight, na.rm = TRUE),
    sum(
      analytic_07$weight[
        !is.na(analytic_07$phq4_score) &
          !is.na(analytic_07$phq4_expected_objective) &
          !is.na(analytic_07$phq4_residual_objective)
      ],
      na.rm = TRUE
    ),
    sum(
      analytic_07$weight[!is.na(analytic_07$phq4_residual_objective_z)],
      na.rm = TRUE
    ),
    sum(
      analytic_07$weight[!is.na(analytic_07$high_subjective_insecurity)],
      na.rm = TRUE
    ),
    sum(
      analytic_07$weight[!is.na(analytic_07$high_subjective_insecurity_no_meet)],
      na.rm = TRUE
    ),
    sum(
      analytic_07$weight[
        !is.na(analytic_07$financially_unprotected_health) &
          !is.na(analytic_07$no_emergency_money_support_binary) &
          !is.na(analytic_07$high_or_uncertain_job_loss_risk) &
          !is.na(analytic_07$debt_payment_problems_binary) &
          !is.na(analytic_07$difficulty_making_ends_meet_binary)
      ],
      na.rm = TRUE
    )
  )
)

variable_check <- tibble::tibble(
  variable = c(
    required_core_vars,
    "high_subjective_insecurity",
    "financially_unprotected_health",
    "no_emergency_money_support_binary",
    "high_or_uncertain_job_loss_risk",
    "high_job_loss_risk_only",
    "debt_payment_problems_binary",
    "difficulty_making_ends_meet_binary",
    "subjective_insecurity_count_no_meet",
    "subjective_insecurity_level_no_meet_f",
    "high_subjective_insecurity_no_meet"
  ),
  available = variable %in% names(analytic_07),
  n_valid = purrr::map_int(
    variable,
    ~ if (.x %in% names(analytic_07)) sum(!is.na(analytic_07[[.x]])) else NA_integer_
  ),
  n_missing = purrr::map_int(
    variable,
    ~ if (.x %in% names(analytic_07)) sum(is.na(analytic_07[[.x]])) else NA_integer_
  ),
  pct_missing = n_missing / nrow(analytic_07) * 100
)


# ------------------------------------------------------------
# 08. Residual-threshold sensitivity
# ------------------------------------------------------------

threshold_values <- c(0.50, 0.75, 1.00)

threshold_sensitivity_data <- purrr::map_dfr(
  threshold_values,
  ~ construct_expected_residual_configuration(analytic_07, .x)
)

threshold_configuration_distribution <- threshold_sensitivity_data %>%
  dplyr::filter(
    !is.na(threshold),
    !is.na(diagnostic_configuration_f),
    !is.na(weight)
  ) %>%
  dplyr::group_by(
    threshold,
    diagnostic_configuration_f,
    diagnostic_configuration_short_f
  ) %>%
  dplyr::summarise(
    unweighted_n = dplyr::n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_observed_phq4 = weighted_mean_approx(phq4_score, weight),
    mean_expected_phq4 = weighted_mean_approx(phq4_expected_objective, weight),
    mean_residual_phq4 = weighted_mean_approx(phq4_residual_objective, weight),
    pct_high_subjective_insecurity =
      100 * weighted_mean_approx(high_subjective_insecurity, weight),
    pct_high_subjective_insecurity_no_meet =
      100 * weighted_mean_approx(high_subjective_insecurity_no_meet, weight),
    pct_debt_payment_problems =
      100 * weighted_mean_approx(debt_payment_problems_binary, weight),
    pct_difficulty_making_ends_meet =
      100 * weighted_mean_approx(difficulty_making_ends_meet_binary, weight),
    .groups = "drop"
  ) %>%
  dplyr::group_by(threshold) %>%
  dplyr::mutate(
    weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    threshold = factor(
      threshold,
      levels = c("0.50 SD", "0.75 SD", "1.00 SD")
    )
  )

threshold_configuration_wide <- threshold_configuration_distribution %>%
  dplyr::select(
    threshold,
    diagnostic_configuration_f,
    weighted_pct,
    pct_high_subjective_insecurity,
    mean_residual_phq4
  ) %>%
  tidyr::pivot_wider(
    names_from = threshold,
    values_from = c(
      weighted_pct,
      pct_high_subjective_insecurity,
      mean_residual_phq4
    )
  )


# ------------------------------------------------------------
# 09. No-making-ends-meet subjective-insecurity sensitivity
# ------------------------------------------------------------

primary_configuration_data <- construct_expected_residual_configuration(
  analytic_07,
  threshold_value = 0.50
)

no_meet_insecurity_by_configuration <- primary_configuration_data %>%
  dplyr::filter(
    !is.na(diagnostic_configuration_f),
    !is.na(high_subjective_insecurity),
    !is.na(high_subjective_insecurity_no_meet),
    !is.na(weight)
  ) %>%
  dplyr::group_by(
    diagnostic_configuration_f,
    diagnostic_configuration_short_f
  ) %>%
  dplyr::summarise(
    unweighted_n = dplyr::n(),
    weighted_n = sum(weight, na.rm = TRUE),
    pct_high_subjective_insecurity_primary =
      100 * weighted_mean_approx(high_subjective_insecurity, weight),
    pct_high_subjective_insecurity_no_meet =
      100 * weighted_mean_approx(high_subjective_insecurity_no_meet, weight),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    difference_no_meet_minus_primary =
      pct_high_subjective_insecurity_no_meet -
      pct_high_subjective_insecurity_primary
  )

no_meet_insecurity_long <- no_meet_insecurity_by_configuration %>%
  dplyr::select(
    diagnostic_configuration_f,
    diagnostic_configuration_short_f,
    pct_high_subjective_insecurity_primary,
    pct_high_subjective_insecurity_no_meet
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      pct_high_subjective_insecurity_primary,
      pct_high_subjective_insecurity_no_meet
    ),
    names_to = "definition",
    values_to = "weighted_pct"
  ) %>%
  dplyr::mutate(
    definition = dplyr::recode(
      definition,
      pct_high_subjective_insecurity_primary =
        "Primary subjective insecurity",
      pct_high_subjective_insecurity_no_meet =
        "Excluding making ends meet"
    ),
    definition = factor(
      definition,
      levels = c(
        "Primary subjective insecurity",
        "Excluding making ends meet"
      )
    )
  )


# ------------------------------------------------------------
# 10. Survey design for model-based sensitivity checks
# ------------------------------------------------------------

model_data <- analytic_07 %>%
  dplyr::filter(
    !is.na(phq4_residual_objective),
    !is.na(higher_than_expected),
    !is.na(weight),
    !is.na(strata),
    !is.na(psu)
  )

design_model <- survey::svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = model_data,
  nest = TRUE
)


# ------------------------------------------------------------
# 11. Residual PHQ-4 model sensitivity
# ------------------------------------------------------------

prospective_terms <- c(
  "health_financial_protection_3cat_f",
  "no_emergency_money_support_f",
  "employment_risk_position_f"
)

current_strain_terms <- c(
  "debt_status_f",
  "making_ends_meet_3cat_f"
)

full_subjective_terms <- c(
  prospective_terms,
  current_strain_terms
)

full_subjective_no_meet_terms <- c(
  prospective_terms,
  "debt_status_f"
)

residual_model_formulas <- list(
  "Prospective protection/threat" =
    make_formula("phq4_residual_objective", prospective_terms),
  "Current financial strain" =
    make_formula("phq4_residual_objective", current_strain_terms),
  "Full subjective-security model" =
    make_formula("phq4_residual_objective", full_subjective_terms),
  "Full model excluding making ends meet" =
    make_formula("phq4_residual_objective", full_subjective_no_meet_terms)
)

residual_models <- purrr::imap(
  residual_model_formulas,
  ~ safe_svyglm(.x, design_model, family = gaussian())
)

residual_model_coefficients <- purrr::imap_dfr(
  residual_models,
  ~ if (is.null(.x)) {
    tibble::tibble()
  } else {
    tidy_svyglm(.x, .y, exponentiate = FALSE)
  }
) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    term_clean = term,
    term_clean = stringr::str_replace_all(term_clean, "`", ""),
    term_clean = stringr::str_replace_all(term_clean, "_", " "),
    model = factor(
      model,
      levels = c(
        "Prospective protection/threat",
        "Current financial strain",
        "Full subjective-security model",
        "Full model excluding making ends meet"
      )
    )
  )

residual_model_fit <- purrr::imap_dfr(
  residual_models,
  ~ model_fit_summary(
    model = .x,
    data = model_data,
    outcome_var = "phq4_residual_objective",
    model_label = .y
  )
) %>%
  dplyr::mutate(
    model = factor(
      model,
      levels = c(
        "Prospective protection/threat",
        "Current financial strain",
        "Full subjective-security model",
        "Full model excluding making ends meet"
      )
    )
  )


# ------------------------------------------------------------
# 12. Logistic residual-excess model sensitivity
# ------------------------------------------------------------

logit_model_formulas <- list(
  "Prospective protection/threat" =
    make_formula("higher_than_expected", prospective_terms),
  "Current financial strain" =
    make_formula("higher_than_expected", current_strain_terms),
  "Full subjective-security model" =
    make_formula("higher_than_expected", full_subjective_terms),
  "Full model excluding making ends meet" =
    make_formula("higher_than_expected", full_subjective_no_meet_terms)
)

logit_models <- purrr::imap(
  logit_model_formulas,
  ~ safe_svyglm(.x, design_model, family = quasibinomial())
)

logit_model_coefficients <- purrr::imap_dfr(
  logit_models,
  ~ if (is.null(.x)) {
    tibble::tibble()
  } else {
    tidy_svyglm(.x, .y, exponentiate = TRUE)
  }
) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    term_clean = term,
    term_clean = stringr::str_replace_all(term_clean, "`", ""),
    term_clean = stringr::str_replace_all(term_clean, "_", " "),
    model = factor(
      model,
      levels = c(
        "Prospective protection/threat",
        "Current financial strain",
        "Full subjective-security model",
        "Full model excluding making ends meet"
      )
    )
  )


# ------------------------------------------------------------
# 13. Objective-model sensitivity excluding sex
# ------------------------------------------------------------

objective_terms_primary <- c(
  "income_group_3cat_f",
  "education_3cat_f",
  "housing_tenure_3cat_f",
  "life_labor_stage_f",
  "health_system_3cat_f",
  "housing_deprivation_3cat_f",
  "territory_insecurity_3cat_f",
  "sex_f"
)

macrozone_candidates <- c("macrozone_f", "macrozone", "region_f")
macrozone_var <- macrozone_candidates[macrozone_candidates %in% names(analytic_07)][1]

if (!is.na(macrozone_var) && length(macrozone_var) > 0) {
  objective_terms_primary <- c(objective_terms_primary, macrozone_var)
}

objective_terms_no_sex <- setdiff(objective_terms_primary, "sex_f")
objective_vars_available <- all(objective_terms_no_sex %in% names(analytic_07))

if (objective_vars_available) {
  no_sex_model_data <- analytic_07 %>%
    dplyr::filter(
      !is.na(phq4_score),
      !is.na(weight),
      !is.na(strata),
      !is.na(psu)
    ) %>%
    tidyr::drop_na(dplyr::all_of(objective_terms_no_sex))
  
  design_no_sex <- survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~weight,
    data = no_sex_model_data,
    nest = TRUE
  )
  
  formula_no_sex <- stats::as.formula(
    paste0(
      "phq4_score ~ ",
      paste(objective_terms_no_sex, collapse = " + ")
    )
  )
  
  model_no_sex <- safe_svyglm(
    formula_no_sex,
    design_no_sex,
    family = gaussian()
  )
  
  if (!is.null(model_no_sex)) {
    no_sex_model_data <- no_sex_model_data %>%
      dplyr::mutate(
        phq4_expected_objective_no_sex =
          as.numeric(stats::predict(model_no_sex, newdata = no_sex_model_data, type = "response")),
        phq4_residual_objective_no_sex =
          phq4_score - phq4_expected_objective_no_sex,
        phq4_residual_objective_no_sex_z =
          as.numeric(scale(phq4_residual_objective_no_sex)),
        higher_than_expected_no_sex =
          dplyr::case_when(
            phq4_residual_objective_no_sex_z >= 0.50 ~ 1,
            !is.na(phq4_residual_objective_no_sex_z) ~ 0,
            TRUE ~ NA_real_
          ),
        lower_than_expected_no_sex =
          dplyr::case_when(
            phq4_residual_objective_no_sex_z <= -0.50 ~ 1,
            !is.na(phq4_residual_objective_no_sex_z) ~ 0,
            TRUE ~ NA_real_
          )
      )
    
    no_sex_comparison <- no_sex_model_data %>%
      dplyr::filter(
        !is.na(phq4_expected_objective),
        !is.na(phq4_expected_objective_no_sex),
        !is.na(phq4_residual_objective),
        !is.na(phq4_residual_objective_no_sex),
        !is.na(higher_than_expected),
        !is.na(higher_than_expected_no_sex)
      ) %>%
      dplyr::summarise(
        unweighted_n = dplyr::n(),
        weighted_n = sum(weight, na.rm = TRUE),
        correlation_expected = stats::cor(
          phq4_expected_objective,
          phq4_expected_objective_no_sex,
          use = "complete.obs"
        ),
        correlation_residual = stats::cor(
          phq4_residual_objective,
          phq4_residual_objective_no_sex,
          use = "complete.obs"
        ),
        pct_same_higher_than_expected =
          100 * weighted_mean_approx(
            as.numeric(higher_than_expected == higher_than_expected_no_sex),
            weight
          ),
        pct_higher_primary =
          100 * weighted_mean_approx(higher_than_expected, weight),
        pct_higher_no_sex =
          100 * weighted_mean_approx(higher_than_expected_no_sex, weight)
      )
    
    no_sex_scatter_data <- no_sex_model_data %>%
      dplyr::select(
        phq4_expected_objective,
        phq4_expected_objective_no_sex,
        phq4_residual_objective,
        phq4_residual_objective_no_sex,
        weight
      ) %>%
      dplyr::filter(
        !is.na(phq4_expected_objective),
        !is.na(phq4_expected_objective_no_sex),
        !is.na(phq4_residual_objective),
        !is.na(phq4_residual_objective_no_sex)
      )
  } else {
    no_sex_comparison <- tibble::tibble(
      status = "No-sex objective model failed"
    )
    no_sex_scatter_data <- tibble::tibble()
  }
} else {
  no_sex_comparison <- tibble::tibble(
    status = "No-sex objective model skipped because objective variables were missing",
    missing_variables = paste(
      setdiff(objective_terms_no_sex, names(analytic_07)),
      collapse = ", "
    )
  )
  no_sex_scatter_data <- tibble::tibble()
}


# ------------------------------------------------------------
# 14. Direct PHQ-4 robustness
# ------------------------------------------------------------

objective_terms_direct <- objective_terms_primary[
  objective_terms_primary %in% names(analytic_07)
]

direct_phq4_required_vars <- c(
  "phq4_score",
  objective_terms_direct,
  full_subjective_terms,
  "weight",
  "strata",
  "psu"
)

direct_phq4_missing_vars <- setdiff(
  direct_phq4_required_vars,
  names(analytic_07)
)

if (length(direct_phq4_missing_vars) == 0) {
  direct_phq4_data <- analytic_07 %>%
    dplyr::select(dplyr::all_of(direct_phq4_required_vars)) %>%
    tidyr::drop_na()
  
  direct_phq4_design <- survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~weight,
    data = direct_phq4_data,
    nest = TRUE
  )
  
  direct_m_objective <- safe_svyglm(
    make_formula("phq4_score", objective_terms_direct, names(direct_phq4_data)),
    direct_phq4_design,
    family = gaussian()
  )
  
  direct_m_objective_prospective <- safe_svyglm(
    make_formula(
      "phq4_score",
      c(objective_terms_direct, prospective_terms),
      names(direct_phq4_data)
    ),
    direct_phq4_design,
    family = gaussian()
  )
  
  direct_m_objective_prospective_debt <- safe_svyglm(
    make_formula(
      "phq4_score",
      c(objective_terms_direct, prospective_terms, "debt_status_f"),
      names(direct_phq4_data)
    ),
    direct_phq4_design,
    family = gaussian()
  )
  
  direct_m_full <- safe_svyglm(
    make_formula(
      "phq4_score",
      c(objective_terms_direct, full_subjective_terms),
      names(direct_phq4_data)
    ),
    direct_phq4_design,
    family = gaussian()
  )
  
  direct_phq4_model_fit <- dplyr::bind_rows(
    model_fit_summary_with_resid_sd(
      direct_m_objective,
      direct_phq4_data,
      "phq4_score",
      "Objective-position model, complete-case sample"
    ),
    model_fit_summary_with_resid_sd(
      direct_m_objective_prospective,
      direct_phq4_data,
      "phq4_score",
      "+ prospective protection/threat indicators"
    ),
    model_fit_summary_with_resid_sd(
      direct_m_objective_prospective_debt,
      direct_phq4_data,
      "phq4_score",
      "+ debt-payment situation"
    ),
    model_fit_summary_with_resid_sd(
      direct_m_full,
      direct_phq4_data,
      "phq4_score",
      "+ making ends meet / full economic-security model"
    )
  ) %>%
    dplyr::mutate(
      r2_percent = 100 * r2_weighted,
      delta_r2_previous_pp = r2_percent - dplyr::lag(r2_percent),
      delta_r2_from_objective_pp = r2_percent - dplyr::first(r2_percent)
    )
  
  direct_phq4_economic_security_coefficients <- tidy_svyglm(
    direct_m_full,
    model_label = "Direct full PHQ-4 model",
    exponentiate = FALSE
  ) %>%
    keep_economic_security_terms() %>%
    dplyr::mutate(
      robustness_check = "Direct PHQ-4 model"
    ) %>%
    dplyr::relocate(robustness_check, .before = model)
  
} else {
  direct_phq4_model_fit <- tibble::tibble(
    status = "Direct PHQ-4 robustness skipped because required variables were missing.",
    missing_variables = paste(direct_phq4_missing_vars, collapse = ", ")
  )
  
  direct_phq4_economic_security_coefficients <- tibble::tibble(
    status = "Direct PHQ-4 coefficients not estimated because required variables were missing.",
    missing_variables = paste(direct_phq4_missing_vars, collapse = ", ")
  )
  
  direct_m_objective <- NULL
  direct_m_objective_prospective <- NULL
  direct_m_objective_prospective_debt <- NULL
  direct_m_full <- NULL
}


# ------------------------------------------------------------
# 15. Debt-adjusted residual robustness
# ------------------------------------------------------------

debt_adjusted_predictors <- c(
  prospective_terms,
  "making_ends_meet_3cat_f"
)

debt_adjusted_required_vars <- c(
  "phq4_score",
  objective_terms_direct,
  "debt_status_f",
  debt_adjusted_predictors,
  "weight",
  "strata",
  "psu"
)

debt_adjusted_missing_vars <- setdiff(
  debt_adjusted_required_vars,
  names(analytic_07)
)

if (length(debt_adjusted_missing_vars) == 0) {
  debt_adjusted_data <- analytic_07 %>%
    dplyr::select(dplyr::all_of(debt_adjusted_required_vars)) %>%
    tidyr::drop_na()
  
  debt_adjusted_design <- survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~weight,
    data = debt_adjusted_data,
    nest = TRUE
  )
  
  debt_adjusted_m_objective <- safe_svyglm(
    make_formula("phq4_score", objective_terms_direct, names(debt_adjusted_data)),
    debt_adjusted_design,
    family = gaussian()
  )
  
  debt_adjusted_m_objective_plus_debt <- safe_svyglm(
    make_formula(
      "phq4_score",
      c(objective_terms_direct, "debt_status_f"),
      names(debt_adjusted_data)
    ),
    debt_adjusted_design,
    family = gaussian()
  )
  
  if (!is.null(debt_adjusted_m_objective_plus_debt)) {
    debt_adjusted_data <- debt_adjusted_data %>%
      dplyr::mutate(
        phq4_expected_debt_adjusted =
          as.numeric(stats::predict(
            debt_adjusted_m_objective_plus_debt,
            newdata = debt_adjusted_data,
            type = "response"
          )),
        phq4_residual_debt_adjusted =
          phq4_score - phq4_expected_debt_adjusted
      )
    
    debt_adjusted_residual_design <- survey::svydesign(
      ids = ~psu,
      strata = ~strata,
      weights = ~weight,
      data = debt_adjusted_data,
      nest = TRUE
    )
    
    debt_adjusted_m_residual <- safe_svyglm(
      make_formula(
        "phq4_residual_debt_adjusted",
        debt_adjusted_predictors,
        names(debt_adjusted_data)
      ),
      debt_adjusted_residual_design,
      family = gaussian()
    )
    
    debt_adjusted_model_fit <- dplyr::bind_rows(
      model_fit_summary_with_resid_sd(
        debt_adjusted_m_objective,
        debt_adjusted_data,
        "phq4_score",
        "Objective-position model, debt-adjusted complete-case sample"
      ),
      model_fit_summary_with_resid_sd(
        debt_adjusted_m_objective_plus_debt,
        debt_adjusted_data,
        "phq4_score",
        "Objective-position model + debt-payment situation"
      )
    ) %>%
      dplyr::mutate(
        r2_percent = 100 * r2_weighted,
        delta_r2_previous_pp = r2_percent - dplyr::lag(r2_percent),
        delta_r2_from_objective_pp = r2_percent - dplyr::first(r2_percent)
      )
    
    debt_adjusted_residual_coefficients <- tidy_svyglm(
      debt_adjusted_m_residual,
      model_label = "Debt-adjusted residual PHQ-4 model",
      exponentiate = FALSE
    ) %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(
        robustness_check = "Debt-adjusted residual model"
      ) %>%
      dplyr::relocate(robustness_check, .before = model)
    
  } else {
    debt_adjusted_model_fit <- tibble::tibble(
      status = "Debt-adjusted expected-PHQ-4 model failed."
    )
    
    debt_adjusted_residual_coefficients <- tibble::tibble(
      status = "Debt-adjusted residual model was not estimated because the expected-PHQ-4 model failed."
    )
    
    debt_adjusted_m_residual <- NULL
  }
  
} else {
  debt_adjusted_model_fit <- tibble::tibble(
    status = "Debt-adjusted residual robustness skipped because required variables were missing.",
    missing_variables = paste(debt_adjusted_missing_vars, collapse = ", ")
  )
  
  debt_adjusted_residual_coefficients <- tibble::tibble(
    status = "Debt-adjusted residual coefficients not estimated because required variables were missing.",
    missing_variables = paste(debt_adjusted_missing_vars, collapse = ", ")
  )
  
  debt_adjusted_m_objective <- NULL
  debt_adjusted_m_objective_plus_debt <- NULL
  debt_adjusted_m_residual <- NULL
}


# ------------------------------------------------------------
# 16. Figures
# ------------------------------------------------------------

p_threshold_distribution <- ggplot2::ggplot(
  threshold_configuration_distribution,
  ggplot2::aes(
    x = threshold,
    y = weighted_pct,
    fill = diagnostic_configuration_f
  )
) +
  ggplot2::geom_col(width = 0.72) +
  ggplot2::scale_fill_manual(
    values = configuration_palette,
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title = "Configuration distribution under alternative residual thresholds",
    subtitle = "Thresholds define higher-than-expected distress using residual PHQ-4 z-scores",
    x = "Residual-excess threshold",
    y = "Weighted percentage",
    fill = "Configuration"
  ) +
  main_theme(base_size = 12)

p_threshold_high_subjective <- ggplot2::ggplot(
  threshold_configuration_distribution,
  ggplot2::aes(
    x = diagnostic_configuration_short_f,
    y = pct_high_subjective_insecurity,
    color = threshold,
    group = threshold
  )
) +
  ggplot2::geom_point(size = 3.0) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_color_manual(
    values = threshold_palette,
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title = "High subjective insecurity by configuration under alternative residual thresholds",
    subtitle = "Subjective insecurity is not used to define configurations",
    x = NULL,
    y = "Weighted percentage with high subjective insecurity",
    color = "Threshold"
  ) +
  main_theme(base_size = 12) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
  )

p_no_meet_insecurity <- ggplot2::ggplot(
  no_meet_insecurity_long,
  ggplot2::aes(
    x = weighted_pct,
    y = forcats::fct_rev(diagnostic_configuration_short_f),
    color = definition
  )
) +
  ggplot2::geom_point(
    size = 3.6,
    position = ggplot2::position_dodge(width = 0.45)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Primary subjective insecurity" = unname(article_palette["mustard_dark"]),
      "Excluding making ends meet" = unname(article_palette["purple_dark"])
    )
  ) +
  ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title = "Subjective-insecurity classification with and without making ends meet",
    subtitle = "Sensitivity check for the subjective-insecurity index",
    x = "Weighted percentage with high insecurity",
    y = NULL,
    color = "Definition"
  ) +
  main_theme(base_size = 12)

plot_residual_coefficients <- residual_model_coefficients %>%
  dplyr::filter(
    !stringr::str_detect(term_clean, "Outside current employment context")
  ) %>%
  dplyr::mutate(
    term_clean = stringr::str_replace_all(
      term_clean,
      "health financial protection 3cat f",
      "Health emergency: "
    ),
    term_clean = stringr::str_replace_all(
      term_clean,
      "no emergency money support f",
      "Emergency support: "
    ),
    term_clean = stringr::str_replace_all(
      term_clean,
      "employment risk position f",
      "Employment risk: "
    ),
    term_clean = stringr::str_replace_all(
      term_clean,
      "debt status f",
      "Debt: "
    ),
    term_clean = stringr::str_replace_all(
      term_clean,
      "making ends meet 3cat f",
      "Making ends meet: "
    ),
    term_clean = stringr::str_squish(term_clean)
  )

p_residual_coefficients <- ggplot2::ggplot(
  plot_residual_coefficients,
  ggplot2::aes(
    x = estimate,
    y = forcats::fct_reorder(term_clean, estimate),
    xmin = conf_low,
    xmax = conf_high,
    color = model
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.75,
    color = unname(article_palette["grey_mid"])
  ) +
  ggplot2::geom_pointrange(
    position = ggplot2::position_dodge(width = 0.55),
    linewidth = 0.65
  ) +
  ggplot2::scale_color_manual(
    values = subjective_block_palette,
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Residual PHQ-4 model sensitivity",
    subtitle = "Survey-weighted linear models; outcome is observed minus objective-position expected PHQ-4",
    x = "Association with residual PHQ-4",
    y = NULL,
    color = "Model"
  ) +
  main_theme(base_size = 11)

if (nrow(no_sex_scatter_data) > 0) {
  p_no_sex_residual_comparison <- ggplot2::ggplot(
    no_sex_scatter_data,
    ggplot2::aes(
      x = phq4_residual_objective,
      y = phq4_residual_objective_no_sex
    )
  ) +
    ggplot2::geom_point(
      alpha = 0.20,
      size = 1.1,
      color = unname(article_palette["purple_dark"])
    ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linewidth = 0.8,
      color = unname(article_palette["grey_dark"])
    ) +
    ggplot2::labs(
      title = "Residual PHQ-4 comparison with and without sex in the objective model",
      subtitle = "Diagonal line indicates identical residual values",
      x = "Primary objective residual",
      y = "Objective residual excluding sex"
    ) +
    main_theme(base_size = 12)
} else {
  p_no_sex_residual_comparison <- NULL
}


# ------------------------------------------------------------
# 17. Export outputs
# ------------------------------------------------------------

output_excel <- file.path(
  script_tables_dir,
  "07_robustness_sensitivity_tables.xlsx"
)

excel_outputs <- list(
  sample_summary = sample_summary,
  variable_check = variable_check,
  threshold_distribution = threshold_configuration_distribution,
  threshold_wide = threshold_configuration_wide,
  no_meet_insecurity = no_meet_insecurity_by_configuration,
  residual_model_fit = residual_model_fit,
  residual_model_coeff = residual_model_coefficients,
  logit_model_coeff = logit_model_coefficients,
  no_sex_comparison = no_sex_comparison,
  direct_phq4_model_fit = direct_phq4_model_fit,
  direct_phq4_coefficients = direct_phq4_economic_security_coefficients,
  debt_adjusted_model_fit = debt_adjusted_model_fit,
  debt_adjusted_residual_coeff = debt_adjusted_residual_coefficients
)

openxlsx::write.xlsx(
  excel_outputs,
  file = output_excel,
  overwrite = TRUE
)

readr::write_csv(
  sample_summary,
  file.path(script_csv_dir, "07_sample_summary.csv")
)

readr::write_csv(
  variable_check,
  file.path(script_csv_dir, "07_variable_check.csv")
)

readr::write_csv(
  threshold_configuration_distribution,
  file.path(script_csv_dir, "07_threshold_configuration_distribution.csv")
)

readr::write_csv(
  threshold_configuration_wide,
  file.path(script_csv_dir, "07_threshold_configuration_wide.csv")
)

readr::write_csv(
  no_meet_insecurity_by_configuration,
  file.path(script_csv_dir, "07_no_meet_insecurity_by_configuration.csv")
)

readr::write_csv(
  residual_model_fit,
  file.path(script_csv_dir, "07_residual_model_fit.csv")
)

readr::write_csv(
  residual_model_coefficients,
  file.path(script_csv_dir, "07_residual_model_coefficients.csv")
)

readr::write_csv(
  logit_model_coefficients,
  file.path(script_csv_dir, "07_logit_model_coefficients.csv")
)

readr::write_csv(
  no_sex_comparison,
  file.path(script_csv_dir, "07_no_sex_comparison.csv")
)

readr::write_csv(
  direct_phq4_model_fit,
  file.path(script_csv_dir, "07_direct_phq4_model_fit.csv")
)

readr::write_csv(
  direct_phq4_economic_security_coefficients,
  file.path(script_csv_dir, "07_direct_phq4_economic_security_coefficients.csv")
)

readr::write_csv(
  debt_adjusted_model_fit,
  file.path(script_csv_dir, "07_debt_adjusted_model_fit.csv")
)

readr::write_csv(
  debt_adjusted_residual_coefficients,
  file.path(script_csv_dir, "07_debt_adjusted_residual_coefficients.csv")
)

analytic_07_path <- file.path(
  script_rds_dir,
  "07_robustness_sensitivity_dataset.rds"
)

saveRDS(
  analytic_07,
  analytic_07_path
)

threshold_sensitivity_path <- file.path(
  script_rds_dir,
  "07_threshold_sensitivity_data.rds"
)

saveRDS(
  threshold_sensitivity_data,
  threshold_sensitivity_path
)

model_objects_path <- file.path(
  script_rds_dir,
  "07_model_objects.rds"
)

saveRDS(
  list(
    residual_models = residual_models,
    logit_models = logit_models,
    no_sex_model = if (exists("model_no_sex")) model_no_sex else NULL,
    direct_phq4_models = list(
      objective = direct_m_objective,
      objective_plus_prospective = direct_m_objective_prospective,
      objective_plus_prospective_debt = direct_m_objective_prospective_debt,
      full_direct = direct_m_full
    ),
    debt_adjusted_models = list(
      objective = debt_adjusted_m_objective,
      objective_plus_debt = debt_adjusted_m_objective_plus_debt,
      debt_adjusted_residual = debt_adjusted_m_residual
    )
  ),
  model_objects_path
)

metadata_07 <- list(
  script_name = script_name,
  input_rds = input_rds,
  threshold_values = threshold_values,
  configuration_levels = configuration_levels,
  robustness_checks = c(
    "Residual-threshold sensitivity",
    "Subjective-insecurity index excluding making ends meet",
    "Objective-position model excluding sex",
    "Residual PHQ-4 model blocks",
    "Logistic higher-than-expected distress models",
    "Direct PHQ-4 model with objective-position and economic-security indicators",
    "Debt-adjusted expected PHQ-4 and residual model"
  ),
  note = "Script 07 provides robustness checks only. It does not replace the primary expected-residual configuration analysis from script 06."
)

metadata_07_path <- file.path(
  script_rds_dir,
  "07_robustness_sensitivity_metadata.rds"
)

saveRDS(
  metadata_07,
  metadata_07_path
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "07_threshold_sensitivity_configuration_distribution.png"),
  plot = p_threshold_distribution,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "07_threshold_sensitivity_high_subjective_insecurity.png"),
  plot = p_threshold_high_subjective,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "07_no_meet_insecurity_by_configuration.png"),
  plot = p_no_meet_insecurity,
  width = 13,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "07_residual_model_sensitivity_coefficients.png"),
  plot = p_residual_coefficients,
  width = 14,
  height = 9,
  dpi = 300,
  bg = "white"
)

if (!is.null(p_no_sex_residual_comparison)) {
  ggplot2::ggsave(
    filename = file.path(script_figures_dir, "07_no_sex_residual_comparison.png"),
    plot = p_no_sex_residual_comparison,
    width = 9,
    height = 8,
    dpi = 300,
    bg = "white"
  )
}

manifest <- tibble::tibble(
  output = c(
    "Excel robustness tables",
    "Script 07 analytic dataset",
    "Threshold sensitivity dataset",
    "Model objects",
    "Metadata",
    "Threshold sensitivity distribution figure",
    "Threshold sensitivity high subjective insecurity figure",
    "No-making-ends-meet sensitivity figure",
    "Residual model sensitivity coefficients figure",
    "No-sex residual comparison figure",
    "Direct PHQ-4 model fit CSV",
    "Direct PHQ-4 economic-security coefficients CSV",
    "Debt-adjusted model fit CSV",
    "Debt-adjusted residual coefficients CSV"
  ),
  path = c(
    output_excel,
    analytic_07_path,
    threshold_sensitivity_path,
    model_objects_path,
    metadata_07_path,
    file.path(script_figures_dir, "07_threshold_sensitivity_configuration_distribution.png"),
    file.path(script_figures_dir, "07_threshold_sensitivity_high_subjective_insecurity.png"),
    file.path(script_figures_dir, "07_no_meet_insecurity_by_configuration.png"),
    file.path(script_figures_dir, "07_residual_model_sensitivity_coefficients.png"),
    if (!is.null(p_no_sex_residual_comparison)) {
      file.path(script_figures_dir, "07_no_sex_residual_comparison.png")
    } else {
      "Not generated; no-sex objective model was skipped or failed"
    },
    file.path(script_csv_dir, "07_direct_phq4_model_fit.csv"),
    file.path(script_csv_dir, "07_direct_phq4_economic_security_coefficients.csv"),
    file.path(script_csv_dir, "07_debt_adjusted_model_fit.csv"),
    file.path(script_csv_dir, "07_debt_adjusted_residual_coefficients.csv")
  )
)

readr::write_csv(
  manifest,
  file.path(script_csv_dir, "07_output_manifest.csv")
)


# ------------------------------------------------------------
# 18. Save session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(script_logs_dir, "07_session_info.txt")
)


# ------------------------------------------------------------
# 19. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("07_robustness_sensitivity_checks.R completed")
message("============================================================")
message("Input file: ", input_rds)
message("Output folder: ", script_output_dir)
message("Residual thresholds checked: ", paste(threshold_values, collapse = ", "))
message("Additional robustness checks exported:")
message(" - Direct PHQ-4 model with objective-position and economic-security indicators")
message(" - Debt-adjusted expected PHQ-4 and residual PHQ-4 model")
message("Excel diagnostics: ", output_excel)
message("Main dataset: ", analytic_07_path)
message("Figures saved in: ", script_figures_dir)
message("CSV outputs saved in: ", script_csv_dir)
message("RDS outputs saved in: ", script_rds_dir)
message("============================================================\n")
