# ============================================================
# 04_subjective_security_residual_associations.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Estimate associations between subjective economic security
#          and residual PHQ-4
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Setup
# ------------------------------------------------------------

source("scripts/00_setup.R")

options(survey.lonely.psu = "adjust")

script_name <- "04_subjective_security_residual_associations"

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
# 02. Load residual dataset from script 03
# ------------------------------------------------------------

input_rds <- file.path(
  output_dir,
  "03_objective_gradient_residual_phq4",
  "rds",
  "03_objective_gradient_residual_dataset.rds"
)

variable_groups_path <- file.path(
  output_dir,
  "01_data_preparation",
  "rds",
  "01_variable_groups.rds"
)

metadata_03_path <- file.path(
  output_dir,
  "03_objective_gradient_residual_phq4",
  "rds",
  "03_objective_model_metadata.rds"
)

if (!file.exists(input_rds)) {
  stop(
    paste0(
      "Input residual dataset not found: ", input_rds, "\n",
      "Run scripts/03_objective_gradient_residual_phq4.R first."
    )
  )
}

analytic <- readRDS(input_rds) %>%
  janitor::clean_names()

if (file.exists(variable_groups_path)) {
  variable_groups <- readRDS(variable_groups_path)
} else {
  variable_groups <- list()
  warning("Variable groups file not found. Proceeding with internal variable definitions.")
}

if (file.exists(metadata_03_path)) {
  metadata_03 <- readRDS(metadata_03_path)
} else {
  metadata_03 <- list()
  warning("Script 03 metadata file not found. Proceeding without it.")
}

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
# 03. Variable definitions
# ------------------------------------------------------------

primary_residual_outcome <- "phq4_residual_objective"
primary_observed_outcome <- "phq4_score"

required_residual_vars <- c(
  "phq4_score",
  "phq4_expected_objective",
  "phq4_residual_objective",
  "phq4_residual_objective_z",
  "phq4_residual_position_f",
  "higher_than_expected",
  "lower_than_expected",
  "weight",
  "strata",
  "psu"
)

missing_residual_vars <- setdiff(required_residual_vars, names(analytic))

if (length(missing_residual_vars) > 0) {
  stop(
    "Missing residual strategy variables from script 03: ",
    paste(missing_residual_vars, collapse = ", ")
  )
}

prospective_protection_threat_vars <- c(
  "health_financial_protection_3cat_f",
  "no_emergency_money_support_f",
  "employment_risk_position_f"
)

current_financial_strain_vars <- c(
  "debt_status_f",
  "making_ends_meet_3cat_f"
)

subjective_security_specific_vars <- c(
  prospective_protection_threat_vars,
  current_financial_strain_vars
)

subjective_security_summary_vars <- c(
  "subjective_insecurity_level_f",
  "prospective_insecurity_level_f",
  "current_financial_strain_level_f",
  "subjective_insecurity_count",
  "prospective_insecurity_count",
  "current_financial_strain_count"
)

objective_position_vars <- c(
  "income_group_3cat_f",
  "education_3cat_f",
  "housing_tenure_3cat_f",
  "life_labor_stage_f",
  "health_system_3cat_f",
  "housing_deprivation_3cat_f",
  "territory_insecurity_3cat_f",
  "sex_f",
  "macrozone_f"
)

subjective_security_specific_vars <- intersect(subjective_security_specific_vars, names(analytic))
prospective_protection_threat_vars <- intersect(prospective_protection_threat_vars, names(analytic))
current_financial_strain_vars <- intersect(current_financial_strain_vars, names(analytic))
subjective_security_summary_vars <- intersect(subjective_security_summary_vars, names(analytic))
objective_position_vars <- intersect(objective_position_vars, names(analytic))

missing_specific_vars <- setdiff(
  c(
    "health_financial_protection_3cat_f",
    "no_emergency_money_support_f",
    "employment_risk_position_f",
    "debt_status_f",
    "making_ends_meet_3cat_f"
  ),
  names(analytic)
)

if (length(missing_specific_vars) > 0) {
  stop(
    "Missing subjective-security variables required for the main residual models: ",
    paste(missing_specific_vars, collapse = ", ")
  )
}

subjective_variable_check <- tibble::tibble(
  analytic_block = c(
    rep("Prospective protection/threat", 3),
    rep("Current financial strain", 2),
    rep("Summary indicators", 6)
  ),
  variable = c(
    "health_financial_protection_3cat_f",
    "no_emergency_money_support_f",
    "employment_risk_position_f",
    "debt_status_f",
    "making_ends_meet_3cat_f",
    "subjective_insecurity_level_f",
    "prospective_insecurity_level_f",
    "current_financial_strain_level_f",
    "subjective_insecurity_count",
    "prospective_insecurity_count",
    "current_financial_strain_count"
  )
) %>%
  dplyr::mutate(
    available = variable %in% names(analytic),
    n_valid = purrr::map_int(
      variable,
      ~ if (.x %in% names(analytic)) sum(!is.na(analytic[[.x]])) else NA_integer_
    ),
    n_missing = purrr::map_int(
      variable,
      ~ if (.x %in% names(analytic)) sum(is.na(analytic[[.x]])) else NA_integer_
    ),
    pct_missing = n_missing / nrow(analytic) * 100
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

subjective_insecurity_palette <- c(
  "Low subjective insecurity" = unname(article_palette["mustard_light"]),
  "Moderate subjective insecurity" = unname(article_palette["mustard_mid"]),
  "High subjective insecurity" = unname(article_palette["mustard_dark"])
)

coefficient_block_palette <- c(
  "Prospective protection/threat" = unname(article_palette["purple_dark"]),
  "Current financial strain" = unname(article_palette["mustard_dark"]),
  "Other" = unname(article_palette["grey_mid"])
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

make_formula <- function(outcome, predictors) {
  if (length(predictors) == 0) {
    stats::as.formula(paste0(outcome, " ~ 1"))
  } else {
    stats::as.formula(
      paste0(outcome, " ~ ", paste(predictors, collapse = " + "))
    )
  }
}

weighted_quantile <- function(x, w, probs = c(0.10, 0.25, 0.50, 0.75, 0.90)) {
  ok <- !is.na(x) & !is.na(w)
  
  if (sum(ok) == 0) {
    return(rep(NA_real_, length(probs)))
  }
  
  x <- x[ok]
  w <- w[ok]
  
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  cumulative_weight <- cumsum(w) / sum(w)
  
  purrr::map_dbl(
    probs,
    ~ x[which(cumulative_weight >= .x)[1]]
  )
}

weighted_correlation <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w)
  
  if (sum(ok) < 5) {
    return(NA_real_)
  }
  
  x <- x[ok]
  y <- y[ok]
  w <- w[ok]
  w <- w / sum(w)
  
  mx <- sum(w * x)
  my <- sum(w * y)
  
  vx <- sum(w * (x - mx)^2)
  vy <- sum(w * (y - my)^2)
  cxy <- sum(w * (x - mx) * (y - my))
  
  if (vx <= 0 || vy <= 0) {
    return(NA_real_)
  }
  
  cxy / sqrt(vx * vy)
}

r2_weighted <- function(observed, predicted, weights) {
  ok <- !is.na(observed) & !is.na(predicted) & !is.na(weights)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  y <- observed[ok]
  yhat <- predicted[ok]
  w <- weights[ok]
  
  ybar <- stats::weighted.mean(y, w, na.rm = TRUE)
  
  sse <- sum(w * (y - yhat)^2, na.rm = TRUE)
  sst <- sum(w * (y - ybar)^2, na.rm = TRUE)
  
  if (sst <= 0) {
    return(NA_real_)
  }
  
  1 - sse / sst
}

rmse_weighted <- function(observed, predicted, weights) {
  ok <- !is.na(observed) & !is.na(predicted) & !is.na(weights)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  sqrt(
    stats::weighted.mean(
      (observed[ok] - predicted[ok])^2,
      weights[ok],
      na.rm = TRUE
    )
  )
}

mae_weighted <- function(observed, predicted, weights) {
  ok <- !is.na(observed) & !is.na(predicted) & !is.na(weights)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  stats::weighted.mean(
    abs(observed[ok] - predicted[ok]),
    weights[ok],
    na.rm = TRUE
  )
}

tidy_svyglm <- function(model_object, model_name, model_family = "gaussian") {
  out <- broom::tidy(model_object) %>%
    dplyr::mutate(
      model = model_name,
      model_family = model_family,
      conf_low = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error,
      p_value = p.value,
      .before = 1
    ) %>%
    dplyr::select(
      model,
      model_family,
      term,
      estimate,
      std.error,
      statistic,
      p_value,
      conf_low,
      conf_high
    )
  
  if (model_family %in% c("quasibinomial", "binomial")) {
    out <- out %>%
      dplyr::mutate(
        odds_ratio = exp(estimate),
        odds_ratio_low = exp(conf_low),
        odds_ratio_high = exp(conf_high)
      )
  }
  
  out
}

model_fit_summary <- function(model_object, data, outcome, model_name) {
  model_frame <- stats::model.frame(model_object)
  included_rows <- as.integer(rownames(model_frame))
  
  predicted <- rep(NA_real_, nrow(data))
  predicted[included_rows] <- as.numeric(stats::predict(model_object, type = "response"))
  
  observed <- data[[outcome]]
  weights <- data$weight
  
  tibble::tibble(
    model = model_name,
    n_unweighted = length(included_rows),
    weighted_n = sum(weights[included_rows], na.rm = TRUE),
    mean_observed = weighted_mean_approx(observed[included_rows], weights[included_rows]),
    mean_predicted = weighted_mean_approx(predicted[included_rows], weights[included_rows]),
    residual_sd = weighted_sd_approx(observed[included_rows] - predicted[included_rows], weights[included_rows]),
    r2_weighted = r2_weighted(observed, predicted, weights),
    rmse_weighted = rmse_weighted(observed, predicted, weights),
    mae_weighted = mae_weighted(observed, predicted, weights)
  )
}

weighted_mean_by_group <- function(data, value_var, group_var, weight_var = "weight") {
  if (!all(c(value_var, group_var, weight_var) %in% names(data))) {
    return(tibble::tibble())
  }
  
  temp <- data %>%
    dplyr::filter(
      !is.na(.data[[value_var]]),
      !is.na(.data[[group_var]]),
      !is.na(.data[[weight_var]])
    )
  
  if (nrow(temp) == 0) {
    return(tibble::tibble())
  }
  
  temp %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      weighted_mean = weighted_mean_approx(.data[[value_var]], .data[[weight_var]]),
      weighted_sd_approx = weighted_sd_approx(.data[[value_var]], .data[[weight_var]]),
      .groups = "drop"
    ) %>%
    dplyr::rename(group = 1) %>%
    dplyr::mutate(
      variable = value_var,
      group_var = group_var,
      group = as.character(group)
    ) %>%
    dplyr::select(
      variable,
      group_var,
      group,
      unweighted_n,
      weighted_n,
      weighted_mean,
      weighted_sd_approx
    )
}

weighted_pct_binary_by_group <- function(data, binary_var, group_var, weight_var = "weight") {
  if (!all(c(binary_var, group_var, weight_var) %in% names(data))) {
    return(tibble::tibble())
  }
  
  temp <- data %>%
    dplyr::filter(
      !is.na(.data[[binary_var]]),
      !is.na(.data[[group_var]]),
      !is.na(.data[[weight_var]])
    )
  
  if (nrow(temp) == 0) {
    return(tibble::tibble())
  }
  
  temp %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      weighted_pct = 100 * weighted_mean_approx(.data[[binary_var]], .data[[weight_var]]),
      .groups = "drop"
    ) %>%
    dplyr::rename(group = 1) %>%
    dplyr::mutate(
      variable = binary_var,
      group_var = group_var,
      group = as.character(group)
    ) %>%
    dplyr::select(
      variable,
      group_var,
      group,
      unweighted_n,
      weighted_n,
      weighted_pct
    )
}

clean_model_term <- function(term) {
  term %>%
    stringr::str_replace_all("health_financial_protection_3cat_f", "Health protection: ") %>%
    stringr::str_replace_all("no_emergency_money_support_f", "Emergency support: ") %>%
    stringr::str_replace_all("employment_risk_position_f", "Employment risk: ") %>%
    stringr::str_replace_all("debt_status_f", "Debt status: ") %>%
    stringr::str_replace_all("making_ends_meet_3cat_f", "Making ends meet: ") %>%
    stringr::str_replace_all("income_group_3cat_f", "Income: ") %>%
    stringr::str_replace_all("education_3cat_f", "Education: ") %>%
    stringr::str_replace_all("housing_tenure_3cat_f", "Housing tenure: ") %>%
    stringr::str_replace_all("life_labor_stage_f", "Life-labor stage: ") %>%
    stringr::str_replace_all("health_system_3cat_f", "Health system: ") %>%
    stringr::str_replace_all("housing_deprivation_3cat_f", "Housing deprivation: ") %>%
    stringr::str_replace_all("territory_insecurity_3cat_f", "Territorial insecurity: ") %>%
    stringr::str_replace_all("sex_f", "Sex: ") %>%
    stringr::str_replace_all("macrozone_f", "Macrozone: ") %>%
    stringr::str_replace_all("`", "") %>%
    stringr::str_squish()
}

term_block <- function(term) {
  dplyr::case_when(
    stringr::str_detect(
      term,
      "health_financial_protection_3cat_f|no_emergency_money_support_f|employment_risk_position_f"
    ) ~ "Prospective protection/threat",
    stringr::str_detect(
      term,
      "debt_status_f|making_ends_meet_3cat_f"
    ) ~ "Current financial strain",
    TRUE ~ "Other"
  )
}


# ------------------------------------------------------------
# 06. Prepare analysis dataset and survey design
# ------------------------------------------------------------

analytic_04 <- analytic %>%
  dplyr::mutate(
    residual_specific_model_complete = stats::complete.cases(
      dplyr::across(
        dplyr::all_of(
          unique(c(
            primary_residual_outcome,
            subjective_security_specific_vars,
            "weight",
            "strata",
            "psu"
          ))
        )
      )
    ),
    residual_summary_model_complete = stats::complete.cases(
      dplyr::across(
        dplyr::all_of(
          unique(c(
            primary_residual_outcome,
            subjective_security_summary_vars,
            "weight",
            "strata",
            "psu"
          ))
        )
      )
    ),
    logistic_higher_complete = stats::complete.cases(
      dplyr::across(
        dplyr::all_of(
          unique(c(
            "higher_than_expected",
            subjective_security_specific_vars,
            "weight",
            "strata",
            "psu"
          ))
        )
      )
    ),
    logistic_lower_complete = stats::complete.cases(
      dplyr::across(
        dplyr::all_of(
          unique(c(
            "lower_than_expected",
            subjective_security_specific_vars,
            "weight",
            "strata",
            "psu"
          ))
        )
      )
    )
  )

design_full <- survey::svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analytic_04,
  nest = TRUE
)

design_residual_specific <- subset(design_full, residual_specific_model_complete)
design_residual_summary <- subset(design_full, residual_summary_model_complete)
design_logistic_higher <- subset(design_full, logistic_higher_complete)
design_logistic_lower <- subset(design_full, logistic_lower_complete)

sample_summary <- tibble::tibble(
  sample_definition = c(
    "Full Script 04 input data",
    "Valid residual PHQ-4",
    "Complete residual model with specific subjective-security indicators",
    "Complete residual model with summary subjective-security indicators",
    "Complete logistic model: higher-than-expected distress",
    "Complete logistic model: lower-than-expected distress"
  ),
  unweighted_n = c(
    nrow(analytic_04),
    sum(!is.na(analytic_04[[primary_residual_outcome]])),
    sum(analytic_04$residual_specific_model_complete),
    sum(analytic_04$residual_summary_model_complete),
    sum(analytic_04$logistic_higher_complete),
    sum(analytic_04$logistic_lower_complete)
  ),
  weighted_n = c(
    sum(analytic_04$weight, na.rm = TRUE),
    sum(analytic_04$weight[!is.na(analytic_04[[primary_residual_outcome]])], na.rm = TRUE),
    sum(analytic_04$weight[analytic_04$residual_specific_model_complete], na.rm = TRUE),
    sum(analytic_04$weight[analytic_04$residual_summary_model_complete], na.rm = TRUE),
    sum(analytic_04$weight[analytic_04$logistic_higher_complete], na.rm = TRUE),
    sum(analytic_04$weight[analytic_04$logistic_lower_complete], na.rm = TRUE)
  )
)


# ------------------------------------------------------------
# 07. Main residual PHQ-4 models
# ------------------------------------------------------------

formula_r0 <- make_formula(primary_residual_outcome, character(0))

formula_r1 <- make_formula(
  primary_residual_outcome,
  prospective_protection_threat_vars
)

formula_r2 <- make_formula(
  primary_residual_outcome,
  current_financial_strain_vars
)

formula_r3 <- make_formula(
  primary_residual_outcome,
  subjective_security_specific_vars
)

model_r0 <- survey::svyglm(
  formula_r0,
  design = design_residual_specific,
  family = gaussian()
)

model_r1 <- survey::svyglm(
  formula_r1,
  design = design_residual_specific,
  family = gaussian()
)

model_r2 <- survey::svyglm(
  formula_r2,
  design = design_residual_specific,
  family = gaussian()
)

model_r3 <- survey::svyglm(
  formula_r3,
  design = design_residual_specific,
  family = gaussian()
)

residual_model_objects <- list(
  R0_intercept_only = model_r0,
  R1_prospective_protection_threat = model_r1,
  R2_current_financial_strain = model_r2,
  R3_full_subjective_security = model_r3
)

residual_model_coefficients <- dplyr::bind_rows(
  tidy_svyglm(model_r0, "R0 Intercept only", "gaussian"),
  tidy_svyglm(model_r1, "R1 Prospective protection/threat", "gaussian"),
  tidy_svyglm(model_r2, "R2 Current financial strain", "gaussian"),
  tidy_svyglm(model_r3, "R3 Full subjective security", "gaussian")
) %>%
  dplyr::mutate(
    term_clean = clean_model_term(term),
    block = term_block(term)
  )

residual_model_fit <- dplyr::bind_rows(
  model_fit_summary(model_r0, analytic_04, primary_residual_outcome, "R0 Intercept only"),
  model_fit_summary(model_r1, analytic_04, primary_residual_outcome, "R1 Prospective protection/threat"),
  model_fit_summary(model_r2, analytic_04, primary_residual_outcome, "R2 Current financial strain"),
  model_fit_summary(model_r3, analytic_04, primary_residual_outcome, "R3 Full subjective security")
) %>%
  dplyr::mutate(
    r2_change_from_previous = r2_weighted - dplyr::lag(r2_weighted),
    r2_change_from_r0 = r2_weighted - r2_weighted[model == "R0 Intercept only"],
    r2_weighted_pct = r2_weighted * 100,
    r2_change_from_previous_pp = r2_change_from_previous * 100,
    r2_change_from_r0_pp = r2_change_from_r0 * 100
  )


# ------------------------------------------------------------
# 08. Summary-indicator residual models
# ------------------------------------------------------------

formula_s1 <- make_formula(
  primary_residual_outcome,
  "subjective_insecurity_level_f"
)

formula_s2 <- make_formula(
  primary_residual_outcome,
  c("prospective_insecurity_level_f", "current_financial_strain_level_f")
)

model_s1 <- survey::svyglm(
  formula_s1,
  design = design_residual_summary,
  family = gaussian()
)

model_s2 <- survey::svyglm(
  formula_s2,
  design = design_residual_summary,
  family = gaussian()
)

summary_model_objects <- list(
  S1_subjective_insecurity_level = model_s1,
  S2_prospective_current_levels = model_s2
)

summary_model_coefficients <- dplyr::bind_rows(
  tidy_svyglm(model_s1, "S1 Subjective insecurity level", "gaussian"),
  tidy_svyglm(model_s2, "S2 Prospective and current levels", "gaussian")
) %>%
  dplyr::mutate(
    term_clean = clean_model_term(term)
  )

summary_model_fit <- dplyr::bind_rows(
  model_fit_summary(model_s1, analytic_04, primary_residual_outcome, "S1 Subjective insecurity level"),
  model_fit_summary(model_s2, analytic_04, primary_residual_outcome, "S2 Prospective and current levels")
) %>%
  dplyr::mutate(
    r2_weighted_pct = r2_weighted * 100
  )


# ------------------------------------------------------------
# 09. Logistic models: higher/lower than expected
# ------------------------------------------------------------

formula_l_higher <- make_formula(
  "higher_than_expected",
  subjective_security_specific_vars
)

formula_l_lower <- make_formula(
  "lower_than_expected",
  subjective_security_specific_vars
)

model_l_higher <- survey::svyglm(
  formula_l_higher,
  design = design_logistic_higher,
  family = quasibinomial()
)

model_l_lower <- survey::svyglm(
  formula_l_lower,
  design = design_logistic_lower,
  family = quasibinomial()
)

logistic_model_objects <- list(
  L1_higher_than_expected = model_l_higher,
  L2_lower_than_expected = model_l_lower
)

logistic_model_coefficients <- dplyr::bind_rows(
  tidy_svyglm(model_l_higher, "L1 Higher-than-expected distress", "quasibinomial"),
  tidy_svyglm(model_l_lower, "L2 Lower-than-expected distress", "quasibinomial")
) %>%
  dplyr::mutate(
    term_clean = clean_model_term(term),
    block = term_block(term)
  )


# ------------------------------------------------------------
# 10. Supplementary observed PHQ-4 model sequence
# ------------------------------------------------------------

observed_model_vars_o0 <- objective_position_vars

observed_model_vars_o1 <- c(
  objective_position_vars,
  prospective_protection_threat_vars
)

observed_model_vars_o2 <- c(
  observed_model_vars_o1,
  "debt_status_f"
)

observed_model_vars_o3 <- c(
  observed_model_vars_o2,
  "making_ends_meet_3cat_f"
)

observed_model_vars_o0 <- intersect(observed_model_vars_o0, names(analytic_04))
observed_model_vars_o1 <- intersect(observed_model_vars_o1, names(analytic_04))
observed_model_vars_o2 <- intersect(observed_model_vars_o2, names(analytic_04))
observed_model_vars_o3 <- intersect(observed_model_vars_o3, names(analytic_04))

observed_complete_vars <- unique(c(
  primary_observed_outcome,
  observed_model_vars_o3,
  "weight",
  "strata",
  "psu"
))

analytic_04 <- analytic_04 %>%
  dplyr::mutate(
    observed_supplementary_complete = stats::complete.cases(
      dplyr::across(dplyr::all_of(observed_complete_vars))
    )
  )

design_full <- survey::svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analytic_04,
  nest = TRUE
)

design_observed_supplementary <- subset(
  design_full,
  observed_supplementary_complete
)

formula_o0 <- make_formula(primary_observed_outcome, observed_model_vars_o0)
formula_o1 <- make_formula(primary_observed_outcome, observed_model_vars_o1)
formula_o2 <- make_formula(primary_observed_outcome, observed_model_vars_o2)
formula_o3 <- make_formula(primary_observed_outcome, observed_model_vars_o3)

model_o0 <- survey::svyglm(
  formula_o0,
  design = design_observed_supplementary,
  family = gaussian()
)

model_o1 <- survey::svyglm(
  formula_o1,
  design = design_observed_supplementary,
  family = gaussian()
)

model_o2 <- survey::svyglm(
  formula_o2,
  design = design_observed_supplementary,
  family = gaussian()
)

model_o3 <- survey::svyglm(
  formula_o3,
  design = design_observed_supplementary,
  family = gaussian()
)

observed_supplementary_model_objects <- list(
  O0_objective_position = model_o0,
  O1_objective_plus_prospective = model_o1,
  O2_plus_debt = model_o2,
  O3_plus_making_ends_meet = model_o3
)

observed_supplementary_coefficients <- dplyr::bind_rows(
  tidy_svyglm(model_o0, "O0 Objective position", "gaussian"),
  tidy_svyglm(model_o1, "O1 + prospective protection/threat", "gaussian"),
  tidy_svyglm(model_o2, "O2 + debt status", "gaussian"),
  tidy_svyglm(model_o3, "O3 + making ends meet", "gaussian")
) %>%
  dplyr::mutate(
    term_clean = clean_model_term(term),
    block = term_block(term)
  )

observed_supplementary_fit <- dplyr::bind_rows(
  model_fit_summary(model_o0, analytic_04, primary_observed_outcome, "O0 Objective position"),
  model_fit_summary(model_o1, analytic_04, primary_observed_outcome, "O1 + prospective protection/threat"),
  model_fit_summary(model_o2, analytic_04, primary_observed_outcome, "O2 + debt status"),
  model_fit_summary(model_o3, analytic_04, primary_observed_outcome, "O3 + making ends meet")
) %>%
  dplyr::mutate(
    r2_change_from_previous = r2_weighted - dplyr::lag(r2_weighted),
    r2_change_from_o0 = r2_weighted - r2_weighted[model == "O0 Objective position"],
    r2_weighted_pct = r2_weighted * 100,
    r2_change_from_previous_pp = r2_change_from_previous * 100,
    r2_change_from_o0_pp = r2_change_from_o0 * 100
  )


# ------------------------------------------------------------
# 11. Descriptive residual gradients by subjective security
# ------------------------------------------------------------

subjective_group_vars_for_descriptives <- intersect(
  c(
    "health_financial_protection_3cat_f",
    "no_emergency_money_support_f",
    "employment_risk_position_f",
    "debt_status_f",
    "making_ends_meet_3cat_f",
    "subjective_insecurity_level_f",
    "prospective_insecurity_level_f",
    "current_financial_strain_level_f"
  ),
  names(analytic_04)
)

subjective_group_labels <- c(
  health_financial_protection_3cat_f = "Health emergency protection",
  no_emergency_money_support_f = "Emergency monetary support",
  employment_risk_position_f = "Employment risk",
  debt_status_f = "Debt status",
  making_ends_meet_3cat_f = "Making ends meet",
  subjective_insecurity_level_f = "Subjective insecurity",
  prospective_insecurity_level_f = "Prospective insecurity",
  current_financial_strain_level_f = "Current financial strain"
)

residual_by_subjective_security <- purrr::map_dfr(
  subjective_group_vars_for_descriptives,
  function(group_var) {
    observed <- weighted_mean_by_group(
      analytic_04,
      "phq4_score",
      group_var,
      "weight"
    ) %>%
      dplyr::mutate(metric = "Observed PHQ-4")
    
    expected <- weighted_mean_by_group(
      analytic_04,
      "phq4_expected_objective",
      group_var,
      "weight"
    ) %>%
      dplyr::mutate(metric = "Expected PHQ-4")
    
    residual <- weighted_mean_by_group(
      analytic_04,
      "phq4_residual_objective",
      group_var,
      "weight"
    ) %>%
      dplyr::mutate(metric = "Residual PHQ-4")
    
    higher <- weighted_pct_binary_by_group(
      analytic_04,
      "higher_than_expected",
      group_var,
      "weight"
    ) %>%
      dplyr::rename(weighted_mean = weighted_pct) %>%
      dplyr::mutate(metric = "Higher-than-expected distress (%)")
    
    dplyr::bind_rows(observed, expected, residual, higher)
  }
) %>%
  dplyr::mutate(
    group_var_label = dplyr::recode(group_var, !!!subjective_group_labels, .default = group_var),
    metric = factor(
      metric,
      levels = c(
        "Observed PHQ-4",
        "Expected PHQ-4",
        "Residual PHQ-4",
        "Higher-than-expected distress (%)"
      )
    ),
    group_wrapped = stringr::str_wrap(group, 32)
  )

residual_summary_by_insecurity_level <- analytic_04 %>%
  dplyr::filter(
    !is.na(subjective_insecurity_level_f),
    !is.na(phq4_score),
    !is.na(phq4_expected_objective),
    !is.na(phq4_residual_objective),
    !is.na(weight)
  ) %>%
  dplyr::group_by(subjective_insecurity_level_f) %>%
  dplyr::summarise(
    unweighted_n = dplyr::n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_observed_phq4 = weighted_mean_approx(phq4_score, weight),
    mean_expected_phq4 = weighted_mean_approx(phq4_expected_objective, weight),
    mean_residual_phq4 = weighted_mean_approx(phq4_residual_objective, weight),
    mean_residual_phq4_z = weighted_mean_approx(phq4_residual_objective_z, weight),
    pct_higher_than_expected = 100 * weighted_mean_approx(higher_than_expected, weight),
    pct_lower_than_expected = 100 * weighted_mean_approx(lower_than_expected, weight),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    weighted_pct = 100 * weighted_n / sum(weighted_n)
  )


# ------------------------------------------------------------
# 12. Term labels for figures
# ------------------------------------------------------------

plot_term_labels <- c(
  "Health protection: Neither protected nor unprotected in health emergency" =
    "Health emergency: neither protected nor unprotected",
  "Health protection: Financially protected in health emergency" =
    "Health emergency: financially protected",
  "Emergency support: No emergency money support" =
    "No emergency monetary support",
  "Employment risk: Employed, uncertain perceived job-loss risk" =
    "Employed: uncertain job-loss risk",
  "Employment risk: Employed, high perceived job-loss risk" =
    "Employed: high job-loss risk",
  "Employment risk: Outside current employment context" =
    "Outside current employment context",
  "Debt status: Debt, all payments on time" =
    "Debt: all payments on time",
  "Debt status: Debt, some payment problems" =
    "Debt: some payment problems",
  "Debt status: Debt, no payments on time" =
    "Debt: no payments on time",
  "Making ends meet: Neither difficulty nor ease" =
    "Making ends meet: neither difficulty nor ease",
  "Making ends meet: Ease making ends meet" =
    "Making ends meet: ease"
)

subjective_insecurity_short_labels <- c(
  "Low subjective insecurity" = "Low",
  "Moderate subjective insecurity" = "Moderate",
  "High subjective insecurity" = "High"
)


# ------------------------------------------------------------
# 13. Figures
# ------------------------------------------------------------

residual_coefficients_plot_data <- residual_model_coefficients %>%
  dplyr::filter(
    model == "R3 Full subjective security",
    term != "(Intercept)"
  ) %>%
  dplyr::mutate(
    term_label = dplyr::recode(term_clean, !!!plot_term_labels, .default = term_clean),
    term_label = stringr::str_wrap(term_label, 42),
    block = factor(
      block,
      levels = c("Prospective protection/threat", "Current financial strain", "Other")
    )
  )

p_residual_coefficients <- ggplot2::ggplot(
  residual_coefficients_plot_data,
  ggplot2::aes(
    x = estimate,
    y = forcats::fct_reorder(term_label, estimate),
    color = block
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.7,
    color = unname(article_palette["grey_mid"])
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = conf_low, xmax = conf_high),
    height = 0.18,
    linewidth = 0.85
  ) +
  ggplot2::geom_point(size = 3.2) +
  ggplot2::scale_color_manual(
    values = coefficient_block_palette,
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Subjective economic security and residual PHQ-4",
    subtitle = "Outcome is observed PHQ-4 minus objective-position expected PHQ-4",
    x = "Association with residual PHQ-4",
    y = NULL,
    color = "Subjective-security block"
  ) +
  main_theme(base_size = 12)

residual_summary_by_insecurity_level <- residual_summary_by_insecurity_level %>%
  dplyr::mutate(
    subjective_insecurity_level_f = factor(
      subjective_insecurity_level_f,
      levels = c(
        "Low subjective insecurity",
        "Moderate subjective insecurity",
        "High subjective insecurity"
      )
    ),
    subjective_insecurity_short = dplyr::recode(
      as.character(subjective_insecurity_level_f),
      !!!subjective_insecurity_short_labels
    ),
    subjective_insecurity_short = factor(
      subjective_insecurity_short,
      levels = c("Low", "Moderate", "High")
    )
  )

p_residual_by_insecurity <- ggplot2::ggplot(
  residual_summary_by_insecurity_level,
  ggplot2::aes(
    x = subjective_insecurity_short,
    y = mean_residual_phq4,
    fill = subjective_insecurity_level_f
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.8,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::scale_fill_manual(
    values = subjective_insecurity_palette,
    drop = FALSE,
    na.value = unname(article_palette["grey_light"])
  ) +
  ggplot2::labs(
    title = "Residual PHQ-4 by subjective insecurity level",
    subtitle = "Positive values indicate distress higher than expected from objective position",
    x = "Subjective insecurity level",
    y = "Mean residual PHQ-4"
  ) +
  main_theme(base_size = 12)

obs_exp_resid_by_insecurity_plot_data <- residual_summary_by_insecurity_level %>%
  dplyr::select(
    subjective_insecurity_level_f,
    subjective_insecurity_short,
    mean_observed_phq4,
    mean_expected_phq4,
    mean_residual_phq4
  ) %>%
  tidyr::pivot_longer(
    cols = c(mean_observed_phq4, mean_expected_phq4, mean_residual_phq4),
    names_to = "metric",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    metric = dplyr::recode(
      metric,
      mean_observed_phq4 = "Observed PHQ-4",
      mean_expected_phq4 = "Expected PHQ-4",
      mean_residual_phq4 = "Residual PHQ-4"
    ),
    metric = factor(
      metric,
      levels = c("Observed PHQ-4", "Expected PHQ-4", "Residual PHQ-4")
    )
  )

p_obs_exp_resid_by_insecurity <- ggplot2::ggplot(
  obs_exp_resid_by_insecurity_plot_data,
  ggplot2::aes(
    x = subjective_insecurity_short,
    y = value,
    fill = subjective_insecurity_level_f
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_hline(
    data = obs_exp_resid_by_insecurity_plot_data %>%
      dplyr::filter(metric == "Residual PHQ-4"),
    ggplot2::aes(yintercept = 0),
    linewidth = 0.7,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  ggplot2::scale_fill_manual(
    values = subjective_insecurity_palette,
    drop = FALSE,
    na.value = unname(article_palette["grey_light"])
  ) +
  ggplot2::labs(
    title = "Observed, expected and residual PHQ-4 by subjective insecurity",
    subtitle = "Expected PHQ-4 comes from the objective-position model",
    x = "Subjective insecurity level",
    y = "Weighted mean"
  ) +
  main_theme(base_size = 12)

higher_odds_plot_data <- logistic_model_coefficients %>%
  dplyr::filter(
    model == "L1 Higher-than-expected distress",
    term != "(Intercept)"
  ) %>%
  dplyr::mutate(
    term_label = dplyr::recode(term_clean, !!!plot_term_labels, .default = term_clean),
    term_label = stringr::str_wrap(term_label, 42),
    block = factor(
      block,
      levels = c("Prospective protection/threat", "Current financial strain", "Other")
    )
  )

p_higher_odds <- ggplot2::ggplot(
  higher_odds_plot_data,
  ggplot2::aes(
    x = odds_ratio,
    y = forcats::fct_reorder(term_label, odds_ratio),
    color = block
  )
) +
  ggplot2::geom_vline(
    xintercept = 1,
    linewidth = 0.75,
    color = unname(article_palette["grey_mid"])
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = odds_ratio_low, xmax = odds_ratio_high),
    height = 0.18,
    linewidth = 0.85
  ) +
  ggplot2::geom_point(size = 3.2) +
  ggplot2::scale_x_log10(
    breaks = c(0.4, 0.5, 0.75, 1, 1.5, 2),
    labels = c("0.4", "0.5", "0.75", "1.0", "1.5", "2.0")
  ) +
  ggplot2::scale_color_manual(
    values = coefficient_block_palette,
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Subjective economic security and higher-than-expected distress",
    subtitle = "Survey-weighted quasibinomial model; odds ratios on log scale",
    x = "Odds ratio",
    y = NULL,
    color = "Subjective-security block"
  ) +
  main_theme(base_size = 12)

lower_odds_plot_data <- logistic_model_coefficients %>%
  dplyr::filter(
    model == "L2 Lower-than-expected distress",
    term != "(Intercept)"
  ) %>%
  dplyr::mutate(
    term_label = dplyr::recode(term_clean, !!!plot_term_labels, .default = term_clean),
    term_label = stringr::str_wrap(term_label, 42),
    block = factor(
      block,
      levels = c("Prospective protection/threat", "Current financial strain", "Other")
    )
  )

p_lower_odds <- ggplot2::ggplot(
  lower_odds_plot_data,
  ggplot2::aes(
    x = odds_ratio,
    y = forcats::fct_reorder(term_label, odds_ratio),
    color = block
  )
) +
  ggplot2::geom_vline(
    xintercept = 1,
    linewidth = 0.75,
    color = unname(article_palette["grey_mid"])
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = odds_ratio_low, xmax = odds_ratio_high),
    height = 0.18,
    linewidth = 0.85
  ) +
  ggplot2::geom_point(size = 3.2) +
  ggplot2::scale_x_log10(
    breaks = c(0.4, 0.5, 0.75, 1, 1.5, 2),
    labels = c("0.4", "0.5", "0.75", "1.0", "1.5", "2.0")
  ) +
  ggplot2::scale_color_manual(
    values = coefficient_block_palette,
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Subjective economic security and lower-than-expected distress",
    subtitle = "Survey-weighted quasibinomial model; odds ratios on log scale",
    x = "Odds ratio",
    y = NULL,
    color = "Subjective-security block"
  ) +
  main_theme(base_size = 12)

residual_by_dimensions_plot_data <- residual_by_subjective_security %>%
  dplyr::filter(
    metric == "Residual PHQ-4",
    group_var %in% c(
      "health_financial_protection_3cat_f",
      "no_emergency_money_support_f",
      "employment_risk_position_f",
      "debt_status_f",
      "making_ends_meet_3cat_f"
    )
  ) %>%
  dplyr::mutate(
    group_var_label = factor(
      group_var_label,
      levels = c(
        "Health emergency protection",
        "Emergency monetary support",
        "Employment risk",
        "Debt status",
        "Making ends meet"
      )
    ),
    dimension_block = dplyr::case_when(
      group_var %in% c(
        "health_financial_protection_3cat_f",
        "no_emergency_money_support_f",
        "employment_risk_position_f"
      ) ~ "Prospective protection/threat",
      group_var %in% c(
        "debt_status_f",
        "making_ends_meet_3cat_f"
      ) ~ "Current financial strain",
      TRUE ~ "Other"
    ),
    dimension_block = factor(
      dimension_block,
      levels = c("Prospective protection/threat", "Current financial strain", "Other")
    )
  )

p_residual_by_dimensions <- ggplot2::ggplot(
  residual_by_dimensions_plot_data,
  ggplot2::aes(
    x = weighted_mean,
    y = forcats::fct_reorder(group_wrapped, weighted_mean),
    color = dimension_block
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.75,
    color = unname(article_palette["grey_mid"])
  ) +
  ggplot2::geom_point(size = 3.2) +
  ggplot2::facet_wrap(
    ~ group_var_label,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::scale_color_manual(
    values = coefficient_block_palette,
    drop = FALSE,
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Residual PHQ-4 by subjective economic-security dimensions",
    subtitle = "Positive values indicate distress higher than expected from objective position",
    x = "Mean residual PHQ-4",
    y = NULL
  ) +
  main_theme(base_size = 12)


# ------------------------------------------------------------
# 14. Export outputs
# ------------------------------------------------------------

output_excel <- file.path(
  script_tables_dir,
  "04_subjective_security_residual_tables.xlsx"
)

openxlsx::write.xlsx(
  list(
    sample_summary = sample_summary,
    variable_check = subjective_variable_check,
    residual_model_fit = residual_model_fit,
    residual_coefficients = residual_model_coefficients,
    summary_model_fit = summary_model_fit,
    summary_coefficients = summary_model_coefficients,
    logistic_coefficients = logistic_model_coefficients,
    observed_supp_fit = observed_supplementary_fit,
    observed_supp_coef = observed_supplementary_coefficients,
    residual_by_subjective = residual_by_subjective_security,
    residual_by_insecurity = residual_summary_by_insecurity_level
  ),
  file = output_excel,
  overwrite = TRUE
)

readr::write_csv(
  sample_summary,
  file.path(script_csv_dir, "04_sample_summary.csv")
)

readr::write_csv(
  subjective_variable_check,
  file.path(script_csv_dir, "04_subjective_variable_check.csv")
)

readr::write_csv(
  residual_model_fit,
  file.path(script_csv_dir, "04_residual_model_fit.csv")
)

readr::write_csv(
  residual_model_coefficients,
  file.path(script_csv_dir, "04_residual_model_coefficients.csv")
)

readr::write_csv(
  summary_model_fit,
  file.path(script_csv_dir, "04_summary_model_fit.csv")
)

readr::write_csv(
  summary_model_coefficients,
  file.path(script_csv_dir, "04_summary_model_coefficients.csv")
)

readr::write_csv(
  logistic_model_coefficients,
  file.path(script_csv_dir, "04_logistic_model_coefficients.csv")
)

readr::write_csv(
  observed_supplementary_fit,
  file.path(script_csv_dir, "04_observed_supplementary_fit.csv")
)

readr::write_csv(
  observed_supplementary_coefficients,
  file.path(script_csv_dir, "04_observed_supplementary_coefficients.csv")
)

readr::write_csv(
  residual_by_subjective_security,
  file.path(script_csv_dir, "04_residual_by_subjective_security.csv")
)

readr::write_csv(
  residual_summary_by_insecurity_level,
  file.path(script_csv_dir, "04_residual_summary_by_insecurity_level.csv")
)

analytic_04_path <- file.path(
  script_rds_dir,
  "04_subjective_security_residual_dataset.rds"
)

saveRDS(
  analytic_04,
  analytic_04_path
)

model_objects_all <- list(
  residual_models = residual_model_objects,
  summary_models = summary_model_objects,
  logistic_models = logistic_model_objects,
  observed_supplementary_models = observed_supplementary_model_objects
)

model_objects_path <- file.path(
  script_rds_dir,
  "04_residual_model_objects.rds"
)

saveRDS(
  model_objects_all,
  model_objects_path
)

metadata_04 <- list(
  script_name = script_name,
  input_rds = input_rds,
  primary_residual_outcome = primary_residual_outcome,
  primary_observed_outcome = primary_observed_outcome,
  prospective_protection_threat_vars = prospective_protection_threat_vars,
  current_financial_strain_vars = current_financial_strain_vars,
  subjective_security_specific_vars = subjective_security_specific_vars,
  subjective_security_summary_vars = subjective_security_summary_vars,
  objective_position_vars = objective_position_vars,
  residual_model_fit = residual_model_fit,
  observed_supplementary_fit = observed_supplementary_fit
)

metadata_04_path <- file.path(
  script_rds_dir,
  "04_subjective_security_residual_metadata.rds"
)

saveRDS(
  metadata_04,
  metadata_04_path
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "04_residual_coefficients_specific_indicators.png"),
  plot = p_residual_coefficients,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "04_residual_by_subjective_insecurity_level.png"),
  plot = p_residual_by_insecurity,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "04_observed_expected_residual_by_subjective_insecurity.png"),
  plot = p_obs_exp_resid_by_insecurity,
  width = 13,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "04_higher_than_expected_odds.png"),
  plot = p_higher_odds,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "04_lower_than_expected_odds.png"),
  plot = p_lower_odds,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "04_residual_by_subjective_security_dimensions.png"),
  plot = p_residual_by_dimensions,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

manifest <- tibble::tibble(
  output = c(
    "Excel diagnostics",
    "Script 04 analytic dataset",
    "Model objects",
    "Script 04 metadata",
    "Residual model fit CSV",
    "Residual model coefficients CSV",
    "Logistic model coefficients CSV",
    "Observed supplementary fit CSV",
    "Observed supplementary coefficients CSV",
    "Residual by subjective security CSV",
    "Residual by subjective insecurity CSV",
    "Residual coefficients figure",
    "Residual by subjective insecurity level figure",
    "Observed expected residual by subjective insecurity figure",
    "Higher-than-expected odds figure",
    "Lower-than-expected odds figure",
    "Residual by subjective dimensions figure"
  ),
  path = c(
    output_excel,
    analytic_04_path,
    model_objects_path,
    metadata_04_path,
    file.path(script_csv_dir, "04_residual_model_fit.csv"),
    file.path(script_csv_dir, "04_residual_model_coefficients.csv"),
    file.path(script_csv_dir, "04_logistic_model_coefficients.csv"),
    file.path(script_csv_dir, "04_observed_supplementary_fit.csv"),
    file.path(script_csv_dir, "04_observed_supplementary_coefficients.csv"),
    file.path(script_csv_dir, "04_residual_by_subjective_security.csv"),
    file.path(script_csv_dir, "04_residual_summary_by_insecurity_level.csv"),
    file.path(script_figures_dir, "04_residual_coefficients_specific_indicators.png"),
    file.path(script_figures_dir, "04_residual_by_subjective_insecurity_level.png"),
    file.path(script_figures_dir, "04_observed_expected_residual_by_subjective_insecurity.png"),
    file.path(script_figures_dir, "04_higher_than_expected_odds.png"),
    file.path(script_figures_dir, "04_lower_than_expected_odds.png"),
    file.path(script_figures_dir, "04_residual_by_subjective_security_dimensions.png")
  )
)

readr::write_csv(
  manifest,
  file.path(script_csv_dir, "04_output_manifest.csv")
)


# ------------------------------------------------------------
# 15. Save session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(script_logs_dir, "04_session_info.txt")
)


# ------------------------------------------------------------
# 16. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("04_subjective_security_residual_associations.R completed")
message("============================================================")
message("Input file: ", input_rds)
message("Output folder: ", script_output_dir)
message("Complete residual model sample n: ", sum(analytic_04$residual_specific_model_complete))
message(
  "R3 weighted R-squared for residual PHQ-4: ",
  round(
    residual_model_fit$r2_weighted[
      residual_model_fit$model == "R3 Full subjective security"
    ] * 100,
    2
  ),
  "%"
)
message("Residual model outcome: ", primary_residual_outcome)
message("Main residual dataset: ", analytic_04_path)
message("Excel diagnostics: ", output_excel)
message("Figures saved in: ", script_figures_dir)
message("CSV outputs saved in: ", script_csv_dir)
message("RDS outputs saved in: ", script_rds_dir)
message("============================================================\n")
