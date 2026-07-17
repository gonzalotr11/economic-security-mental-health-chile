# ============================================================
# 03_objective_gradient_residual_phq4.R
# Project: Subjective economic security and psychological distress in Chile
# Purpose: Estimate objective-position gradients and construct residual PHQ-4
# Author: GT / ChatGPT
# ============================================================

# ------------------------------------------------------------
# 00. Instructions
# ------------------------------------------------------------
# This script is the fourth step of the rebuilt analysis pipeline.
# It starts from the analytic dataset produced by:
#
#   output_01_construct_rebuild_variables/rds/01_ebs_rebuild_analytic.rds
#
# Main conceptual goal:
#   Estimate the objective socioeconomic gradient in PHQ-4 and construct
#   objective-position-predicted PHQ-4 and residual PHQ-4.
#
# In the new paper strategy:
#   - The objective-position model defines the expected level of psychological
#     distress given social and economic position.
#   - The residual captures psychological distress above or below what would be
#     expected from objective position.
#   - Later scripts use this residual to analyze within-gradient heterogeneity,
#     subjective economic security, and convergence/mismatch configurations.
#
# This script estimates:
#   M0: Intercept-only model
#   M1: Conventional SES model
#       income + education + housing tenure
#   M2: M1 + life-labor stage
#   M3: Expanded objective-position model
#       income + education + housing tenure + life-labor stage +
#       health system + housing deprivation + territorial insecurity +
#       sex + macrozone
#
# Main outputs:
#   1. Objective model coefficients and fit summaries.
#   2. PHQ-4 expected from the expanded objective-position model.
#   3. PHQ-4 residual and standardized residual.
#   4. Residual position categories using +/- 0.50 SD.
#   5. Sensitivity residual categories using +/- 0.75 SD.
#   6. Expected PHQ-4 terciles.
#   7. Residual diagnostics and figures.
#
# Output folder:
#   output_03_objective_gradient_residual_phq4
#
# Main files:
#   rds/03_objective_gradient_residual_dataset.rds
#   rds/03_objective_model_objects.rds
#   tables/03_objective_gradient_residual_tables.xlsx
#   figures/03_objective_gradient_phq4.png
#   figures/03_expected_phq4_distribution.png
#   figures/03_residual_phq4_distribution.png
#   figures/03_observed_expected_residual_by_objective_position.png

# ============================================================
# 01. Packages
# ============================================================

required_packages <- c(
  "tidyverse",
  "haven",
  "labelled",
  "janitor",
  "openxlsx",
  "survey",
  "broom",
  "stringr",
  "forcats",
  "scales",
  "patchwork"
)

installed_packages <- rownames(installed.packages())
packages_to_install <- setdiff(required_packages, installed_packages)

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

invisible(lapply(required_packages, library, character.only = TRUE))

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
options(survey.lonely.psu = "adjust")


# ============================================================
# 02. Paths
# ============================================================

script_name <- "03_objective_gradient_residual_phq4"

possible_base_paths <- c(
  getwd(),
  "C:/Users/PC/OneDrive/Publicaciones/Subj Econ Hardship in the middle/Data",
  "D:/OneDrive/Publicaciones/Subj Econ Hardship in the middle/Data",
  "C:/Users/Gonzalo/OneDrive/Publicaciones/Subj Econ Hardship in the middle/Data",
  "C:/Users/gonza/OneDrive/Publicaciones/Subj Econ Hardship in the middle/Data",
  "C:/Users/gonzalotr/OneDrive/Publicaciones/Subj Econ Hardship in the middle/Data"
)

possible_base_paths <- unique(possible_base_paths)

base_path <- possible_base_paths[file.exists(possible_base_paths)][1]

if (is.na(base_path) || length(base_path) == 0) {
  stop(
    "No valid base path was found. Please add your current OneDrive data path to `possible_base_paths`."
  )
}

input_01_dir <- file.path(base_path, "output_01_construct_rebuild_variables")
input_02_dir <- file.path(base_path, "output_02_phq4_and_validation_outcomes")

input_rds_candidates <- c(
  file.path(input_01_dir, "rds", "01_ebs_rebuild_analytic.rds"),
  file.path(input_01_dir, "01_ebs_rebuild_analytic.rds")
)

input_rds <- input_rds_candidates[file.exists(input_rds_candidates)][1]

if (is.na(input_rds) || length(input_rds) == 0) {
  stop(
    "Input analytic dataset not found. Run 01_construct_rebuild_variables.R first.\n",
    "Expected one of:\n",
    paste(input_rds_candidates, collapse = "\n")
  )
}

variable_groups_candidates <- c(
  file.path(input_01_dir, "rds", "01_variable_groups_rebuild.rds"),
  file.path(input_01_dir, "01_variable_groups_rebuild.rds")
)

variable_groups_path <- variable_groups_candidates[file.exists(variable_groups_candidates)][1]

output_dir <- file.path(base_path, paste0("output_", script_name))

output_tables_dir  <- file.path(output_dir, "tables")
output_figures_dir <- file.path(output_dir, "figures")
output_csv_dir     <- file.path(output_dir, "csv")
output_rds_dir     <- file.path(output_dir, "rds")
output_logs_dir    <- file.path(output_dir, "logs")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_rds_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_logs_dir, recursive = TRUE, showWarnings = FALSE)

message("Using base path: ", base_path)
message("Using input dataset: ", input_rds)
message("Using output folder: ", output_dir)


# ============================================================
# 03. Global plotting style and article palette
# ============================================================

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

article_discrete_palette <- c(
  article_palette["purple_dark"],
  article_palette["purple_mid"],
  article_palette["purple_light"],
  article_palette["mustard_dark"],
  article_palette["mustard_mid"],
  article_palette["mustard_light"],
  article_palette["grey_dark"],
  article_palette["grey_mid"],
  article_palette["grey_light"]
)

expected_phq4_palette <- c(
  "Low expected distress" = article_palette["purple_light"],
  "Intermediate expected distress" = article_palette["purple_mid"],
  "High expected distress" = article_palette["purple_dark"]
)

residual_position_palette <- c(
  "Lower than expected distress" = article_palette["mustard_light"],
  "Close to expected distress" = article_palette["grey_light"],
  "Higher than expected distress" = article_palette["purple_dark"]
)

main_theme <- function(base_size = 16) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 4,
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
      legend.title = ggplot2::element_text(
        size = base_size - 1,
        color = article_palette["grey_dark"]
      ),
      legend.text = ggplot2::element_text(
        size = base_size - 2,
        color = article_palette["grey_dark"]
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = base_size - 1,
        color = article_palette["grey_dark"]
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = article_palette["grey_light"],
        linewidth = 0.25
      ),
      panel.grid.major.y = ggplot2::element_line(
        color = article_palette["grey_light"],
        linewidth = 0.25
      ),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )
}


# ============================================================
# 04. Load analytic dataset
# ============================================================

analytic <- readRDS(input_rds) %>%
  janitor::clean_names()

if (!is.na(variable_groups_path) && length(variable_groups_path) > 0) {
  variable_groups <- readRDS(variable_groups_path)
} else {
  variable_groups <- list()
}

required_design_vars <- c("weight", "strata", "psu")
missing_design_vars <- setdiff(required_design_vars, names(analytic))

if (length(missing_design_vars) > 0) {
  stop("Missing survey design variables: ", paste(missing_design_vars, collapse = ", "))
}

analytic <- analytic %>%
  dplyr::mutate(
    weight = as.numeric(weight),
    strata = as.factor(strata),
    psu = as.factor(psu)
  )


# ============================================================
# 05. Variable definitions
# ============================================================

outcome_var <- "phq4_score"

if (!outcome_var %in% names(analytic)) {
  stop("Primary outcome variable `phq4_score` was not found in the analytic dataset.")
}

m0_vars <- character(0)

m1_vars <- c(
  "income_group_3cat_f",
  "education_3cat_f"
)

m2_vars <- c(
  m1_vars,
  "life_labor_stage_f"
)

m3_vars <- c(
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

model_variable_check <- tibble::tibble(
  model = c(
    rep("M1 conventional SES", length(m1_vars)),
    rep("M2 SES plus life-labor", length(m2_vars)),
    rep("M3 expanded objective position", length(m3_vars))
  ),
  variable = c(m1_vars, m2_vars, m3_vars),
  available = variable %in% names(analytic)
)

missing_m3_vars <- setdiff(m3_vars, names(analytic))

if (length(missing_m3_vars) > 0) {
  stop(
    "Missing variables required for the expanded objective-position model: ",
    paste(missing_m3_vars, collapse = ", ")
  )
}

objective_group_vars_for_descriptives <- intersect(
  c(
    "income_group_3cat_f",
    "education_3cat_f",
    "housing_tenure_3cat_f",
    "life_labor_stage_f",
    "health_system_3cat_f",
    "housing_deprivation_3cat_f",
    "territory_insecurity_3cat_f",
    "sex_f",
    "macrozone_f"
  ),
  names(analytic)
)

objective_group_labels <- c(
  income_group_3cat_f = "Income group",
  education_3cat_f = "Education",
  housing_tenure_3cat_f = "Housing tenure",
  life_labor_stage_f = "Life-labor stage",
  health_system_3cat_f = "Health system",
  housing_deprivation_3cat_f = "Housing deprivation",
  territory_insecurity_3cat_f = "Territorial insecurity",
  sex_f = "Sex",
  macrozone_f = "Macrozone"
)


# ============================================================
# 06. Helper functions
# ============================================================

weighted_mean_approx <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

weighted_sd_approx <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  m <- stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
  sqrt(stats::weighted.mean((x[ok] - m)^2, w[ok], na.rm = TRUE))
}

weighted_quantile <- function(x, w, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
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

make_formula <- function(outcome, predictors) {
  if (length(predictors) == 0) {
    stats::as.formula(paste0(outcome, " ~ 1"))
  } else {
    stats::as.formula(
      paste0(outcome, " ~ ", paste(predictors, collapse = " + "))
    )
  }
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
  
  sqrt(stats::weighted.mean((observed[ok] - predicted[ok])^2, weights[ok], na.rm = TRUE))
}

mae_weighted <- function(observed, predicted, weights) {
  ok <- !is.na(observed) & !is.na(predicted) & !is.na(weights)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  stats::weighted.mean(abs(observed[ok] - predicted[ok]), weights[ok], na.rm = TRUE)
}

tidy_svyglm <- function(model, model_name) {
  broom::tidy(model) %>%
    dplyr::mutate(
      model = model_name,
      conf_low = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error,
      p_value = p.value,
      .before = 1
    ) %>%
    dplyr::select(
      model,
      term,
      estimate,
      std.error,
      statistic,
      p_value,
      conf_low,
      conf_high
    )
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
      group_var_label = dplyr::recode(group_var, !!!objective_group_labels, .default = group_var),
      group = as.character(group)
    ) %>%
    dplyr::select(
      variable,
      group_var,
      group_var_label,
      group,
      unweighted_n,
      weighted_n,
      weighted_mean,
      weighted_sd_approx
    )
}


# ============================================================
# 07. Prepare model sample and survey design
# ============================================================

model_core_vars <- unique(c(
  outcome_var,
  m3_vars,
  "weight",
  "strata",
  "psu"
))

analytic_model <- analytic %>%
  dplyr::mutate(
    objective_model_complete = stats::complete.cases(dplyr::across(dplyr::all_of(model_core_vars)))
  )

design_full <- survey::svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analytic_model,
  nest = TRUE
)

design_m3_complete <- subset(design_full, objective_model_complete)

model_sample_summary <- tibble::tibble(
  sample_definition = c(
    "Full analytic data",
    "Valid PHQ-4",
    "Complete expanded objective-position model sample"
  ),
  unweighted_n = c(
    nrow(analytic_model),
    sum(!is.na(analytic_model[[outcome_var]])),
    sum(analytic_model$objective_model_complete)
  ),
  weighted_n = c(
    sum(analytic_model$weight, na.rm = TRUE),
    sum(analytic_model$weight[!is.na(analytic_model[[outcome_var]])], na.rm = TRUE),
    sum(analytic_model$weight[analytic_model$objective_model_complete], na.rm = TRUE)
  )
)


# ============================================================
# 08. Estimate objective-position models
# ============================================================

formula_m0 <- make_formula(outcome_var, m0_vars)
formula_m1 <- make_formula(outcome_var, m1_vars)
formula_m2 <- make_formula(outcome_var, m2_vars)
formula_m3 <- make_formula(outcome_var, m3_vars)

model_m0 <- survey::svyglm(
  formula_m0,
  design = design_m3_complete,
  family = gaussian()
)

model_m1 <- survey::svyglm(
  formula_m1,
  design = design_m3_complete,
  family = gaussian()
)

model_m2 <- survey::svyglm(
  formula_m2,
  design = design_m3_complete,
  family = gaussian()
)

model_m3 <- survey::svyglm(
  formula_m3,
  design = design_m3_complete,
  family = gaussian()
)

model_objects <- list(
  M0_intercept_only = model_m0,
  M1_conventional_ses = model_m1,
  M2_ses_life_labor = model_m2,
  M3_expanded_objective_position = model_m3
)

model_coefficients <- dplyr::bind_rows(
  tidy_svyglm(model_m0, "M0 Intercept only"),
  tidy_svyglm(model_m1, "M1 Conventional SES"),
  tidy_svyglm(model_m2, "M2 SES + life-labor stage"),
  tidy_svyglm(model_m3, "M3 Expanded objective position")
)

model_fit <- dplyr::bind_rows(
  model_fit_summary(model_m0, analytic_model, outcome_var, "M0 Intercept only"),
  model_fit_summary(model_m1, analytic_model, outcome_var, "M1 Conventional SES"),
  model_fit_summary(model_m2, analytic_model, outcome_var, "M2 SES + life-labor stage"),
  model_fit_summary(model_m3, analytic_model, outcome_var, "M3 Expanded objective position")
) %>%
  dplyr::mutate(
    r2_change_from_previous = r2_weighted - dplyr::lag(r2_weighted),
    r2_change_from_m0 = r2_weighted - r2_weighted[model == "M0 Intercept only"]
  )


# ============================================================
# 09. Construct expected and residual PHQ-4
# ============================================================

m3_model_frame <- stats::model.frame(model_m3)
m3_included_rows <- as.integer(rownames(m3_model_frame))

analytic_residual <- analytic_model %>%
  dplyr::mutate(
    phq4_expected_objective = NA_real_
  )

analytic_residual$phq4_expected_objective[m3_included_rows] <- as.numeric(
  stats::predict(model_m3, type = "response")
)

analytic_residual <- analytic_residual %>%
  dplyr::mutate(
    phq4_residual_objective = phq4_score - phq4_expected_objective
  )

residual_mean_weighted <- weighted_mean_approx(
  analytic_residual$phq4_residual_objective,
  analytic_residual$weight
)

residual_sd_weighted <- weighted_sd_approx(
  analytic_residual$phq4_residual_objective,
  analytic_residual$weight
)

analytic_residual <- analytic_residual %>%
  dplyr::mutate(
    phq4_residual_objective_z = (
      phq4_residual_objective - residual_mean_weighted
    ) / residual_sd_weighted,
    
    phq4_residual_position_f = dplyr::case_when(
      phq4_residual_objective_z <= -0.50 ~ "Lower than expected distress",
      phq4_residual_objective_z >= 0.50 ~ "Higher than expected distress",
      !is.na(phq4_residual_objective_z) ~ "Close to expected distress",
      TRUE ~ NA_character_
    ),
    
    phq4_residual_position_f = factor(
      phq4_residual_position_f,
      levels = c(
        "Lower than expected distress",
        "Close to expected distress",
        "Higher than expected distress"
      )
    ),
    
    phq4_residual_position_075_f = dplyr::case_when(
      phq4_residual_objective_z <= -0.75 ~ "Lower than expected distress",
      phq4_residual_objective_z >= 0.75 ~ "Higher than expected distress",
      !is.na(phq4_residual_objective_z) ~ "Close to expected distress",
      TRUE ~ NA_character_
    ),
    
    phq4_residual_position_075_f = factor(
      phq4_residual_position_075_f,
      levels = c(
        "Lower than expected distress",
        "Close to expected distress",
        "Higher than expected distress"
      )
    ),
    
    higher_than_expected = dplyr::case_when(
      phq4_residual_position_f == "Higher than expected distress" ~ 1,
      phq4_residual_position_f %in% c(
        "Lower than expected distress",
        "Close to expected distress"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    lower_than_expected = dplyr::case_when(
      phq4_residual_position_f == "Lower than expected distress" ~ 1,
      phq4_residual_position_f %in% c(
        "Close to expected distress",
        "Higher than expected distress"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    close_to_expected = dplyr::case_when(
      phq4_residual_position_f == "Close to expected distress" ~ 1,
      phq4_residual_position_f %in% c(
        "Lower than expected distress",
        "Higher than expected distress"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    not_higher_than_expected = dplyr::case_when(
      higher_than_expected == 1 ~ 0,
      higher_than_expected == 0 ~ 1,
      TRUE ~ NA_real_
    )
  )

# Expected PHQ-4 terciles are created within the M3 complete sample.
expected_tercile_breaks <- stats::quantile(
  analytic_residual$phq4_expected_objective,
  probs = c(0, 1 / 3, 2 / 3, 1),
  na.rm = TRUE,
  names = FALSE
)

# Make breaks unique in case of ties.
expected_tercile_breaks <- unique(expected_tercile_breaks)

if (length(expected_tercile_breaks) < 4) {
  analytic_residual <- analytic_residual %>%
    dplyr::mutate(
      phq4_expected_tercile = dplyr::ntile(phq4_expected_objective, 3),
      phq4_expected_tercile_f = factor(
        phq4_expected_tercile,
        levels = c(1, 2, 3),
        labels = c(
          "Low expected distress",
          "Intermediate expected distress",
          "High expected distress"
        )
      )
    )
} else {
  analytic_residual <- analytic_residual %>%
    dplyr::mutate(
      phq4_expected_tercile = dplyr::case_when(
        is.na(phq4_expected_objective) ~ NA_integer_,
        phq4_expected_objective <= expected_tercile_breaks[2] ~ 1L,
        phq4_expected_objective <= expected_tercile_breaks[3] ~ 2L,
        phq4_expected_objective <= expected_tercile_breaks[4] ~ 3L,
        TRUE ~ NA_integer_
      ),
      phq4_expected_tercile_f = factor(
        phq4_expected_tercile,
        levels = c(1, 2, 3),
        labels = c(
          "Low expected distress",
          "Intermediate expected distress",
          "High expected distress"
        )
      )
    )
}

# Backward-compatible aliases for older scripts.
analytic_residual <- analytic_residual %>%
  dplyr::mutate(
    phq4_score_expected_m3 = phq4_expected_objective,
    phq4_score_residual_m3 = phq4_residual_objective,
    phq4_score_residual_m3_z = phq4_residual_objective_z
  )


# ============================================================
# 10. Residual diagnostics
# ============================================================

residual_diagnostics <- analytic_residual %>%
  dplyr::summarise(
    n_valid_observed_expected_residual = sum(
      !is.na(phq4_score) &
        !is.na(phq4_expected_objective) &
        !is.na(phq4_residual_objective)
    ),
    weighted_n_valid = sum(
      weight[
        !is.na(phq4_score) &
          !is.na(phq4_expected_objective) &
          !is.na(phq4_residual_objective)
      ],
      na.rm = TRUE
    ),
    mean_observed_phq4_weighted = weighted_mean_approx(phq4_score, weight),
    mean_expected_phq4_weighted = weighted_mean_approx(phq4_expected_objective, weight),
    mean_residual_phq4_weighted = weighted_mean_approx(phq4_residual_objective, weight),
    sd_residual_phq4_weighted = weighted_sd_approx(phq4_residual_objective, weight),
    p10_residual = weighted_quantile(phq4_residual_objective, weight, probs = 0.10)[1],
    p25_residual = weighted_quantile(phq4_residual_objective, weight, probs = 0.25)[1],
    median_residual = weighted_quantile(phq4_residual_objective, weight, probs = 0.50)[1],
    p75_residual = weighted_quantile(phq4_residual_objective, weight, probs = 0.75)[1],
    p90_residual = weighted_quantile(phq4_residual_objective, weight, probs = 0.90)[1],
    min_residual = min(phq4_residual_objective, na.rm = TRUE),
    max_residual = max(phq4_residual_objective, na.rm = TRUE)
  )

observed_expected_residual_correlations <- tidyr::expand_grid(
  variable_x = c("phq4_score", "phq4_expected_objective", "phq4_residual_objective"),
  variable_y = c("phq4_score", "phq4_expected_objective", "phq4_residual_objective")
) %>%
  dplyr::mutate(
    correlation_weighted = purrr::map2_dbl(
      variable_x,
      variable_y,
      ~ weighted_correlation(
        analytic_residual[[.x]],
        analytic_residual[[.y]],
        analytic_residual$weight
      )
    ),
    correlation_unweighted = purrr::map2_dbl(
      variable_x,
      variable_y,
      ~ stats::cor(
        analytic_residual[[.x]],
        analytic_residual[[.y]],
        use = "pairwise.complete.obs"
      )
    ),
    variable_x_label = dplyr::recode(
      variable_x,
      phq4_score = "Observed PHQ-4",
      phq4_expected_objective = "Expected PHQ-4",
      phq4_residual_objective = "Residual PHQ-4"
    ),
    variable_y_label = dplyr::recode(
      variable_y,
      phq4_score = "Observed PHQ-4",
      phq4_expected_objective = "Expected PHQ-4",
      phq4_residual_objective = "Residual PHQ-4"
    )
  )

residual_position_distribution <- analytic_residual %>%
  dplyr::filter(!is.na(phq4_residual_position_f), !is.na(weight)) %>%
  dplyr::group_by(phq4_residual_position_f) %>%
  dplyr::summarise(
    unweighted_n = dplyr::n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    weighted_pct = 100 * weighted_n / sum(weighted_n)
  )

residual_position_075_distribution <- analytic_residual %>%
  dplyr::filter(!is.na(phq4_residual_position_075_f), !is.na(weight)) %>%
  dplyr::group_by(phq4_residual_position_075_f) %>%
  dplyr::summarise(
    unweighted_n = dplyr::n(),
    weighted_n = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    weighted_pct = 100 * weighted_n / sum(weighted_n)
  )

expected_tercile_distribution <- analytic_residual %>%
  dplyr::filter(!is.na(phq4_expected_tercile_f), !is.na(weight)) %>%
  dplyr::group_by(phq4_expected_tercile_f) %>%
  dplyr::summarise(
    unweighted_n = dplyr::n(),
    weighted_n = sum(weight, na.rm = TRUE),
    mean_observed_phq4 = weighted_mean_approx(phq4_score, weight),
    mean_expected_phq4 = weighted_mean_approx(phq4_expected_objective, weight),
    mean_residual_phq4 = weighted_mean_approx(phq4_residual_objective, weight),
    pct_higher_than_expected = weighted_mean_approx(higher_than_expected, weight) * 100,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    weighted_pct = 100 * weighted_n / sum(weighted_n)
  )


# ============================================================
# 11. Objective-position descriptive gradients
# ============================================================

phq4_objective_gradients <- purrr::map_dfr(
  objective_group_vars_for_descriptives,
  ~ weighted_mean_by_group(analytic_residual, "phq4_score", .x, "weight")
) %>%
  dplyr::mutate(
    outcome = "Observed PHQ-4",
    group_wrapped = stringr::str_wrap(group, 30)
  )

observed_expected_residual_by_objective <- purrr::map_dfr(
  objective_group_vars_for_descriptives,
  function(group_var) {
    observed <- weighted_mean_by_group(
      analytic_residual,
      "phq4_score",
      group_var,
      "weight"
    ) %>%
      dplyr::mutate(metric = "Observed PHQ-4")
    
    expected <- weighted_mean_by_group(
      analytic_residual,
      "phq4_expected_objective",
      group_var,
      "weight"
    ) %>%
      dplyr::mutate(metric = "Expected PHQ-4")
    
    residual <- weighted_mean_by_group(
      analytic_residual,
      "phq4_residual_objective",
      group_var,
      "weight"
    ) %>%
      dplyr::mutate(metric = "Residual PHQ-4")
    
    dplyr::bind_rows(observed, expected, residual)
  }
) %>%
  dplyr::mutate(
    metric = factor(
      metric,
      levels = c("Observed PHQ-4", "Expected PHQ-4", "Residual PHQ-4")
    ),
    group_wrapped = stringr::str_wrap(group, 30)
  )


# ============================================================
# 12. Figures
# ============================================================

# ------------------------------------------------------------
# 12.1 Model R2 figure
# ------------------------------------------------------------

p_model_fit <- ggplot2::ggplot(
  model_fit,
  ggplot2::aes(
    x = forcats::fct_reorder(model, r2_weighted),
    y = r2_weighted * 100
  )
) +
  ggplot2::geom_col(
    width = 0.75,
    fill = article_palette["purple_dark"]
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Weighted variance explained by objective-position models",
    subtitle = "PHQ-4 modeled as a continuous psychological distress outcome",
    x = NULL,
    y = "Weighted R-squared (%)"
  ) +
  main_theme(base_size = 15)

# ------------------------------------------------------------
# 12.2 PHQ-4 objective gradient figure
# ------------------------------------------------------------

gradient_plot_groups <- c(
  "income_group_3cat_f",
  "education_3cat_f",
  "housing_tenure_3cat_f",
  "life_labor_stage_f",
  "health_system_3cat_f",
  "territory_insecurity_3cat_f"
)

gradient_plot_data <- phq4_objective_gradients %>%
  dplyr::filter(group_var %in% gradient_plot_groups) %>%
  dplyr::mutate(
    group_var_label = factor(
      group_var_label,
      levels = c(
        "Income group",
        "Education",
        "Housing tenure",
        "Life-labor stage",
        "Health system",
        "Territorial insecurity"
      )
    )
  )

p_objective_gradient <- ggplot2::ggplot(
  gradient_plot_data,
  ggplot2::aes(
    x = weighted_mean,
    y = forcats::fct_reorder(group_wrapped, weighted_mean)
  )
) +
  ggplot2::geom_point(
    size = 3.8,
    color = article_palette["purple_dark"]
  ) +
  ggplot2::facet_wrap(
    ~ group_var_label,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::labs(
    title = "Observed PHQ-4 by objective-position variables",
    subtitle = "Weighted means; descriptive gradient before residual construction",
    x = "Weighted mean PHQ-4",
    y = NULL
  ) +
  main_theme(base_size = 15)

# ------------------------------------------------------------
# 12.3 Expected PHQ-4 distribution
# ------------------------------------------------------------

p_expected_distribution <- ggplot2::ggplot(
  analytic_residual %>%
    dplyr::filter(!is.na(phq4_expected_objective), !is.na(weight)),
  ggplot2::aes(x = phq4_expected_objective, weight = weight)
) +
  ggplot2::geom_histogram(
    bins = 40,
    fill = article_palette["purple_mid"],
    color = "white"
  ) +
  ggplot2::labs(
    title = "Distribution of objective-position-expected PHQ-4",
    subtitle = "Predicted values from the expanded objective-position model",
    x = "Expected PHQ-4",
    y = "Weighted count"
  ) +
  main_theme(base_size = 15)

# ------------------------------------------------------------
# 12.4 Residual PHQ-4 distribution
# ------------------------------------------------------------

p_residual_distribution <- ggplot2::ggplot(
  analytic_residual %>%
    dplyr::filter(!is.na(phq4_residual_objective), !is.na(weight)),
  ggplot2::aes(x = phq4_residual_objective, weight = weight)
) +
  ggplot2::geom_histogram(
    bins = 50,
    fill = article_palette["mustard_mid"],
    color = "white"
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.8,
    color = article_palette["grey_dark"]
  ) +
  ggplot2::labs(
    title = "Distribution of residual PHQ-4",
    subtitle = "Observed PHQ-4 minus objective-position-expected PHQ-4",
    x = "Residual PHQ-4",
    y = "Weighted count"
  ) +
  main_theme(base_size = 15)

# ------------------------------------------------------------
# 12.5 Residual position distribution
# ------------------------------------------------------------

p_residual_position <- ggplot2::ggplot(
  residual_position_distribution,
  ggplot2::aes(
    x = phq4_residual_position_f,
    y = weighted_pct,
    fill = phq4_residual_position_f
  )
) +
  ggplot2::geom_col(width = 0.75, show.legend = FALSE) +
  ggplot2::scale_fill_manual(
    values = residual_position_palette,
    drop = FALSE,
    na.value = article_palette["grey_light"]
  ) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title = "Residual PHQ-4 position",
    subtitle = "Categories based on +/- 0.50 SD of residual PHQ-4",
    x = NULL,
    y = "Weighted percentage"
  ) +
  main_theme(base_size = 15)

# ------------------------------------------------------------
# 12.6 Expected tercile distribution
# ------------------------------------------------------------

p_expected_terciles <- ggplot2::ggplot(
  expected_tercile_distribution,
  ggplot2::aes(
    x = phq4_expected_tercile_f,
    y = weighted_pct,
    fill = phq4_expected_tercile_f
  )
) +
  ggplot2::geom_col(width = 0.75, show.legend = FALSE) +
  ggplot2::scale_fill_manual(
    values = expected_phq4_palette,
    drop = FALSE,
    na.value = article_palette["grey_light"]
  ) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title = "Objective-position-expected PHQ-4 terciles",
    subtitle = "Terciles of predicted distress from the expanded objective-position model",
    x = NULL,
    y = "Weighted percentage"
  ) +
  main_theme(base_size = 15)

# ------------------------------------------------------------
# 12.7 Observed, expected and residual PHQ-4 by objective position
# ------------------------------------------------------------

observed_expected_plot_data <- observed_expected_residual_by_objective %>%
  dplyr::filter(
    group_var %in% c(
      "income_group_3cat_f",
      "education_3cat_f",
      "housing_tenure_3cat_f",
      "life_labor_stage_f",
      "territory_insecurity_3cat_f"
    )
  ) %>%
  dplyr::mutate(
    group_var_label = factor(
      group_var_label,
      levels = c(
        "Income group",
        "Education",
        "Housing tenure",
        "Life-labor stage",
        "Territorial insecurity"
      )
    )
  )

p_observed_expected_residual <- ggplot2::ggplot(
  observed_expected_plot_data,
  ggplot2::aes(
    x = weighted_mean,
    y = forcats::fct_reorder(group_wrapped, weighted_mean),
    color = metric
  )
) +
  ggplot2::geom_point(size = 3.3) +
  ggplot2::facet_grid(
    metric ~ group_var_label,
    scales = "free",
    space = "free_y"
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Observed PHQ-4" = article_palette["purple_dark"],
      "Expected PHQ-4" = article_palette["purple_mid"],
      "Residual PHQ-4" = article_palette["mustard_dark"]
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Observed, expected and residual PHQ-4 by objective position",
    subtitle = "Observed and expected distress are on the PHQ-4 scale; residuals are observed minus expected",
    x = "Weighted mean",
    y = NULL
  ) +
  main_theme(base_size = 12)


# ============================================================
# 13. Export outputs
# ============================================================

output_excel <- file.path(output_tables_dir, "03_objective_gradient_residual_tables.xlsx")

openxlsx::write.xlsx(
  list(
    sample_summary = model_sample_summary,
    model_var_check = model_variable_check,
    model_fit = model_fit,
    model_coefficients = model_coefficients,
    residual_diagnostics = residual_diagnostics,
    residual_position = residual_position_distribution,
    residual_position_075 = residual_position_075_distribution,
    expected_terciles = expected_tercile_distribution,
    obs_exp_resid_corr = observed_expected_residual_correlations,
    phq4_gradients = phq4_objective_gradients,
    obs_exp_resid_by_group = observed_expected_residual_by_objective
  ),
  file = output_excel,
  overwrite = TRUE
)

readr::write_csv(
  model_sample_summary,
  file.path(output_csv_dir, "03_model_sample_summary.csv")
)

readr::write_csv(
  model_variable_check,
  file.path(output_csv_dir, "03_model_variable_check.csv")
)

readr::write_csv(
  model_fit,
  file.path(output_csv_dir, "03_objective_model_fit.csv")
)

readr::write_csv(
  model_coefficients,
  file.path(output_csv_dir, "03_objective_model_coefficients.csv")
)

readr::write_csv(
  residual_diagnostics,
  file.path(output_csv_dir, "03_residual_diagnostics.csv")
)

readr::write_csv(
  observed_expected_residual_correlations,
  file.path(output_csv_dir, "03_observed_expected_residual_correlations.csv")
)

readr::write_csv(
  residual_position_distribution,
  file.path(output_csv_dir, "03_residual_position_distribution.csv")
)

readr::write_csv(
  expected_tercile_distribution,
  file.path(output_csv_dir, "03_expected_tercile_distribution.csv")
)

readr::write_csv(
  phq4_objective_gradients,
  file.path(output_csv_dir, "03_phq4_objective_gradients.csv")
)

readr::write_csv(
  observed_expected_residual_by_objective,
  file.path(output_csv_dir, "03_observed_expected_residual_by_objective.csv")
)

analytic_residual_path <- file.path(output_rds_dir, "03_objective_gradient_residual_dataset.rds")
saveRDS(analytic_residual, analytic_residual_path)

model_objects_path <- file.path(output_rds_dir, "03_objective_model_objects.rds")
saveRDS(model_objects, model_objects_path)

objective_model_metadata <- list(
  script_name = script_name,
  input_rds = input_rds,
  outcome_var = outcome_var,
  m0_vars = m0_vars,
  m1_vars = m1_vars,
  m2_vars = m2_vars,
  m3_vars = m3_vars,
  residual_cutoff_primary_sd = 0.50,
  residual_cutoff_sensitivity_sd = 0.75,
  residual_mean_weighted = residual_mean_weighted,
  residual_sd_weighted = residual_sd_weighted,
  expected_tercile_breaks = expected_tercile_breaks,
  model_fit = model_fit
)

metadata_path <- file.path(output_rds_dir, "03_objective_model_metadata.rds")
saveRDS(objective_model_metadata, metadata_path)

ggplot2::ggsave(
  filename = file.path(output_figures_dir, "03_objective_model_r2.png"),
  plot = p_model_fit,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(output_figures_dir, "03_objective_gradient_phq4.png"),
  plot = p_objective_gradient,
  width = 15,
  height = 12,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(output_figures_dir, "03_expected_phq4_distribution.png"),
  plot = p_expected_distribution,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(output_figures_dir, "03_residual_phq4_distribution.png"),
  plot = p_residual_distribution,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(output_figures_dir, "03_residual_position_distribution.png"),
  plot = p_residual_position,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(output_figures_dir, "03_expected_tercile_distribution.png"),
  plot = p_expected_terciles,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(output_figures_dir, "03_observed_expected_residual_by_objective_position.png"),
  plot = p_observed_expected_residual,
  width = 18,
  height = 13,
  dpi = 300,
  bg = "white"
)

manifest <- tibble::tibble(
  output = c(
    "Excel diagnostics",
    "Residual analytic dataset",
    "Objective model objects",
    "Objective model metadata",
    "Model fit CSV",
    "Model coefficients CSV",
    "Residual diagnostics CSV",
    "Objective gradient figure",
    "Expected PHQ-4 distribution figure",
    "Residual PHQ-4 distribution figure",
    "Residual position distribution figure",
    "Expected tercile distribution figure",
    "Observed expected residual by objective-position figure"
  ),
  path = c(
    output_excel,
    analytic_residual_path,
    model_objects_path,
    metadata_path,
    file.path(output_csv_dir, "03_objective_model_fit.csv"),
    file.path(output_csv_dir, "03_objective_model_coefficients.csv"),
    file.path(output_csv_dir, "03_residual_diagnostics.csv"),
    file.path(output_figures_dir, "03_objective_gradient_phq4.png"),
    file.path(output_figures_dir, "03_expected_phq4_distribution.png"),
    file.path(output_figures_dir, "03_residual_phq4_distribution.png"),
    file.path(output_figures_dir, "03_residual_position_distribution.png"),
    file.path(output_figures_dir, "03_expected_tercile_distribution.png"),
    file.path(output_figures_dir, "03_observed_expected_residual_by_objective_position.png")
  )
)

readr::write_csv(
  manifest,
  file.path(output_csv_dir, "03_output_manifest.csv")
)


# ============================================================
# 14. Save session information
# ============================================================

session_info_text <- capture.output(sessionInfo())

writeLines(
  session_info_text,
  con = file.path(output_logs_dir, "03_session_info.txt")
)


# ============================================================
# 15. Console summary
# ============================================================

message("\n============================================================")
message("03 objective gradient and residual PHQ-4 completed")
message("============================================================")
message("Input file: ", input_rds)
message("Output folder: ", output_dir)
message("Expanded objective model sample n: ", sum(analytic_residual$objective_model_complete))
message("Valid observed/expected/residual PHQ-4 n: ", residual_diagnostics$n_valid_observed_expected_residual[1])
message("M3 weighted R-squared: ", round(model_fit$r2_weighted[model_fit$model == "M3 Expanded objective position"] * 100, 2), "%")
message("Weighted residual SD: ", round(residual_sd_weighted, 3))
message("Primary residual threshold: +/- 0.50 SD")
message("Sensitivity residual threshold: +/- 0.75 SD")
message("Residual dataset: ", analytic_residual_path)
message("Excel diagnostics: ", output_excel)
message("Figures saved in: ", output_figures_dir)
message("CSV outputs saved in: ", output_csv_dir)
message("RDS outputs saved in: ", output_rds_dir)
message("============================================================\n")
