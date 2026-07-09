# ============================================================
# 05_within_gradient_heterogeneity.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Examine subjective insecurity within objective-position
#          expected distress strata
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Setup
# ------------------------------------------------------------

source("scripts/00_setup.R")

options(survey.lonely.psu = "adjust")

script_name <- "05_within_gradient_heterogeneity"

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
# 02. Load dataset from script 04
# ------------------------------------------------------------

input_rds <- file.path(
  output_dir,
  "04_subjective_security_residual_associations",
  "rds",
  "04_subjective_security_residual_dataset.rds"
)

if (!file.exists(input_rds)) {
  stop(
    paste0(
      "Input dataset not found: ", input_rds, "\n",
      "Run scripts/04_subjective_security_residual_associations.R first."
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
# 03. Required variables and labels
# ------------------------------------------------------------

required_within_gradient_vars <- c(
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
  "weight",
  "strata",
  "psu"
)

missing_within_gradient_vars <- setdiff(
  required_within_gradient_vars,
  names(analytic)
)

if (length(missing_within_gradient_vars) > 0) {
  stop(
    "Missing required within-gradient variables: ",
    paste(missing_within_gradient_vars, collapse = ", "),
    "\nRun scripts 01, 03 and 04 before running script 05."
  )
}

expected_tercile_levels <- c(
  "Low expected distress",
  "Intermediate expected distress",
  "High expected distress"
)

subjective_insecurity_levels <- c(
  "Low subjective insecurity",
  "Moderate subjective insecurity",
  "High subjective insecurity"
)

prospective_insecurity_levels <- c(
  "No prospective insecurity",
  "One prospective insecurity",
  "Multiple prospective insecurities"
)

current_financial_strain_levels <- c(
  "No current financial strain",
  "One current financial strain",
  "Multiple current financial strains"
)

analytic_05 <- analytic %>%
  dplyr::mutate(
    phq4_expected_tercile_f = factor(
      as.character(phq4_expected_tercile_f),
      levels = expected_tercile_levels
    ),
    
    subjective_insecurity_level_f = factor(
      as.character(subjective_insecurity_level_f),
      levels = subjective_insecurity_levels
    ),
    
    prospective_insecurity_level_f = factor(
      as.character(prospective_insecurity_level_f),
      levels = prospective_insecurity_levels
    ),
    
    current_financial_strain_level_f = factor(
      as.character(current_financial_strain_level_f),
      levels = current_financial_strain_levels
    ),
    
    expected_tercile_short = dplyr::recode(
      as.character(phq4_expected_tercile_f),
      "Low expected distress" = "Low expected",
      "Intermediate expected distress" = "Intermediate expected",
      "High expected distress" = "High expected"
    ),
    
    expected_tercile_short = factor(
      expected_tercile_short,
      levels = c("Low expected", "Intermediate expected", "High expected")
    ),
    
    subjective_insecurity_short = dplyr::recode(
      as.character(subjective_insecurity_level_f),
      "Low subjective insecurity" = "Low",
      "Moderate subjective insecurity" = "Moderate",
      "High subjective insecurity" = "High"
    ),
    
    subjective_insecurity_short = factor(
      subjective_insecurity_short,
      levels = c("Low", "Moderate", "High")
    ),
    
    prospective_insecurity_short = dplyr::recode(
      as.character(prospective_insecurity_level_f),
      "No prospective insecurity" = "None",
      "One prospective insecurity" = "One",
      "Multiple prospective insecurities" = "Multiple"
    ),
    
    prospective_insecurity_short = factor(
      prospective_insecurity_short,
      levels = c("None", "One", "Multiple")
    ),
    
    current_financial_strain_short = dplyr::recode(
      as.character(current_financial_strain_level_f),
      "No current financial strain" = "None",
      "One current financial strain" = "One",
      "Multiple current financial strains" = "Multiple"
    ),
    
    current_financial_strain_short = factor(
      current_financial_strain_short,
      levels = c("None", "One", "Multiple")
    )
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

weighted_summary_by_two_groups <- function(data,
                                           group_var_1,
                                           group_var_2,
                                           group_label_1 = group_var_1,
                                           group_label_2 = group_var_2,
                                           weight_var = "weight") {
  if (!all(c(group_var_1, group_var_2, weight_var) %in% names(data))) {
    return(tibble::tibble())
  }
  
  data %>%
    dplyr::filter(
      !is.na(.data[[group_var_1]]),
      !is.na(.data[[group_var_2]]),
      !is.na(.data[[weight_var]]),
      !is.na(phq4_score),
      !is.na(phq4_expected_objective),
      !is.na(phq4_residual_objective)
    ) %>%
    dplyr::group_by(
      .data[[group_var_1]],
      .data[[group_var_2]]
    ) %>%
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
      .groups = "drop"
    ) %>%
    dplyr::rename(
      group_1 = 1,
      group_2 = 2
    ) %>%
    dplyr::mutate(
      group_var_1 = group_var_1,
      group_var_2 = group_var_2,
      group_label_1 = group_label_1,
      group_label_2 = group_label_2
    ) %>%
    dplyr::relocate(
      group_var_1,
      group_label_1,
      group_1,
      group_var_2,
      group_label_2,
      group_2
    )
}

weighted_distribution_by_two_groups <- function(data,
                                                group_var_1,
                                                group_var_2,
                                                weight_var = "weight") {
  data %>%
    dplyr::filter(
      !is.na(.data[[group_var_1]]),
      !is.na(.data[[group_var_2]]),
      !is.na(.data[[weight_var]])
    ) %>%
    dplyr::group_by(.data[[group_var_1]], .data[[group_var_2]]) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      group_1 = 1,
      group_2 = 2
    ) %>%
    dplyr::mutate(
      weighted_pct_total = 100 * weighted_n / sum(weighted_n, na.rm = TRUE)
    )
}

make_metric_long <- function(summary_table) {
  summary_table %>%
    dplyr::select(
      group_1,
      group_2,
      unweighted_n,
      weighted_n,
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
}


# ------------------------------------------------------------
# 06. Sample diagnostics
# ------------------------------------------------------------

sample_summary <- tibble::tibble(
  sample_definition = c(
    "Full Script 05 input data",
    "Valid observed, expected and residual PHQ-4",
    "Valid expected tercile and subjective insecurity level",
    "Valid expected tercile and prospective insecurity level",
    "Valid expected tercile and current financial strain level"
  ),
  unweighted_n = c(
    nrow(analytic_05),
    sum(
      !is.na(analytic_05$phq4_score) &
        !is.na(analytic_05$phq4_expected_objective) &
        !is.na(analytic_05$phq4_residual_objective)
    ),
    sum(
      !is.na(analytic_05$phq4_expected_tercile_f) &
        !is.na(analytic_05$subjective_insecurity_level_f) &
        !is.na(analytic_05$phq4_residual_objective)
    ),
    sum(
      !is.na(analytic_05$phq4_expected_tercile_f) &
        !is.na(analytic_05$prospective_insecurity_level_f) &
        !is.na(analytic_05$phq4_residual_objective)
    ),
    sum(
      !is.na(analytic_05$phq4_expected_tercile_f) &
        !is.na(analytic_05$current_financial_strain_level_f) &
        !is.na(analytic_05$phq4_residual_objective)
    )
  ),
  weighted_n = c(
    sum(analytic_05$weight, na.rm = TRUE),
    sum(
      analytic_05$weight[
        !is.na(analytic_05$phq4_score) &
          !is.na(analytic_05$phq4_expected_objective) &
          !is.na(analytic_05$phq4_residual_objective)
      ],
      na.rm = TRUE
    ),
    sum(
      analytic_05$weight[
        !is.na(analytic_05$phq4_expected_tercile_f) &
          !is.na(analytic_05$subjective_insecurity_level_f) &
          !is.na(analytic_05$phq4_residual_objective)
      ],
      na.rm = TRUE
    ),
    sum(
      analytic_05$weight[
        !is.na(analytic_05$phq4_expected_tercile_f) &
          !is.na(analytic_05$prospective_insecurity_level_f) &
          !is.na(analytic_05$phq4_residual_objective)
      ],
      na.rm = TRUE
    ),
    sum(
      analytic_05$weight[
        !is.na(analytic_05$phq4_expected_tercile_f) &
          !is.na(analytic_05$current_financial_strain_level_f) &
          !is.na(analytic_05$phq4_residual_objective)
      ],
      na.rm = TRUE
    )
  )
)

variable_check <- tibble::tibble(
  variable = required_within_gradient_vars,
  available = variable %in% names(analytic_05),
  n_valid = purrr::map_int(
    variable,
    ~ if (.x %in% names(analytic_05)) sum(!is.na(analytic_05[[.x]])) else NA_integer_
  ),
  n_missing = purrr::map_int(
    variable,
    ~ if (.x %in% names(analytic_05)) sum(is.na(analytic_05[[.x]])) else NA_integer_
  ),
  pct_missing = n_missing / nrow(analytic_05) * 100
)


# ------------------------------------------------------------
# 07. Main within-gradient summaries
# ------------------------------------------------------------

within_expected_by_subjective_insecurity <- weighted_summary_by_two_groups(
  data = analytic_05,
  group_var_1 = "expected_tercile_short",
  group_var_2 = "subjective_insecurity_short",
  group_label_1 = "Expected PHQ-4 tercile",
  group_label_2 = "Subjective insecurity level"
)

within_expected_by_prospective_insecurity <- weighted_summary_by_two_groups(
  data = analytic_05,
  group_var_1 = "expected_tercile_short",
  group_var_2 = "prospective_insecurity_short",
  group_label_1 = "Expected PHQ-4 tercile",
  group_label_2 = "Prospective insecurity level"
)

within_expected_by_current_strain <- weighted_summary_by_two_groups(
  data = analytic_05,
  group_var_1 = "expected_tercile_short",
  group_var_2 = "current_financial_strain_short",
  group_label_1 = "Expected PHQ-4 tercile",
  group_label_2 = "Current financial strain level"
)

distribution_expected_by_subjective_insecurity <- weighted_distribution_by_two_groups(
  data = analytic_05,
  group_var_1 = "expected_tercile_short",
  group_var_2 = "subjective_insecurity_short"
)

distribution_expected_by_prospective_insecurity <- weighted_distribution_by_two_groups(
  data = analytic_05,
  group_var_1 = "expected_tercile_short",
  group_var_2 = "prospective_insecurity_short"
)

distribution_expected_by_current_strain <- weighted_distribution_by_two_groups(
  data = analytic_05,
  group_var_1 = "expected_tercile_short",
  group_var_2 = "current_financial_strain_short"
)

within_expected_by_subjective_long <- make_metric_long(
  within_expected_by_subjective_insecurity
)

within_expected_by_prospective_long <- make_metric_long(
  within_expected_by_prospective_insecurity
)

within_expected_by_current_strain_long <- make_metric_long(
  within_expected_by_current_strain
)


# ------------------------------------------------------------
# 08. Additional heterogeneity summaries
# ------------------------------------------------------------

subjective_gradient_within_expected <- within_expected_by_subjective_insecurity %>%
  dplyr::filter(group_2 %in% c("Low", "High")) %>%
  dplyr::select(
    group_1,
    group_2,
    mean_observed_phq4,
    mean_expected_phq4,
    mean_residual_phq4,
    pct_higher_than_expected,
    pct_lower_than_expected
  ) %>%
  tidyr::pivot_wider(
    names_from = group_2,
    values_from = c(
      mean_observed_phq4,
      mean_expected_phq4,
      mean_residual_phq4,
      pct_higher_than_expected,
      pct_lower_than_expected
    )
  ) %>%
  dplyr::mutate(
    observed_difference_high_minus_low =
      mean_observed_phq4_High - mean_observed_phq4_Low,
    expected_difference_high_minus_low =
      mean_expected_phq4_High - mean_expected_phq4_Low,
    residual_difference_high_minus_low =
      mean_residual_phq4_High - mean_residual_phq4_Low,
    higher_than_expected_difference_high_minus_low =
      pct_higher_than_expected_High - pct_higher_than_expected_Low,
    lower_than_expected_difference_high_minus_low =
      pct_lower_than_expected_High - pct_lower_than_expected_Low
  )

expected_gradient_within_subjective <- within_expected_by_subjective_insecurity %>%
  dplyr::filter(group_1 %in% c("Low expected", "High expected")) %>%
  dplyr::select(
    group_1,
    group_2,
    mean_observed_phq4,
    mean_expected_phq4,
    mean_residual_phq4,
    pct_higher_than_expected,
    pct_lower_than_expected
  ) %>%
  tidyr::pivot_wider(
    names_from = group_1,
    values_from = c(
      mean_observed_phq4,
      mean_expected_phq4,
      mean_residual_phq4,
      pct_higher_than_expected,
      pct_lower_than_expected
    )
  ) %>%
  dplyr::mutate(
    observed_difference_high_expected_minus_low_expected =
      `mean_observed_phq4_High expected` - `mean_observed_phq4_Low expected`,
    expected_difference_high_expected_minus_low_expected =
      `mean_expected_phq4_High expected` - `mean_expected_phq4_Low expected`,
    residual_difference_high_expected_minus_low_expected =
      `mean_residual_phq4_High expected` - `mean_residual_phq4_Low expected`,
    higher_than_expected_difference_high_expected_minus_low_expected =
      `pct_higher_than_expected_High expected` - `pct_higher_than_expected_Low expected`,
    lower_than_expected_difference_high_expected_minus_low_expected =
      `pct_lower_than_expected_High expected` - `pct_lower_than_expected_Low expected`
  )


# ------------------------------------------------------------
# 09. Figures
# ------------------------------------------------------------

p_residual_by_expected_subjective <- ggplot2::ggplot(
  within_expected_by_subjective_insecurity,
  ggplot2::aes(
    x = group_2,
    y = mean_residual_phq4,
    fill = group_2
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.8,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::facet_wrap(~ group_1, nrow = 1) +
  ggplot2::scale_fill_manual(
    values = c(
      "Low" = unname(article_palette["mustard_light"]),
      "Moderate" = unname(article_palette["mustard_mid"]),
      "High" = unname(article_palette["mustard_dark"])
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Residual PHQ-4 within objective-position expected distress",
    subtitle = "Residual distress varies by subjective insecurity within each expected-PHQ-4 tercile",
    x = "Subjective insecurity level",
    y = "Mean residual PHQ-4"
  ) +
  main_theme(base_size = 13)

plot_oe_residual_data <- within_expected_by_subjective_long %>%
  dplyr::filter(
    metric %in% c(
      "Observed PHQ-4",
      "Expected PHQ-4",
      "Residual PHQ-4"
    )
  )

p_observed_expected_residual_within <- ggplot2::ggplot(
  plot_oe_residual_data,
  ggplot2::aes(
    x = group_2,
    y = value,
    fill = group_2
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_hline(
    data = plot_oe_residual_data %>%
      dplyr::filter(metric == "Residual PHQ-4"),
    ggplot2::aes(yintercept = 0),
    linewidth = 0.75,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::facet_grid(metric ~ group_1, scales = "free_y") +
  ggplot2::scale_fill_manual(
    values = c(
      "Low" = unname(article_palette["mustard_light"]),
      "Moderate" = unname(article_palette["mustard_mid"]),
      "High" = unname(article_palette["mustard_dark"])
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Observed, expected and residual PHQ-4 within objective-position strata",
    subtitle = "Columns show expected-PHQ-4 terciles; bars show subjective insecurity levels",
    x = "Subjective insecurity level",
    y = "Weighted mean"
  ) +
  main_theme(base_size = 12)

p_higher_than_expected_within <- ggplot2::ggplot(
  within_expected_by_subjective_insecurity,
  ggplot2::aes(
    x = group_2,
    y = pct_higher_than_expected,
    fill = group_2
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::facet_wrap(~ group_1, nrow = 1) +
  ggplot2::scale_fill_manual(
    values = c(
      "Low" = unname(article_palette["mustard_light"]),
      "Moderate" = unname(article_palette["mustard_mid"]),
      "High" = unname(article_palette["mustard_dark"])
    ),
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title = "Higher-than-expected distress within objective-position strata",
    subtitle = "Percentage classified above expected PHQ-4 by residual threshold",
    x = "Subjective insecurity level",
    y = "Weighted percentage"
  ) +
  main_theme(base_size = 13)

p_residual_heatmap <- ggplot2::ggplot(
  within_expected_by_subjective_insecurity,
  ggplot2::aes(
    x = group_2,
    y = group_1,
    fill = mean_residual_phq4
  )
) +
  ggplot2::geom_tile(color = "white", linewidth = 1) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", mean_residual_phq4)),
    size = 5,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::scale_fill_gradient2(
    low = unname(article_palette["mustard_light"]),
    mid = "white",
    high = unname(article_palette["purple_dark"]),
    midpoint = 0,
    name = "Mean\nresidual"
  ) +
  ggplot2::labs(
    title = "Residual PHQ-4 by expected distress and subjective insecurity",
    subtitle = "Cells show weighted mean residual PHQ-4",
    x = "Subjective insecurity level",
    y = "Objective-position expected distress"
  ) +
  main_theme(base_size = 13) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank()
  )

p_residual_by_expected_prospective <- ggplot2::ggplot(
  within_expected_by_prospective_insecurity,
  ggplot2::aes(
    x = group_2,
    y = mean_residual_phq4,
    fill = group_2
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.8,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::facet_wrap(~ group_1, nrow = 1) +
  ggplot2::scale_fill_manual(
    values = c(
      "None" = unname(article_palette["grey_light"]),
      "One" = unname(article_palette["mustard_mid"]),
      "Multiple" = unname(article_palette["mustard_dark"])
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Residual PHQ-4 by prospective insecurity within expected distress",
    subtitle = "Prospective insecurity includes health-shock protection, emergency support and job-loss threat",
    x = "Prospective insecurity level",
    y = "Mean residual PHQ-4"
  ) +
  main_theme(base_size = 13)

p_residual_by_expected_current_strain <- ggplot2::ggplot(
  within_expected_by_current_strain,
  ggplot2::aes(
    x = group_2,
    y = mean_residual_phq4,
    fill = group_2
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.8,
    color = unname(article_palette["grey_dark"])
  ) +
  ggplot2::facet_wrap(~ group_1, nrow = 1) +
  ggplot2::scale_fill_manual(
    values = c(
      "None" = unname(article_palette["grey_light"]),
      "One" = unname(article_palette["mustard_mid"]),
      "Multiple" = unname(article_palette["mustard_dark"])
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Residual PHQ-4 by current financial strain within expected distress",
    subtitle = "Current strain combines debt repayment problems and difficulty making ends meet",
    x = "Current financial strain level",
    y = "Mean residual PHQ-4"
  ) +
  main_theme(base_size = 13)

p_distribution_expected_subjective <- ggplot2::ggplot(
  distribution_expected_by_subjective_insecurity,
  ggplot2::aes(
    x = group_2,
    y = weighted_pct_total,
    fill = group_2
  )
) +
  ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
  ggplot2::facet_wrap(~ group_1, nrow = 1) +
  ggplot2::scale_fill_manual(
    values = c(
      "Low" = unname(article_palette["mustard_light"]),
      "Moderate" = unname(article_palette["mustard_mid"]),
      "High" = unname(article_palette["mustard_dark"])
    ),
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title = "Distribution of cases across expected distress and subjective insecurity",
    subtitle = "Percentages refer to the full valid expected-by-subjective-insecurity sample",
    x = "Subjective insecurity level",
    y = "Weighted percentage of total sample"
  ) +
  main_theme(base_size = 13)


# ------------------------------------------------------------
# 10. Export outputs
# ------------------------------------------------------------

output_excel <- file.path(
  script_tables_dir,
  "05_within_gradient_heterogeneity_tables.xlsx"
)

openxlsx::write.xlsx(
  list(
    sample_summary = sample_summary,
    variable_check = variable_check,
    expected_by_subjective = within_expected_by_subjective_insecurity,
    expected_by_prospective = within_expected_by_prospective_insecurity,
    expected_by_current = within_expected_by_current_strain,
    dist_expected_subjective = distribution_expected_by_subjective_insecurity,
    dist_expected_prospective = distribution_expected_by_prospective_insecurity,
    dist_expected_current = distribution_expected_by_current_strain,
    subjective_gradients = subjective_gradient_within_expected,
    expected_gradients = expected_gradient_within_subjective,
    long_subjective = within_expected_by_subjective_long,
    long_prospective = within_expected_by_prospective_long,
    long_current = within_expected_by_current_strain_long
  ),
  file = output_excel,
  overwrite = TRUE
)

readr::write_csv(
  sample_summary,
  file.path(script_csv_dir, "05_sample_summary.csv")
)

readr::write_csv(
  variable_check,
  file.path(script_csv_dir, "05_variable_check.csv")
)

readr::write_csv(
  within_expected_by_subjective_insecurity,
  file.path(script_csv_dir, "05_within_expected_by_subjective_insecurity.csv")
)

readr::write_csv(
  within_expected_by_prospective_insecurity,
  file.path(script_csv_dir, "05_within_expected_by_prospective_insecurity.csv")
)

readr::write_csv(
  within_expected_by_current_strain,
  file.path(script_csv_dir, "05_within_expected_by_current_financial_strain.csv")
)

readr::write_csv(
  distribution_expected_by_subjective_insecurity,
  file.path(script_csv_dir, "05_distribution_expected_by_subjective_insecurity.csv")
)

readr::write_csv(
  distribution_expected_by_prospective_insecurity,
  file.path(script_csv_dir, "05_distribution_expected_by_prospective_insecurity.csv")
)

readr::write_csv(
  distribution_expected_by_current_strain,
  file.path(script_csv_dir, "05_distribution_expected_by_current_financial_strain.csv")
)

readr::write_csv(
  subjective_gradient_within_expected,
  file.path(script_csv_dir, "05_subjective_gradient_within_expected.csv")
)

readr::write_csv(
  expected_gradient_within_subjective,
  file.path(script_csv_dir, "05_expected_gradient_within_subjective.csv")
)

analytic_05_path <- file.path(
  script_rds_dir,
  "05_within_gradient_dataset.rds"
)

saveRDS(
  analytic_05,
  analytic_05_path
)

metadata_05 <- list(
  script_name = script_name,
  input_rds = input_rds,
  expected_tercile_levels = expected_tercile_levels,
  subjective_insecurity_levels = subjective_insecurity_levels,
  prospective_insecurity_levels = prospective_insecurity_levels,
  current_financial_strain_levels = current_financial_strain_levels
)

metadata_05_path <- file.path(
  script_rds_dir,
  "05_within_gradient_metadata.rds"
)

saveRDS(
  metadata_05,
  metadata_05_path
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "05_residual_by_expected_tercile_subjective_insecurity.png"),
  plot = p_residual_by_expected_subjective,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "05_observed_expected_residual_by_expected_tercile_subjective_insecurity.png"),
  plot = p_observed_expected_residual_within,
  width = 15,
  height = 11,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "05_higher_than_expected_by_expected_tercile_subjective_insecurity.png"),
  plot = p_higher_than_expected_within,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "05_residual_heatmap_expected_by_subjective_insecurity.png"),
  plot = p_residual_heatmap,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "05_residual_by_expected_tercile_prospective_insecurity.png"),
  plot = p_residual_by_expected_prospective,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "05_residual_by_expected_tercile_current_financial_strain.png"),
  plot = p_residual_by_expected_current_strain,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "05_distribution_expected_by_subjective_insecurity.png"),
  plot = p_distribution_expected_subjective,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white"
)

manifest <- tibble::tibble(
  output = c(
    "Excel diagnostics",
    "Script 05 analytic dataset",
    "Script 05 metadata",
    "Main residual by expected tercile and subjective insecurity figure",
    "Observed expected residual by expected tercile and subjective insecurity figure",
    "Higher-than-expected by expected tercile and subjective insecurity figure",
    "Residual heatmap expected by subjective insecurity figure",
    "Residual by expected tercile and prospective insecurity figure",
    "Residual by expected tercile and current financial strain figure",
    "Distribution expected by subjective insecurity figure"
  ),
  path = c(
    output_excel,
    analytic_05_path,
    metadata_05_path,
    file.path(script_figures_dir, "05_residual_by_expected_tercile_subjective_insecurity.png"),
    file.path(script_figures_dir, "05_observed_expected_residual_by_expected_tercile_subjective_insecurity.png"),
    file.path(script_figures_dir, "05_higher_than_expected_by_expected_tercile_subjective_insecurity.png"),
    file.path(script_figures_dir, "05_residual_heatmap_expected_by_subjective_insecurity.png"),
    file.path(script_figures_dir, "05_residual_by_expected_tercile_prospective_insecurity.png"),
    file.path(script_figures_dir, "05_residual_by_expected_tercile_current_financial_strain.png"),
    file.path(script_figures_dir, "05_distribution_expected_by_subjective_insecurity.png")
  )
)

readr::write_csv(
  manifest,
  file.path(script_csv_dir, "05_output_manifest.csv")
)


# ------------------------------------------------------------
# 11. Save session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(script_logs_dir, "05_session_info.txt")
)


# ------------------------------------------------------------
# 12. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("05_within_gradient_heterogeneity.R completed")
message("============================================================")
message("Input file: ", input_rds)
message("Output folder: ", script_output_dir)
message(
  "Valid expected tercile + subjective insecurity cases: ",
  sample_summary$unweighted_n[
    sample_summary$sample_definition ==
      "Valid expected tercile and subjective insecurity level"
  ]
)
message("Excel diagnostics: ", output_excel)
message("Main dataset: ", analytic_05_path)
message("Figures saved in: ", script_figures_dir)
message("CSV outputs saved in: ", script_csv_dir)
message("RDS outputs saved in: ", script_rds_dir)
message("============================================================\n")
