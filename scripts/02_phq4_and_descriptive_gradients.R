# ============================================================
# 02_phq4_and_descriptive_gradients.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Document PHQ-4 distribution, reliability, severity categories,
#          and descriptive gradients by objective position and subjective
#          economic-security indicators
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Setup
# ------------------------------------------------------------

source("scripts/00_setup.R")

options(survey.lonely.psu = "adjust")

script_name <- "02_phq4_and_descriptive_gradients"

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
# 02. Load analytic data from script 01
# ------------------------------------------------------------

input_rds <- file.path(
  output_dir,
  "01_data_preparation",
  "rds",
  "01_ebs_analytic.rds"
)

variable_groups_path <- file.path(
  output_dir,
  "01_data_preparation",
  "rds",
  "01_variable_groups.rds"
)

if (!file.exists(input_rds)) {
  stop(
    paste0(
      "Input analytic dataset not found: ", input_rds, "\n",
      "Run scripts/01_data_preparation.R first."
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

survey_design <- survey::svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analytic,
  nest = TRUE
)


# ------------------------------------------------------------
# 03. Variable definitions
# ------------------------------------------------------------

phq4_item_vars <- c(
  "phq4_item_a",
  "phq4_item_b",
  "phq4_item_c",
  "phq4_item_d"
)

primary_outcome_var <- "phq4_score"

phq4_categorical_vars <- intersect(
  c("phq4_severity_f"),
  names(analytic)
)

objective_gradient_vars <- intersect(
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

subjective_security_gradient_vars <- intersect(
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
  names(analytic)
)

selected_table_1_vars <- intersect(
  c(
    "income_group_3cat_f",
    "education_3cat_f",
    "territory_insecurity_3cat_f",
    "sex_f",
    "health_financial_protection_3cat_f",
    "debt_status_f",
    "making_ends_meet_3cat_f",
    "subjective_insecurity_level_f"
  ),
  names(analytic)
)

variable_labels <- c(
  income_group_3cat_f = "Income group",
  education_3cat_f = "Education",
  housing_tenure_3cat_f = "Housing tenure",
  life_labor_stage_f = "Life-labor stage",
  health_system_3cat_f = "Health system",
  housing_deprivation_3cat_f = "Housing deprivation",
  territory_insecurity_3cat_f = "Territorial insecurity",
  sex_f = "Sex",
  macrozone_f = "Macrozone",
  health_financial_protection_3cat_f = "Health emergency protection",
  no_emergency_money_support_f = "Emergency monetary support",
  employment_risk_position_f = "Employment risk",
  debt_status_f = "Debt-payment situation",
  making_ends_meet_3cat_f = "Making ends meet",
  subjective_insecurity_level_f = "Subjective insecurity level",
  prospective_insecurity_level_f = "Prospective insecurity level",
  current_financial_strain_level_f = "Current financial strain"
)

if (!primary_outcome_var %in% names(analytic)) {
  stop("Primary outcome `phq4_score` was not found in the analytic dataset.")
}


# ------------------------------------------------------------
# 04. Local helper functions
# ------------------------------------------------------------

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

weighted_frequency_local <- function(data, var_name, weight_var = "weight") {
  if (!all(c(var_name, weight_var) %in% names(data))) {
    return(tibble::tibble())
  }
  
  non_missing <- data %>%
    dplyr::filter(!is.na(.data[[var_name]]), !is.na(.data[[weight_var]]))
  
  if (nrow(non_missing) == 0) {
    return(
      tibble::tibble(
        variable = var_name,
        variable_label = dplyr::recode(var_name, !!!variable_labels, .default = var_name),
        category = NA_character_,
        unweighted_n = 0L,
        weighted_n = 0,
        weighted_pct = NA_real_
      )
    )
  }
  
  total_w <- sum(non_missing[[weight_var]], na.rm = TRUE)
  
  non_missing %>%
    dplyr::mutate(category = as.character(.data[[var_name]])) %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      variable = var_name,
      variable_label = dplyr::recode(var_name, !!!variable_labels, .default = var_name),
      weighted_pct = 100 * weighted_n / total_w
    ) %>%
    dplyr::select(
      variable,
      variable_label,
      category,
      unweighted_n,
      weighted_n,
      weighted_pct
    ) %>%
    dplyr::arrange(variable, dplyr::desc(weighted_pct))
}

weighted_mean_by_group <- function(data,
                                   outcome_var,
                                   group_var,
                                   weight_var = "weight") {
  if (!all(c(outcome_var, group_var, weight_var) %in% names(data))) {
    return(tibble::tibble())
  }
  
  temp <- data %>%
    dplyr::filter(
      !is.na(.data[[outcome_var]]),
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
      weighted_pct = weighted_n / sum(temp[[weight_var]], na.rm = TRUE) * 100,
      mean_phq4 = weighted_mean_approx(.data[[outcome_var]], .data[[weight_var]]),
      sd_phq4_approx = weighted_sd_approx(.data[[outcome_var]], .data[[weight_var]]),
      .groups = "drop"
    ) %>%
    dplyr::rename(category = 1) %>%
    dplyr::mutate(
      variable = group_var,
      variable_label = dplyr::recode(group_var, !!!variable_labels, .default = group_var),
      category = as.character(category)
    ) %>%
    dplyr::select(
      variable,
      variable_label,
      category,
      unweighted_n,
      weighted_n,
      weighted_pct,
      mean_phq4,
      sd_phq4_approx
    )
}

cronbach_alpha_manual <- function(data, item_vars) {
  missing_items <- setdiff(item_vars, names(data))
  
  if (length(missing_items) > 0) {
    return(
      tibble::tibble(
        scale = "PHQ-4",
        n_complete = NA_integer_,
        items = length(item_vars),
        cronbach_alpha = NA_real_,
        note = paste("Missing item variables:", paste(missing_items, collapse = ", "))
      )
    )
  }
  
  item_data <- data %>%
    dplyr::select(dplyr::all_of(item_vars)) %>%
    tidyr::drop_na()
  
  k <- length(item_vars)
  
  if (nrow(item_data) == 0 || k < 2) {
    return(
      tibble::tibble(
        scale = "PHQ-4",
        n_complete = nrow(item_data),
        items = k,
        cronbach_alpha = NA_real_,
        note = "No complete cases or fewer than two items"
      )
    )
  }
  
  item_var_sum <- sum(apply(item_data, 2, stats::var, na.rm = TRUE))
  total_var <- stats::var(rowSums(item_data), na.rm = TRUE)
  
  alpha <- (k / (k - 1)) * (1 - item_var_sum / total_var)
  
  tibble::tibble(
    scale = "PHQ-4",
    n_complete = nrow(item_data),
    items = k,
    cronbach_alpha = alpha,
    note = NA_character_
  )
}


# ------------------------------------------------------------
# 05. PHQ-4 summary statistics
# ------------------------------------------------------------

phq4_vector <- analytic[[primary_outcome_var]]
weight_vector <- analytic$weight

phq4_quantiles <- weighted_quantile(
  phq4_vector,
  weight_vector,
  probs = c(0.10, 0.25, 0.50, 0.75, 0.90)
)

phq4_distribution_summary <- tibble::tibble(
  outcome = "PHQ-4 score",
  unweighted_n = sum(!is.na(phq4_vector)),
  weighted_n = sum(weight_vector[!is.na(phq4_vector) & !is.na(weight_vector)], na.rm = TRUE),
  weighted_mean = weighted_mean_approx(phq4_vector, weight_vector),
  weighted_sd = weighted_sd_approx(phq4_vector, weight_vector),
  min = min(phq4_vector, na.rm = TRUE),
  p10 = phq4_quantiles[1],
  p25 = phq4_quantiles[2],
  median = phq4_quantiles[3],
  p75 = phq4_quantiles[4],
  p90 = phq4_quantiles[5],
  max = max(phq4_vector, na.rm = TRUE)
)

phq4_score_distribution <- analytic %>%
  dplyr::filter(!is.na(phq4_score), !is.na(weight)) %>%
  dplyr::count(phq4_score, wt = weight, name = "weighted_n") %>%
  dplyr::mutate(
    unweighted_n = purrr::map_int(
      phq4_score,
      ~ sum(analytic$phq4_score == .x, na.rm = TRUE)
    ),
    weighted_pct = weighted_n / sum(weighted_n, na.rm = TRUE) * 100
  ) %>%
  dplyr::select(
    phq4_score,
    unweighted_n,
    weighted_n,
    weighted_pct
  )

phq4_severity_distribution <- if ("phq4_severity_f" %in% names(analytic)) {
  weighted_frequency_local(analytic, "phq4_severity_f", "weight") %>%
    dplyr::mutate(
      category = factor(
        category,
        levels = c("Normal", "Mild", "Moderate", "Severe")
      )
    ) %>%
    dplyr::arrange(category)
} else {
  tibble::tibble()
}

phq4_item_summaries <- purrr::map_dfr(
  intersect(phq4_item_vars, names(analytic)),
  function(item_var) {
    x <- analytic[[item_var]]
    w <- analytic$weight
    qs <- weighted_quantile(x, w)
    
    tibble::tibble(
      item = item_var,
      unweighted_n = sum(!is.na(x)),
      weighted_n = sum(w[!is.na(x) & !is.na(w)], na.rm = TRUE),
      weighted_mean = weighted_mean_approx(x, w),
      weighted_sd = weighted_sd_approx(x, w),
      min = min(x, na.rm = TRUE),
      p25 = qs[2],
      median = qs[3],
      p75 = qs[4],
      max = max(x, na.rm = TRUE)
    )
  }
)

phq4_reliability <- cronbach_alpha_manual(
  analytic,
  phq4_item_vars
)


# ------------------------------------------------------------
# 06. Descriptive gradients
# ------------------------------------------------------------

objective_phq4_gradients <- purrr::map_dfr(
  objective_gradient_vars,
  ~ weighted_mean_by_group(
    data = analytic,
    outcome_var = "phq4_score",
    group_var = .x,
    weight_var = "weight"
  )
)

subjective_security_phq4_gradients <- purrr::map_dfr(
  subjective_security_gradient_vars,
  ~ weighted_mean_by_group(
    data = analytic,
    outcome_var = "phq4_score",
    group_var = .x,
    weight_var = "weight"
  )
)

full_phq4_gradients <- dplyr::bind_rows(
  objective_phq4_gradients %>%
    dplyr::mutate(domain = "Objective position"),
  subjective_security_phq4_gradients %>%
    dplyr::mutate(domain = "Subjective economic security")
) %>%
  dplyr::relocate(domain, .before = variable)

selected_descriptive_gradients <- full_phq4_gradients %>%
  dplyr::filter(variable %in% selected_table_1_vars) %>%
  dplyr::arrange(
    factor(
      variable,
      levels = selected_table_1_vars
    ),
    category
  ) %>%
  dplyr::select(
    domain,
    variable,
    variable_label,
    category,
    unweighted_n,
    weighted_pct,
    mean_phq4
  )


# ------------------------------------------------------------
# 07. Sample summary and metadata
# ------------------------------------------------------------

sample_summary <- tibble::tibble(
  statistic = c(
    "Total rows in analytic data",
    "Total weighted population",
    "Valid survey design: weight, strata, PSU",
    "Valid PHQ-4",
    "Valid PHQ-4 severity",
    "Valid objective-position variables",
    "Valid PHQ-4 + objective-position variables",
    "Valid PHQ-4 + selected subjective-security variables"
  ),
  value = c(
    nrow(analytic),
    sum(analytic$weight, na.rm = TRUE),
    sum(!is.na(analytic$weight) & !is.na(analytic$strata) & !is.na(analytic$psu)),
    sum(!is.na(analytic$phq4_score)),
    if ("phq4_severity_f" %in% names(analytic)) {
      sum(!is.na(analytic$phq4_severity_f))
    } else {
      NA_real_
    },
    sum(stats::complete.cases(analytic[, objective_gradient_vars, drop = FALSE])),
    sum(stats::complete.cases(analytic[, c("phq4_score", objective_gradient_vars), drop = FALSE])),
    sum(stats::complete.cases(analytic[, c("phq4_score", subjective_security_gradient_vars), drop = FALSE]))
  )
)

phq4_outcome_role <- tibble::tribble(
  ~outcome, ~variable, ~role_in_article, ~interpretation_note,
  "PHQ-4", "phq4_score", "Primary psychological distress outcome", "Continuous 0-12 symptom measure; not interpreted as a clinical diagnosis",
  "PHQ-4 severity", "phq4_severity_f", "Descriptive severity indicator", "Used descriptively in supplementary materials; not used as the main regression outcome"
)

phq4_variable_map <- list(
  primary_outcome_var = primary_outcome_var,
  phq4_item_vars = phq4_item_vars,
  phq4_categorical_vars = phq4_categorical_vars,
  objective_gradient_vars = objective_gradient_vars,
  subjective_security_gradient_vars = subjective_security_gradient_vars,
  selected_table_1_vars = selected_table_1_vars,
  variable_labels = variable_labels,
  phq4_outcome_role = phq4_outcome_role
)


# ------------------------------------------------------------
# 08. Figures
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

p_phq4_distribution <- ggplot2::ggplot(
  phq4_score_distribution,
  ggplot2::aes(x = phq4_score, y = weighted_pct)
) +
  ggplot2::geom_col(
    width = 0.80,
    fill = article_palette["purple_dark"]
  ) +
  ggplot2::scale_x_continuous(breaks = 0:12) +
  ggplot2::labs(
    title = "Weighted distribution of PHQ-4",
    subtitle = "Primary psychological distress outcome",
    x = "PHQ-4 score",
    y = "Weighted percentage"
  ) +
  main_theme(base_size = 13)

if (nrow(phq4_severity_distribution) > 0) {
  p_phq4_severity <- ggplot2::ggplot(
    phq4_severity_distribution,
    ggplot2::aes(x = category, y = weighted_pct, fill = category)
  ) +
    ggplot2::geom_col(width = 0.75, show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "Normal" = article_palette["grey_light"],
        "Mild" = article_palette["purple_light"],
        "Moderate" = article_palette["purple_mid"],
        "Severe" = article_palette["purple_dark"]
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "Weighted distribution of PHQ-4 severity",
      subtitle = "Severity categories are descriptive and not interpreted as clinical diagnoses",
      x = NULL,
      y = "Weighted percentage"
    ) +
    main_theme(base_size = 13)
} else {
  p_phq4_severity <- NULL
}

selected_gradient_plot_data <- selected_descriptive_gradients %>%
  dplyr::mutate(
    variable_label = factor(
      variable_label,
      levels = unique(variable_label)
    ),
    category_wrapped = stringr::str_wrap(category, 30)
  )

p_selected_gradients <- ggplot2::ggplot(
  selected_gradient_plot_data,
  ggplot2::aes(
    x = mean_phq4,
    y = forcats::fct_reorder(category_wrapped, mean_phq4)
  )
) +
  ggplot2::geom_point(
    size = 3.2,
    color = article_palette["purple_dark"]
  ) +
  ggplot2::facet_wrap(
    ~ variable_label,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::labs(
    title = "Selected descriptive gradients in PHQ-4",
    subtitle = "Survey-weighted mean PHQ-4 by objective-position and subjective-security indicators",
    x = "Weighted mean PHQ-4",
    y = NULL
  ) +
  main_theme(base_size = 12)


# ------------------------------------------------------------
# 09. Export outputs
# ------------------------------------------------------------

output_excel <- file.path(
  script_tables_dir,
  "02_phq4_and_descriptive_gradients_tables.xlsx"
)

openxlsx::write.xlsx(
  list(
    sample_summary = sample_summary,
    outcome_role = phq4_outcome_role,
    phq4_summary = phq4_distribution_summary,
    phq4_scores = phq4_score_distribution,
    phq4_severity = phq4_severity_distribution,
    phq4_items = phq4_item_summaries,
    phq4_reliability = phq4_reliability,
    selected_gradients = selected_descriptive_gradients,
    full_gradients = full_phq4_gradients,
    objective_gradients = objective_phq4_gradients,
    subjective_gradients = subjective_security_phq4_gradients
  ),
  file = output_excel,
  overwrite = TRUE
)

readr::write_csv(
  sample_summary,
  file.path(script_csv_dir, "02_sample_summary.csv")
)

readr::write_csv(
  phq4_distribution_summary,
  file.path(script_csv_dir, "02_phq4_distribution_summary.csv")
)

readr::write_csv(
  phq4_score_distribution,
  file.path(script_csv_dir, "02_phq4_score_distribution.csv")
)

readr::write_csv(
  phq4_severity_distribution,
  file.path(script_csv_dir, "02_phq4_severity_distribution.csv")
)

readr::write_csv(
  phq4_item_summaries,
  file.path(script_csv_dir, "02_phq4_item_summaries.csv")
)

readr::write_csv(
  phq4_reliability,
  file.path(script_csv_dir, "02_phq4_reliability.csv")
)

readr::write_csv(
  selected_descriptive_gradients,
  file.path(script_csv_dir, "02_selected_descriptive_gradients.csv")
)

readr::write_csv(
  full_phq4_gradients,
  file.path(script_csv_dir, "02_full_phq4_gradients.csv")
)

saveRDS(
  phq4_variable_map,
  file.path(script_rds_dir, "02_phq4_variable_map.rds")
)

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "02_phq4_distribution.png"),
  plot = p_phq4_distribution,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

if (!is.null(p_phq4_severity)) {
  ggplot2::ggsave(
    filename = file.path(script_figures_dir, "02_phq4_severity_distribution.png"),
    plot = p_phq4_severity,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "white"
  )
}

ggplot2::ggsave(
  filename = file.path(script_figures_dir, "02_selected_descriptive_gradients.png"),
  plot = p_selected_gradients,
  width = 13,
  height = 10,
  dpi = 300,
  bg = "white"
)

manifest <- tibble::tibble(
  output = c(
    "Excel diagnostics",
    "PHQ-4 variable map",
    "Sample summary CSV",
    "PHQ-4 distribution summary CSV",
    "PHQ-4 score distribution CSV",
    "PHQ-4 severity distribution CSV",
    "PHQ-4 item summaries CSV",
    "PHQ-4 reliability CSV",
    "Selected descriptive gradients CSV",
    "Full PHQ-4 gradients CSV",
    "PHQ-4 distribution figure",
    "PHQ-4 severity distribution figure",
    "Selected descriptive gradients figure"
  ),
  path = c(
    output_excel,
    file.path(script_rds_dir, "02_phq4_variable_map.rds"),
    file.path(script_csv_dir, "02_sample_summary.csv"),
    file.path(script_csv_dir, "02_phq4_distribution_summary.csv"),
    file.path(script_csv_dir, "02_phq4_score_distribution.csv"),
    file.path(script_csv_dir, "02_phq4_severity_distribution.csv"),
    file.path(script_csv_dir, "02_phq4_item_summaries.csv"),
    file.path(script_csv_dir, "02_phq4_reliability.csv"),
    file.path(script_csv_dir, "02_selected_descriptive_gradients.csv"),
    file.path(script_csv_dir, "02_full_phq4_gradients.csv"),
    file.path(script_figures_dir, "02_phq4_distribution.png"),
    file.path(script_figures_dir, "02_phq4_severity_distribution.png"),
    file.path(script_figures_dir, "02_selected_descriptive_gradients.png")
  )
)

readr::write_csv(
  manifest,
  file.path(script_csv_dir, "02_output_manifest.csv")
)


# ------------------------------------------------------------
# 10. Save session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(script_logs_dir, "02_session_info.txt")
)


# ------------------------------------------------------------
# 11. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("02_phq4_and_descriptive_gradients.R completed")
message("============================================================")
message("Input file: ", input_rds)
message("Output folder: ", script_output_dir)
message("Excel output: ", output_excel)
message("Valid PHQ-4 cases: ", sum(!is.na(analytic$phq4_score)))
message(
  "PHQ-4 Cronbach alpha: ",
  round(phq4_reliability$cronbach_alpha[1], 3),
  " (complete-case n = ",
  phq4_reliability$n_complete[1],
  ")"
)
message("Selected descriptive gradient variables: ", paste(selected_table_1_vars, collapse = ", "))
message("Figures saved in: ", script_figures_dir)
message("CSV outputs saved in: ", script_csv_dir)
message("RDS outputs saved in: ", script_rds_dir)
message("============================================================\n")
