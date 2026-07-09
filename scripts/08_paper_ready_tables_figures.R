# ============================================================
# 08_paper_ready_tables_figures.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Create paper-ready tables and title-free figures from scripts 02-07
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Setup
# ------------------------------------------------------------

source("scripts/00_setup.R")

script_name <- "08_paper_ready_tables_figures"

extra_packages <- c(
  "patchwork",
  "fs"
)

missing_extra_packages <- extra_packages[
  !extra_packages %in% rownames(installed.packages())
]

if (length(missing_extra_packages) > 0) {
  stop(
    paste0(
      "The following additional packages are missing: ",
      paste(missing_extra_packages, collapse = ", "),
      ".\nPlease install them manually or run renv::restore() if using renv."
    )
  )
}

invisible(
  lapply(extra_packages, library, character.only = TRUE)
)

script_output_dir <- file.path(output_dir, script_name)

figures_main_dir       <- file.path(script_output_dir, "figures_main")
figures_supplement_dir <- file.path(script_output_dir, "figures_supplement")
tables_main_dir        <- file.path(script_output_dir, "tables_main")
tables_supplement_dir  <- file.path(script_output_dir, "tables_supplement")
script_csv_dir         <- file.path(script_output_dir, "csv")
script_rds_dir         <- file.path(script_output_dir, "rds")
script_logs_dir        <- file.path(script_output_dir, "logs")

dirs_to_create <- c(
  script_output_dir,
  figures_main_dir,
  figures_supplement_dir,
  tables_main_dir,
  tables_supplement_dir,
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
# 02. Input folders
# ------------------------------------------------------------

input_dirs <- list(
  script_02 = file.path(output_dir, "02_phq4_and_descriptive_gradients"),
  script_03 = file.path(output_dir, "03_objective_gradient_residual_phq4"),
  script_04 = file.path(output_dir, "04_subjective_security_residual_associations"),
  script_05 = file.path(output_dir, "05_within_gradient_heterogeneity"),
  script_06 = file.path(output_dir, "06_configurational_diagnostics_expected_residual"),
  script_07 = file.path(output_dir, "07_robustness_sensitivity_checks")
)

input_folder_check <- tibble::tibble(
  script = names(input_dirs),
  path = unlist(input_dirs),
  exists = file.exists(path)
)

missing_input_folders <- input_folder_check %>%
  dplyr::filter(!exists)

if (nrow(missing_input_folders) > 0) {
  warning(
    "Some input folders were not found:\n",
    paste(missing_input_folders$path, collapse = "\n")
  )
}


# ------------------------------------------------------------
# 03. Article palette and paper-ready plotting style
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

subjective_level_palette <- c(
  "Low" = unname(article_palette["mustard_light"]),
  "Moderate" = unname(article_palette["mustard_mid"]),
  "High" = unname(article_palette["mustard_dark"]),
  "Low subjective insecurity" = unname(article_palette["mustard_light"]),
  "Moderate subjective insecurity" = unname(article_palette["mustard_mid"]),
  "High subjective insecurity" = unname(article_palette["mustard_dark"])
)

security_block_palette <- c(
  "Prospective protection/threat" = unname(article_palette["purple_dark"]),
  "Current financial strain" = unname(article_palette["mustard_dark"]),
  "Other" = unname(article_palette["grey_dark"])
)

paper_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(
        size = base_size - 2,
        color = unname(article_palette["grey_mid"]),
        hjust = 0
      ),
      axis.title = ggplot2::element_text(
        size = base_size,
        color = unname(article_palette["grey_dark"])
      ),
      axis.text = ggplot2::element_text(
        size = base_size - 1,
        color = unname(article_palette["grey_dark"])
      ),
      legend.title = ggplot2::element_text(
        size = base_size - 1,
        color = unname(article_palette["grey_dark"])
      ),
      legend.text = ggplot2::element_text(
        size = base_size - 2,
        color = unname(article_palette["grey_dark"])
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = base_size - 1,
        color = unname(article_palette["grey_dark"])
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = unname(article_palette["grey_light"]),
        linewidth = 0.25
      ),
      panel.grid.major.y = ggplot2::element_line(
        color = unname(article_palette["grey_light"]),
        linewidth = 0.25
      ),
      axis.line = ggplot2::element_line(
        color = unname(article_palette["grey_light"]),
        linewidth = 0.25
      ),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )
}

paper_theme_no_legend <- function(base_size = 11) {
  paper_theme(base_size = base_size) +
    ggplot2::theme(legend.position = "none")
}


# ------------------------------------------------------------
# 04. Helper functions
# ------------------------------------------------------------

find_existing_file <- function(candidates) {
  candidates <- candidates[!is.na(candidates)]
  existing <- candidates[file.exists(candidates)]
  
  if (length(existing) == 0) {
    return(NA_character_)
  }
  
  existing[1]
}

copy_if_exists <- function(source_path, target_path, required = FALSE) {
  if (is.na(source_path) || !file.exists(source_path)) {
    if (required) {
      warning("Required file was not found: ", source_path)
    }
    
    return(
      tibble::tibble(
        source_path = source_path,
        target_path = target_path,
        copied = FALSE,
        status = "Source file not found"
      )
    )
  }
  
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(source_path, target_path, overwrite = TRUE)
  
  tibble::tibble(
    source_path = source_path,
    target_path = target_path,
    copied = file.exists(target_path),
    status = ifelse(file.exists(target_path), "Copied", "Copy failed")
  )
}

read_csv_if_exists <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(tibble::tibble())
  }
  
  readr::read_csv(path, show_col_types = FALSE) %>%
    janitor::clean_names()
}

read_rds_if_exists <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  
  readRDS(path) %>%
    janitor::clean_names()
}

safe_excel_sheet_names <- function(named_list) {
  old_names <- names(named_list)
  
  new_names <- old_names %>%
    stringr::str_replace_all("[^A-Za-z0-9_]", "_") %>%
    stringr::str_sub(1, 31)
  
  duplicated_names <- duplicated(new_names)
  
  if (any(duplicated_names)) {
    new_names[duplicated_names] <- paste0(
      stringr::str_sub(new_names[duplicated_names], 1, 27),
      "_",
      seq_len(sum(duplicated_names))
    )
  }
  
  names(named_list) <- new_names
  named_list
}

save_paper_figure <- function(plot, filename_base, width, height, dpi = 600) {
  png_path <- file.path(figures_main_dir, paste0(filename_base, ".png"))
  pdf_path <- file.path(figures_main_dir, paste0(filename_base, ".pdf"))
  
  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  
  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height,
    device = grDevices::cairo_pdf,
    bg = "white"
  )
  
  tibble::tibble(
    figure = filename_base,
    png_path = png_path,
    pdf_path = pdf_path,
    width = width,
    height = height,
    dpi = dpi,
    generated = file.exists(png_path) & file.exists(pdf_path)
  )
}

save_supplement_figure <- function(plot, filename_base, width, height, dpi = 600) {
  png_path <- file.path(figures_supplement_dir, paste0(filename_base, ".png"))
  pdf_path <- file.path(figures_supplement_dir, paste0(filename_base, ".pdf"))
  
  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  
  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height,
    device = grDevices::cairo_pdf,
    bg = "white"
  )
  
  tibble::tibble(
    figure = filename_base,
    png_path = png_path,
    pdf_path = pdf_path,
    width = width,
    height = height,
    dpi = dpi,
    generated = file.exists(png_path) & file.exists(pdf_path)
  )
}

clean_term_label <- function(x) {
  x %>%
    stringr::str_replace_all("`", "") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_replace_all("health financial protection 3cat f", "Health emergency: ") %>%
    stringr::str_replace_all("no emergency money support f", "Emergency support: ") %>%
    stringr::str_replace_all("employment risk position f", "Employment risk: ") %>%
    stringr::str_replace_all("debt status f", "Debt: ") %>%
    stringr::str_replace_all("making ends meet 3cat f", "Making ends meet: ") %>%
    stringr::str_replace_all("Financially protected in health emergency", "financially protected") %>%
    stringr::str_replace_all("Neither protected nor unprotected in health emergency", "neither protected nor unprotected") %>%
    stringr::str_replace_all("Financially unprotected in health emergency", "financially unprotected") %>%
    stringr::str_replace_all("Has emergency money support", "has emergency monetary support") %>%
    stringr::str_replace_all("No emergency money support", "no emergency monetary support") %>%
    stringr::str_replace_all("Employed, low perceived job-loss risk", "employed, low job-loss risk") %>%
    stringr::str_replace_all("Employed, uncertain perceived job-loss risk", "employed, uncertain job-loss risk") %>%
    stringr::str_replace_all("Employed, high perceived job-loss risk", "employed, high job-loss risk") %>%
    stringr::str_replace_all("Outside current employment context", "outside current employment context") %>%
    stringr::str_replace_all("Debt, all payments on time", "debt, all payments on time") %>%
    stringr::str_replace_all("Debt, some payment problems", "debt, some payment problems") %>%
    stringr::str_replace_all("Debt, no payments on time", "debt, no payments on time") %>%
    stringr::str_replace_all("No debt", "no debt") %>%
    stringr::str_replace_all("Ease making ends meet", "ease") %>%
    stringr::str_replace_all("Neither difficulty nor ease", "neither difficulty nor ease") %>%
    stringr::str_replace_all("Difficulty making ends meet", "difficulty") %>%
    stringr::str_squish()
}


# ------------------------------------------------------------
# 05. Locate input files
# ------------------------------------------------------------

rds_sources <- list(
  dataset_03 = find_existing_file(c(
    file.path(input_dirs$script_03, "rds", "03_objective_gradient_residual_dataset.rds")
  )),
  dataset_04 = find_existing_file(c(
    file.path(input_dirs$script_04, "rds", "04_subjective_security_residual_dataset.rds")
  )),
  dataset_05 = find_existing_file(c(
    file.path(input_dirs$script_05, "rds", "05_within_gradient_dataset.rds")
  )),
  dataset_06 = find_existing_file(c(
    file.path(input_dirs$script_06, "rds", "06_configurational_diagnostics_dataset.rds")
  ))
)

csv_sources <- list(
  sample_02 = find_existing_file(c(
    file.path(input_dirs$script_02, "csv", "02_sample_summary.csv")
  )),
  phq4_summary_02 = find_existing_file(c(
    file.path(input_dirs$script_02, "csv", "02_phq4_distribution_summary.csv")
  )),
  phq4_reliability_02 = find_existing_file(c(
    file.path(input_dirs$script_02, "csv", "02_phq4_reliability.csv")
  )),
  selected_gradients_02 = find_existing_file(c(
    file.path(input_dirs$script_02, "csv", "02_selected_descriptive_gradients.csv")
  )),
  model_fit_03 = find_existing_file(c(
    file.path(input_dirs$script_03, "csv", "03_objective_model_fit.csv")
  )),
  residual_diagnostics_03 = find_existing_file(c(
    file.path(input_dirs$script_03, "csv", "03_residual_diagnostics.csv")
  )),
  residual_coefficients_04 = find_existing_file(c(
    file.path(input_dirs$script_04, "csv", "04_residual_model_coefficients.csv")
  )),
  residual_model_fit_04 = find_existing_file(c(
    file.path(input_dirs$script_04, "csv", "04_residual_model_fit.csv")
  )),
  within_gradient_05 = find_existing_file(c(
    file.path(input_dirs$script_05, "csv", "05_within_expected_by_subjective_insecurity.csv")
  )),
  config_summary_06 = find_existing_file(c(
    file.path(input_dirs$script_06, "csv", "06_diagnostic_summary.csv")
  )),
  config_distribution_06 = find_existing_file(c(
    file.path(input_dirs$script_06, "csv", "06_diagnostic_distribution.csv")
  )),
  threshold_sensitivity_07 = find_existing_file(c(
    file.path(input_dirs$script_07, "csv", "07_threshold_configuration_distribution.csv")
  )),
  no_meet_07 = find_existing_file(c(
    file.path(input_dirs$script_07, "csv", "07_no_meet_insecurity_by_configuration.csv")
  )),
  no_sex_07 = find_existing_file(c(
    file.path(input_dirs$script_07, "csv", "07_no_sex_comparison.csv")
  ))
)

rds_source_check <- tibble::tibble(
  object_name = names(rds_sources),
  path = unlist(rds_sources),
  exists = file.exists(path)
)

csv_source_check <- tibble::tibble(
  object_name = names(csv_sources),
  path = unlist(csv_sources),
  exists = file.exists(path)
)


# ------------------------------------------------------------
# 06. Load source data
# ------------------------------------------------------------

data_03 <- read_rds_if_exists(rds_sources$dataset_03)
data_04 <- read_rds_if_exists(rds_sources$dataset_04)
data_05 <- read_rds_if_exists(rds_sources$dataset_05)
data_06 <- read_rds_if_exists(rds_sources$dataset_06)

sample_02 <- read_csv_if_exists(csv_sources$sample_02)
phq4_summary_02 <- read_csv_if_exists(csv_sources$phq4_summary_02)
phq4_reliability_02 <- read_csv_if_exists(csv_sources$phq4_reliability_02)
selected_gradients_02 <- read_csv_if_exists(csv_sources$selected_gradients_02)

model_fit_03 <- read_csv_if_exists(csv_sources$model_fit_03)
residual_diagnostics_03 <- read_csv_if_exists(csv_sources$residual_diagnostics_03)

residual_coefficients_04 <- read_csv_if_exists(csv_sources$residual_coefficients_04)
residual_model_fit_04 <- read_csv_if_exists(csv_sources$residual_model_fit_04)

within_gradient_05 <- read_csv_if_exists(csv_sources$within_gradient_05)

config_summary_06 <- read_csv_if_exists(csv_sources$config_summary_06)
config_distribution_06 <- read_csv_if_exists(csv_sources$config_distribution_06)

threshold_sensitivity_07 <- read_csv_if_exists(csv_sources$threshold_sensitivity_07)
no_meet_07 <- read_csv_if_exists(csv_sources$no_meet_07)
no_sex_07 <- read_csv_if_exists(csv_sources$no_sex_07)


# ------------------------------------------------------------
# 07. Figure 1: expected and residual PHQ-4
# ------------------------------------------------------------

figure_generation_manifest <- list()

if (!is.null(data_03) &&
    all(c("phq4_expected_objective", "phq4_residual_objective", "weight") %in% names(data_03))) {
  
  figure_01_data <- data_03 %>%
    dplyr::filter(
      !is.na(phq4_expected_objective),
      !is.na(phq4_residual_objective),
      !is.na(weight)
    )
  
  p_fig1_expected <- ggplot2::ggplot(
    figure_01_data,
    ggplot2::aes(x = phq4_expected_objective, weight = weight)
  ) +
    ggplot2::geom_histogram(
      bins = 45,
      fill = unname(article_palette["purple_mid"]),
      color = "white",
      linewidth = 0.20
    ) +
    ggplot2::labs(
      x = "Objective-position expected PHQ-4",
      y = "Weighted count"
    ) +
    paper_theme_no_legend(base_size = 11)
  
  p_fig1_residual <- ggplot2::ggplot(
    figure_01_data,
    ggplot2::aes(x = phq4_residual_objective, weight = weight)
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 0.60,
      color = unname(article_palette["grey_dark"])
    ) +
    ggplot2::geom_histogram(
      bins = 50,
      fill = unname(article_palette["mustard_mid"]),
      color = "white",
      linewidth = 0.20
    ) +
    ggplot2::labs(
      x = "Residual PHQ-4",
      y = "Weighted count"
    ) +
    paper_theme_no_legend(base_size = 11)
  
  figure_01 <- p_fig1_expected + p_fig1_residual +
    patchwork::plot_layout(ncol = 2, widths = c(1, 1)) +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        face = "bold",
        size = 12,
        color = unname(article_palette["grey_dark"])
      )
    )
  
  figure_generation_manifest$figure_01 <- save_paper_figure(
    plot = figure_01,
    filename_base = "figure_01_expected_and_residual_phq4",
    width = 7.5,
    height = 3.8,
    dpi = 600
  )
  
  figure_generation_manifest$figure_s_expected <- save_supplement_figure(
    plot = p_fig1_expected,
    filename_base = "figure_s_expected_phq4_distribution_title_free",
    width = 6.5,
    height = 4.2,
    dpi = 600
  )
  
  figure_generation_manifest$figure_s_residual <- save_supplement_figure(
    plot = p_fig1_residual,
    filename_base = "figure_s_residual_phq4_distribution_title_free",
    width = 6.5,
    height = 4.2,
    dpi = 600
  )
  
} else {
  warning("Figure 1 could not be generated because script 03 data or variables were missing.")
}


# ------------------------------------------------------------
# 08. Figure 2: subjective security and residual PHQ-4
# ------------------------------------------------------------

if (nrow(residual_coefficients_04) > 0 &&
    all(c("model", "term", "estimate") %in% names(residual_coefficients_04))) {
  
  coefficient_data <- residual_coefficients_04 %>%
    dplyr::mutate(
      model_clean = as.character(model) %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_squish()
    ) %>%
    dplyr::filter(
      model_clean == "R3 Full subjective security",
      term != "(Intercept)"
    )
  
  if (nrow(coefficient_data) == 0) {
    stop(
      "Figure 2 could not be generated because model 'R3 Full subjective security' was not found."
    )
  }
  
  coefficient_names <- names(coefficient_data)
  
  if (all(c("conf_low", "conf_high") %in% coefficient_names)) {
    coefficient_data <- coefficient_data %>%
      dplyr::mutate(
        conf_low_final = conf_low,
        conf_high_final = conf_high
      )
  } else if (all(c("std_error", "estimate") %in% coefficient_names)) {
    coefficient_data <- coefficient_data %>%
      dplyr::mutate(
        conf_low_final = estimate - 1.96 * std_error,
        conf_high_final = estimate + 1.96 * std_error
      )
  } else {
    coefficient_data <- coefficient_data %>%
      dplyr::mutate(
        conf_low_final = estimate,
        conf_high_final = estimate
      )
  }
  
  coefficient_data <- coefficient_data %>%
    dplyr::mutate(
      term_label = dplyr::case_when(
        stringr::str_detect(term, stringr::regex("health_financial_protection_3cat_fNeither protected", ignore_case = TRUE)) ~
          "Health emergency: neither protected nor unprotected",
        stringr::str_detect(term, stringr::regex("health_financial_protection_3cat_fFinancially protected", ignore_case = TRUE)) ~
          "Health emergency: financially protected",
        stringr::str_detect(term, stringr::regex("no_emergency_money_support_fNo emergency", ignore_case = TRUE)) ~
          "No emergency monetary support",
        stringr::str_detect(term, stringr::regex("employment_risk_position_fEmployed, uncertain", ignore_case = TRUE)) ~
          "Employment: uncertain job-loss risk",
        stringr::str_detect(term, stringr::regex("employment_risk_position_fEmployed, high", ignore_case = TRUE)) ~
          "Employment: high job-loss risk",
        stringr::str_detect(term, stringr::regex("employment_risk_position_fOutside current employment", ignore_case = TRUE)) ~
          "Outside current employment context",
        stringr::str_detect(term, stringr::regex("debt_status_fDebt, all payments on time", ignore_case = TRUE)) ~
          "Debt: all payments on time",
        stringr::str_detect(term, stringr::regex("debt_status_fDebt, some payment problems", ignore_case = TRUE)) ~
          "Debt: some payment problems",
        stringr::str_detect(term, stringr::regex("debt_status_fDebt, no payments on time", ignore_case = TRUE)) ~
          "Debt: no payments on time",
        stringr::str_detect(term, stringr::regex("making_ends_meet_3cat_fNeither difficulty nor ease", ignore_case = TRUE)) ~
          "Making ends meet: neither difficulty nor ease",
        stringr::str_detect(term, stringr::regex("making_ends_meet_3cat_fEase making ends meet", ignore_case = TRUE)) ~
          "Making ends meet: ease",
        TRUE ~ clean_term_label(term)
      ),
      subjective_block = dplyr::case_when(
        stringr::str_detect(
          term_label,
          stringr::regex("Health emergency|emergency monetary support|Employment|Outside current employment", ignore_case = TRUE)
        ) ~ "Prospective protection/threat",
        stringr::str_detect(
          term_label,
          stringr::regex("Debt|Making ends meet", ignore_case = TRUE)
        ) ~ "Current financial strain",
        TRUE ~ "Other"
      ),
      subjective_block = factor(
        subjective_block,
        levels = c(
          "Prospective protection/threat",
          "Current financial strain",
          "Other"
        )
      )
    ) %>%
    dplyr::filter(
      !stringr::str_detect(
        term_label,
        stringr::regex("Outside current employment context", ignore_case = TRUE)
      )
    ) %>%
    dplyr::arrange(estimate) %>%
    dplyr::mutate(
      term_label = factor(term_label, levels = unique(term_label))
    )
  
  readr::write_csv(
    coefficient_data,
    file.path(script_csv_dir, "08_figure_02_selected_coefficients.csv")
  )
  
  figure_02 <- ggplot2::ggplot(
    coefficient_data,
    ggplot2::aes(
      x = estimate,
      y = term_label,
      xmin = conf_low_final,
      xmax = conf_high_final,
      color = subjective_block
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 0.55,
      color = unname(article_palette["grey_mid"])
    ) +
    ggplot2::geom_pointrange(
      linewidth = 0.55,
      size = 0.45
    ) +
    ggplot2::scale_color_manual(
      values = security_block_palette,
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Association with residual PHQ-4",
      y = NULL,
      color = "Subjective-security block"
    ) +
    paper_theme(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text.y = ggplot2::element_text(size = 9),
      axis.title.x = ggplot2::element_text(size = 10)
    )
  
  figure_generation_manifest$figure_02 <- save_paper_figure(
    plot = figure_02,
    filename_base = "figure_02_subjective_security_residual_phq4",
    width = 7.5,
    height = 5.1,
    dpi = 600
  )
  
} else {
  warning("Figure 2 could not be generated because script 04 coefficients were missing.")
}


# ------------------------------------------------------------
# 09. Figure 3: residual PHQ-4 within expected distress
# ------------------------------------------------------------

figure_03_source <- dplyr::coalesce(
  list(data_05, data_04, data_03)[[which(!purrr::map_lgl(list(data_05, data_04, data_03), is.null))[1]]],
  NULL
)

if (!is.null(figure_03_source) &&
    all(c(
      "phq4_expected_tercile_f",
      "subjective_insecurity_level_f",
      "phq4_residual_objective",
      "weight"
    ) %in% names(figure_03_source))) {
  
  figure_03_data <- figure_03_source %>%
    dplyr::filter(
      !is.na(phq4_expected_tercile_f),
      !is.na(subjective_insecurity_level_f),
      !is.na(phq4_residual_objective),
      !is.na(weight)
    ) %>%
    dplyr::mutate(
      expected_position_2cat = dplyr::case_when(
        as.character(phq4_expected_tercile_f) %in% c(
          "Low expected distress",
          "Intermediate expected distress",
          "Low expected",
          "Intermediate expected"
        ) ~ "Low/intermediate expected distress",
        as.character(phq4_expected_tercile_f) %in% c(
          "High expected distress",
          "High expected"
        ) ~ "High expected distress",
        TRUE ~ NA_character_
      ),
      expected_position_2cat = factor(
        expected_position_2cat,
        levels = c(
          "Low/intermediate expected distress",
          "High expected distress"
        )
      ),
      subjective_insecurity_short = dplyr::case_when(
        stringr::str_detect(as.character(subjective_insecurity_level_f), stringr::regex("^low", ignore_case = TRUE)) ~ "Low",
        stringr::str_detect(as.character(subjective_insecurity_level_f), stringr::regex("^moderate", ignore_case = TRUE)) ~ "Moderate",
        stringr::str_detect(as.character(subjective_insecurity_level_f), stringr::regex("^high", ignore_case = TRUE)) ~ "High",
        TRUE ~ as.character(subjective_insecurity_level_f)
      ),
      subjective_insecurity_short = factor(
        subjective_insecurity_short,
        levels = c("Low", "Moderate", "High")
      )
    ) %>%
    dplyr::filter(
      !is.na(expected_position_2cat),
      !is.na(subjective_insecurity_short)
    ) %>%
    dplyr::group_by(expected_position_2cat, subjective_insecurity_short) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(weight, na.rm = TRUE),
      mean_residual_phq4 = weighted_mean_approx(phq4_residual_objective, weight),
      .groups = "drop"
    )
  
  readr::write_csv(
    figure_03_data,
    file.path(script_csv_dir, "08_figure_03_selected_within_gradient_data.csv")
  )
  
  figure_03 <- ggplot2::ggplot(
    figure_03_data,
    ggplot2::aes(
      x = subjective_insecurity_short,
      y = mean_residual_phq4,
      fill = subjective_insecurity_short
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.55,
      color = unname(article_palette["grey_dark"])
    ) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::facet_wrap(
      ~ expected_position_2cat,
      nrow = 1
    ) +
    ggplot2::scale_fill_manual(
      values = subjective_level_palette,
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Subjective insecurity level",
      y = "Mean residual PHQ-4"
    ) +
    paper_theme_no_legend(base_size = 11)
  
  figure_generation_manifest$figure_03 <- save_paper_figure(
    plot = figure_03,
    filename_base = "figure_03_residual_within_expected_distress",
    width = 7.2,
    height = 3.8,
    dpi = 600
  )
  
} else {
  warning("Figure 3 could not be generated because required variables were missing.")
}


# ------------------------------------------------------------
# 10. Figure 4: expected-position by residual excess
# ------------------------------------------------------------

figure_04_source <- dplyr::coalesce(
  list(data_06, data_05, data_04)[[which(!purrr::map_lgl(list(data_06, data_05, data_04), is.null))[1]]],
  NULL
)

if (!is.null(figure_04_source) &&
    all(c(
      "phq4_expected_tercile_f",
      "phq4_residual_objective",
      "higher_than_expected",
      "weight"
    ) %in% names(figure_04_source))) {
  
  figure_04_data <- figure_04_source %>%
    dplyr::mutate(
      expected_position_2cat = dplyr::case_when(
        as.character(phq4_expected_tercile_f) %in% c(
          "Low expected distress",
          "Intermediate expected distress",
          "Low expected",
          "Intermediate expected"
        ) ~ "Low/intermediate expected distress",
        as.character(phq4_expected_tercile_f) %in% c(
          "High expected distress",
          "High expected"
        ) ~ "High expected distress",
        TRUE ~ NA_character_
      ),
      expected_position_2cat = factor(
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
      residual_excess_2cat = factor(
        residual_excess_2cat,
        levels = c(
          "Not higher than expected",
          "Higher than expected"
        )
      ),
      high_subjective_insecurity = dplyr::case_when(
        "high_subjective_insecurity" %in% names(.) ~ as.numeric(high_subjective_insecurity),
        "subjective_insecurity_level_f" %in% names(.) &
          stringr::str_detect(
            as.character(subjective_insecurity_level_f),
            stringr::regex("^high", ignore_case = TRUE)
          ) ~ 1,
        "subjective_insecurity_level_f" %in% names(.) &
          !is.na(subjective_insecurity_level_f) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::filter(
      !is.na(expected_position_2cat),
      !is.na(residual_excess_2cat),
      !is.na(phq4_residual_objective),
      !is.na(weight)
    ) %>%
    dplyr::group_by(expected_position_2cat, residual_excess_2cat) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(weight, na.rm = TRUE),
      mean_residual_phq4 = weighted_mean_approx(phq4_residual_objective, weight),
      pct_high_subjective_insecurity =
        100 * weighted_mean_approx(high_subjective_insecurity, weight),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE),
      label_text = paste0(
        sprintf("%.1f", weighted_pct), "%\n",
        "Residual: ", sprintf("%.2f", mean_residual_phq4), "\n",
        "High subj. ins.: ", sprintf("%.1f", pct_high_subjective_insecurity), "%"
      ),
      label_color = dplyr::if_else(
        mean_residual_phq4 > 1,
        "white",
        unname(article_palette["grey_dark"])
      )
    )
  
  readr::write_csv(
    figure_04_data,
    file.path(script_csv_dir, "08_figure_04_expected_residual_matrix_data.csv")
  )
  
  figure_04 <- ggplot2::ggplot(
    figure_04_data,
    ggplot2::aes(
      x = residual_excess_2cat,
      y = expected_position_2cat,
      fill = mean_residual_phq4
    )
  ) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = 1.0
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = label_text,
        color = label_color
      ),
      size = 3.5,
      lineheight = 1.05
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
      x = "Residual PHQ-4 position",
      y = "Objective-position expected PHQ-4"
    ) +
    paper_theme(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  
  figure_generation_manifest$figure_04 <- save_paper_figure(
    plot = figure_04,
    filename_base = "figure_04_expected_position_by_residual_excess",
    width = 6.8,
    height = 4.8,
    dpi = 600
  )
  
} else {
  warning("Figure 4 could not be generated because required variables were missing.")
}


# ------------------------------------------------------------
# 11. Copy selected supplementary figures
# ------------------------------------------------------------

supplement_figure_dictionary <- tibble::tribble(
  ~paper_item, ~source_script, ~source_file, ~target_file, ~notes,
  
  "Figure S1", "02", "02_phq4_distribution.png",
  "figure_s01_phq4_distribution.png",
  "Weighted PHQ-4 distribution.",
  
  "Figure S2", "02", "02_phq4_severity_distribution.png",
  "figure_s02_phq4_severity_distribution.png",
  "Weighted PHQ-4 severity distribution.",
  
  "Figure S3", "02", "02_selected_descriptive_gradients.png",
  "figure_s03_selected_descriptive_gradients.png",
  "Selected descriptive PHQ-4 gradients.",
  
  "Figure S4", "03", "03_objective_model_r2.png",
  "figure_s04_objective_model_r2.png",
  "Objective model R-squared sequence.",
  
  "Figure S5", "03", "03_objective_gradient_phq4.png",
  "figure_s05_objective_gradient_phq4.png",
  "Observed PHQ-4 by objective-position variables.",
  
  "Figure S6", "03", "03_expected_phq4_distribution.png",
  "figure_s06_expected_phq4_distribution_original.png",
  "Expected PHQ-4 distribution.",
  
  "Figure S7", "03", "03_residual_phq4_distribution.png",
  "figure_s07_residual_phq4_distribution_original.png",
  "Residual PHQ-4 distribution.",
  
  "Figure S8", "04", "04_higher_than_expected_odds.png",
  "figure_s08_higher_than_expected_odds.png",
  "Higher-than-expected logistic model.",
  
  "Figure S9", "04", "04_lower_than_expected_odds.png",
  "figure_s09_lower_than_expected_odds.png",
  "Lower-than-expected logistic model.",
  
  "Figure S10", "05", "05_higher_than_expected_by_expected_tercile_subjective_insecurity.png",
  "figure_s10_higher_than_expected_by_expected_tercile_subjective_insecurity.png",
  "Higher-than-expected distress within expected strata.",
  
  "Figure S11", "06", "06_diagnostic_configuration_distribution.png",
  "figure_s11_diagnostic_configuration_distribution.png",
  "Distribution of expected-residual configurations.",
  
  "Figure S12", "06", "06_observed_expected_residual_by_diagnostic_configuration.png",
  "figure_s12_observed_expected_residual_by_configuration.png",
  "Observed, expected and residual PHQ-4 by configuration.",
  
  "Figure S13", "06", "06_subjective_security_diagnostics_by_configuration.png",
  "figure_s13_subjective_security_diagnostics_by_configuration.png",
  "Detailed subjective-security diagnostics.",
  
  "Figure S14", "06", "06_subjective_security_diagnostics_by_configuration_compact.png",
  "figure_s14_subjective_security_diagnostics_compact.png",
  "Compact subjective-security diagnostics.",
  
  "Figure S15", "07", "07_threshold_sensitivity_configuration_distribution.png",
  "figure_s15_threshold_sensitivity_configuration_distribution.png",
  "Alternative residual thresholds.",
  
  "Figure S16", "07", "07_threshold_sensitivity_high_subjective_insecurity.png",
  "figure_s16_threshold_sensitivity_high_subjective_insecurity.png",
  "High subjective insecurity under alternative thresholds.",
  
  "Figure S17", "07", "07_no_meet_insecurity_by_configuration.png",
  "figure_s17_no_meet_insecurity_by_configuration.png",
  "Subjective-insecurity sensitivity excluding making ends meet.",
  
  "Figure S18", "07", "07_no_sex_residual_comparison.png",
  "figure_s18_no_sex_residual_comparison.png",
  "Residual comparison with and without sex.",
  
  "Figure S19", "07", "07_residual_model_sensitivity_coefficients.png",
  "figure_s19_residual_model_sensitivity_coefficients.png",
  "Residual model sensitivity coefficients."
) %>%
  dplyr::mutate(
    source_dir = input_dirs[paste0("script_", source_script)] %>% unlist(),
    source_path = purrr::map2_chr(
      source_dir,
      source_file,
      ~ find_existing_file(c(
        file.path(.x, "figures", .y),
        file.path(.x, .y)
      ))
    ),
    target_path = file.path(figures_supplement_dir, target_file)
  )

supplement_figure_copy_manifest <- purrr::pmap_dfr(
  list(
    supplement_figure_dictionary$source_path,
    supplement_figure_dictionary$target_path,
    supplement_figure_dictionary$paper_item
  ),
  function(source_path, target_path, paper_item) {
    copy_if_exists(
      source_path = source_path,
      target_path = target_path,
      required = FALSE
    ) %>%
      dplyr::mutate(paper_item = paper_item)
  }
) %>%
  dplyr::left_join(
    supplement_figure_dictionary %>%
      dplyr::select(
        paper_item,
        source_script,
        source_file,
        target_file,
        notes
      ),
    by = "paper_item"
  ) %>%
  dplyr::relocate(
    paper_item,
    source_script,
    source_file,
    target_file,
    copied,
    status,
    notes
  )


# ------------------------------------------------------------
# 12. Copy and curate tables
# ------------------------------------------------------------

table_dictionary <- tibble::tribble(
  ~paper_item, ~source_script, ~source_file, ~target_file, ~paper_role, ~notes,
  
  "Table 1",
  "02",
  "02_phq4_and_descriptive_gradients_tables.xlsx",
  "table_01_sample_phq4_descriptives.xlsx",
  "Main manuscript",
  "Analytic sample, PHQ-4 distribution, reliability and selected descriptive gradients.",
  
  "Table 2",
  "03",
  "03_objective_gradient_residual_tables.xlsx",
  "table_02_objective_gradient_residual_decomposition.xlsx",
  "Main manuscript",
  "Objective-position model fit, expected PHQ-4 and residual decomposition.",
  
  "Table 3",
  "04",
  "04_subjective_security_residual_tables.xlsx",
  "table_03_subjective_security_residual_phq4.xlsx",
  "Main manuscript",
  "Associations between subjective economic-security dimensions and residual PHQ-4.",
  
  "Table 4",
  "06",
  "06_configurational_diagnostics_tables.xlsx",
  "table_04_expected_residual_configurations.xlsx",
  "Main manuscript",
  "Expected-residual configurations and subjective-security diagnostics.",
  
  "Supplement S1",
  "02",
  "02_phq4_and_descriptive_gradients_tables.xlsx",
  "supplement_s01_phq4_descriptives.xlsx",
  "Supplement",
  "PHQ-4 distribution, reliability and descriptive gradients.",
  
  "Supplement S2",
  "03",
  "03_objective_gradient_residual_tables.xlsx",
  "supplement_s02_objective_gradient_model_diagnostics.xlsx",
  "Supplement",
  "Detailed objective-position gradients and residual diagnostics.",
  
  "Supplement S3",
  "04",
  "04_subjective_security_residual_tables.xlsx",
  "supplement_s03_subjective_security_models.xlsx",
  "Supplement",
  "Detailed residual models and logistic higher/lower-than-expected models.",
  
  "Supplement S4",
  "05",
  "05_within_gradient_heterogeneity_tables.xlsx",
  "supplement_s04_within_gradient_heterogeneity.xlsx",
  "Supplement",
  "Within-gradient residual heterogeneity by subjective insecurity.",
  
  "Supplement S5",
  "06",
  "06_configurational_diagnostics_tables.xlsx",
  "supplement_s05_expected_residual_configurational_diagnostics.xlsx",
  "Supplement",
  "Detailed diagnostic configuration tables.",
  
  "Supplement S6",
  "07",
  "07_robustness_sensitivity_tables.xlsx",
  "supplement_s06_robustness_sensitivity_checks.xlsx",
  "Supplement",
  "Residual-threshold, subjective-insecurity and objective-model sensitivity checks."
) %>%
  dplyr::mutate(
    source_dir = input_dirs[paste0("script_", source_script)] %>% unlist(),
    source_path = purrr::map2_chr(
      source_dir,
      source_file,
      ~ find_existing_file(c(
        file.path(.x, "tables", .y),
        file.path(.x, .y)
      ))
    ),
    target_dir = dplyr::case_when(
      paper_role == "Main manuscript" ~ tables_main_dir,
      paper_role == "Supplement" ~ tables_supplement_dir,
      TRUE ~ tables_supplement_dir
    ),
    target_path = file.path(target_dir, target_file)
  )

table_copy_manifest <- purrr::pmap_dfr(
  list(
    table_dictionary$source_path,
    table_dictionary$target_path,
    table_dictionary$paper_item
  ),
  function(source_path, target_path, paper_item) {
    copy_if_exists(
      source_path = source_path,
      target_path = target_path,
      required = stringr::str_detect(paper_item, "^Table [1-4]$")
    ) %>%
      dplyr::mutate(paper_item = paper_item)
  }
) %>%
  dplyr::left_join(
    table_dictionary %>%
      dplyr::select(
        paper_item,
        source_script,
        source_file,
        target_file,
        paper_role,
        notes
      ),
    by = "paper_item"
  ) %>%
  dplyr::relocate(
    paper_item,
    paper_role,
    source_script,
    source_file,
    target_file,
    copied,
    status,
    notes
  )


# ------------------------------------------------------------
# 13. Curated Excel tables from CSV components
# ------------------------------------------------------------

table_01_components <- list()

if (nrow(sample_02) > 0) {
  table_01_components$sample_summary <- sample_02
}

if (nrow(phq4_summary_02) > 0) {
  table_01_components$phq4_summary <- phq4_summary_02
}

if (nrow(phq4_reliability_02) > 0) {
  table_01_components$phq4_reliability <- phq4_reliability_02
}

if (nrow(selected_gradients_02) > 0) {
  table_01_components$selected_gradients <- selected_gradients_02
}

if (length(table_01_components) == 0) {
  table_01_components$note <- tibble::tibble(
    note = "No script 02 CSV components were available."
  )
}

openxlsx::write.xlsx(
  safe_excel_sheet_names(table_01_components),
  file = file.path(tables_main_dir, "table_01_curated_sample_phq4_descriptives.xlsx"),
  overwrite = TRUE
)

table_02_components <- list()

if (nrow(model_fit_03) > 0) {
  table_02_components$objective_fit <- model_fit_03
}

if (nrow(residual_diagnostics_03) > 0) {
  table_02_components$residual_diagnostics <- residual_diagnostics_03
}

if (length(table_02_components) == 0) {
  table_02_components$note <- tibble::tibble(
    note = "No script 03 CSV components were available."
  )
}

openxlsx::write.xlsx(
  safe_excel_sheet_names(table_02_components),
  file = file.path(tables_main_dir, "table_02_curated_objective_gradient_residual.xlsx"),
  overwrite = TRUE
)

table_03_components <- list()

if (nrow(residual_model_fit_04) > 0) {
  table_03_components$residual_fit <- residual_model_fit_04
}

if (nrow(residual_coefficients_04) > 0) {
  table_03_components$residual_coefficients <- residual_coefficients_04
}

if (length(table_03_components) == 0) {
  table_03_components$note <- tibble::tibble(
    note = "No script 04 CSV components were available."
  )
}

openxlsx::write.xlsx(
  safe_excel_sheet_names(table_03_components),
  file = file.path(tables_main_dir, "table_03_curated_subjective_security_residual.xlsx"),
  overwrite = TRUE
)

table_04_components <- list()

if (nrow(config_distribution_06) > 0) {
  table_04_components$config_distribution <- config_distribution_06
}

if (nrow(config_summary_06) > 0) {
  table_04_components$config_summary <- config_summary_06
}

if (length(table_04_components) == 0) {
  table_04_components$note <- tibble::tibble(
    note = "No script 06 CSV components were available."
  )
}

openxlsx::write.xlsx(
  safe_excel_sheet_names(table_04_components),
  file = file.path(tables_main_dir, "table_04_curated_expected_residual_configurations.xlsx"),
  overwrite = TRUE
)

robustness_components <- list()

if (nrow(threshold_sensitivity_07) > 0) {
  robustness_components$threshold_sensitivity <- threshold_sensitivity_07
}

if (nrow(no_meet_07) > 0) {
  robustness_components$no_meet_sensitivity <- no_meet_07
}

if (nrow(no_sex_07) > 0) {
  robustness_components$no_sex_sensitivity <- no_sex_07
}

if (length(robustness_components) == 0) {
  robustness_components$note <- tibble::tibble(
    note = "No script 07 CSV components were available."
  )
}

openxlsx::write.xlsx(
  safe_excel_sheet_names(robustness_components),
  file = file.path(tables_supplement_dir, "supplement_curated_robustness_tables.xlsx"),
  overwrite = TRUE
)


# ------------------------------------------------------------
# 14. Inventories and recommendations
# ------------------------------------------------------------

figure_generation_manifest_df <- dplyr::bind_rows(figure_generation_manifest)

main_figure_inventory <- tibble::tibble(
  paper_item = c(
    "Figure 1",
    "Figure 2",
    "Figure 3",
    "Figure 4"
  ),
  target_file_png = c(
    "figure_01_expected_and_residual_phq4.png",
    "figure_02_subjective_security_residual_phq4.png",
    "figure_03_residual_within_expected_distress.png",
    "figure_04_expected_position_by_residual_excess.png"
  ),
  target_file_pdf = c(
    "figure_01_expected_and_residual_phq4.pdf",
    "figure_02_subjective_security_residual_phq4.pdf",
    "figure_03_residual_within_expected_distress.pdf",
    "figure_04_expected_position_by_residual_excess.pdf"
  ),
  folder = "figures_main",
  internal_title_removed = TRUE,
  recommended_caption_role = c(
    "Shows the distribution of objective-position expected PHQ-4 and residual PHQ-4.",
    "Shows associations between subjective economic-security dimensions and residual PHQ-4.",
    "Shows residual PHQ-4 by subjective insecurity within low/intermediate and high expected-distress strata.",
    "Shows expected-position PHQ-4 by residual excess, with sample share, mean residual and high subjective insecurity."
  )
) %>%
  dplyr::mutate(
    png_path = file.path(figures_main_dir, target_file_png),
    pdf_path = file.path(figures_main_dir, target_file_pdf),
    png_exists = file.exists(png_path),
    pdf_exists = file.exists(pdf_path)
  )

supplement_figure_inventory <- supplement_figure_copy_manifest %>%
  dplyr::mutate(folder = "figures_supplement")

paper_ready_inventory <- dplyr::bind_rows(
  main_figure_inventory %>%
    dplyr::transmute(
      item_type = "Main figure",
      paper_item,
      target_file = target_file_png,
      folder,
      exists = png_exists,
      notes = recommended_caption_role
    ),
  supplement_figure_inventory %>%
    dplyr::transmute(
      item_type = "Supplement figure",
      paper_item,
      target_file,
      folder,
      exists = copied,
      notes
    ),
  table_copy_manifest %>%
    dplyr::transmute(
      item_type = dplyr::if_else(
        paper_role == "Main manuscript",
        "Main table",
        "Supplement table"
      ),
      paper_item,
      target_file,
      folder = dplyr::if_else(
        paper_role == "Main manuscript",
        "tables_main",
        "tables_supplement"
      ),
      exists = copied,
      notes
    )
)

recommended_main_outputs <- tibble::tribble(
  ~section, ~paper_item, ~recommended_output, ~folder, ~rationale,
  
  "Results 1. Objective-position gradient",
  "Table 2",
  "table_02_curated_objective_gradient_residual.xlsx",
  "tables_main",
  "Reports the objective-position model sequence and residual decomposition.",
  
  "Results 1. Objective-position gradient",
  "Figure 1",
  "figure_01_expected_and_residual_phq4.png",
  "figures_main",
  "Shows objective-position expected PHQ-4 and residual PHQ-4.",
  
  "Results 2. Subjective economic security and residual PHQ-4",
  "Table 3",
  "table_03_curated_subjective_security_residual.xlsx",
  "tables_main",
  "Reports associations between subjective-security dimensions and residual PHQ-4.",
  
  "Results 2. Subjective economic security and residual PHQ-4",
  "Figure 2",
  "figure_02_subjective_security_residual_phq4.png",
  "figures_main",
  "Main coefficient figure for residual PHQ-4 models.",
  
  "Results 3. Within-gradient heterogeneity",
  "Figure 3",
  "figure_03_residual_within_expected_distress.png",
  "figures_main",
  "Shows residual PHQ-4 by subjective insecurity within expected-distress strata.",
  
  "Results 4. Expected-residual configurations",
  "Table 4",
  "table_04_curated_expected_residual_configurations.xlsx",
  "tables_main",
  "Reports configuration distribution and subjective-security diagnostics.",
  
  "Results 4. Expected-residual configurations",
  "Figure 4",
  "figure_04_expected_position_by_residual_excess.png",
  "figures_main",
  "Visual summary of the expected-residual diagnostic configuration strategy."
)

recommended_supplement_structure <- tibble::tribble(
  ~supplement_section, ~content, ~recommended_files,
  
  "S1. PHQ-4 descriptives",
  "PHQ-4 distribution, severity, reliability and selected descriptive gradients.",
  "figure_s01, figure_s02, figure_s03, supplement_s01",
  
  "S2. Objective-position model diagnostics",
  "Objective model sequence, expected PHQ-4 and residual PHQ-4 diagnostics.",
  "figure_s04, figure_s05, figure_s06, figure_s07, supplement_s02",
  
  "S3. Subjective-security residual models",
  "Detailed coefficient tables and logistic higher/lower-than-expected models.",
  "figure_s08, figure_s09, supplement_s03",
  
  "S4. Within-gradient heterogeneity",
  "Detailed expected-strata heterogeneity by subjective insecurity.",
  "figure_s10, supplement_s04",
  
  "S5. Configurational diagnostics",
  "Distribution, observed/expected/residual by configuration, detailed subjective diagnostics.",
  "figure_s11, figure_s12, figure_s13, figure_s14, supplement_s05",
  
  "S6. Robustness checks",
  "Alternative residual thresholds, subjective-insecurity definition, no-sex model and coefficient sensitivity.",
  "figure_s15, figure_s16, figure_s17, figure_s18, figure_s19, supplement_s06"
)


# ------------------------------------------------------------
# 15. Export inventories and documentation
# ------------------------------------------------------------

readr::write_csv(
  input_folder_check,
  file.path(script_csv_dir, "08_input_folder_check.csv")
)

readr::write_csv(
  rds_source_check,
  file.path(script_csv_dir, "08_rds_source_check.csv")
)

readr::write_csv(
  csv_source_check,
  file.path(script_csv_dir, "08_csv_source_check.csv")
)

readr::write_csv(
  figure_generation_manifest_df,
  file.path(script_csv_dir, "08_generated_main_figures_manifest.csv")
)

readr::write_csv(
  main_figure_inventory,
  file.path(script_csv_dir, "08_main_figure_inventory.csv")
)

readr::write_csv(
  supplement_figure_dictionary,
  file.path(script_csv_dir, "08_supplement_figure_dictionary.csv")
)

readr::write_csv(
  supplement_figure_copy_manifest,
  file.path(script_csv_dir, "08_supplement_figure_copy_manifest.csv")
)

readr::write_csv(
  table_dictionary,
  file.path(script_csv_dir, "08_table_dictionary.csv")
)

readr::write_csv(
  table_copy_manifest,
  file.path(script_csv_dir, "08_table_copy_manifest.csv")
)

readr::write_csv(
  paper_ready_inventory,
  file.path(script_csv_dir, "08_paper_ready_inventory.csv")
)

readr::write_csv(
  recommended_main_outputs,
  file.path(script_csv_dir, "08_recommended_main_outputs.csv")
)

readr::write_csv(
  recommended_supplement_structure,
  file.path(script_csv_dir, "08_recommended_supplement_structure.csv")
)

output_excel <- file.path(
  script_output_dir,
  "08_paper_ready_inventory_and_recommendations.xlsx"
)

openxlsx::write.xlsx(
  safe_excel_sheet_names(
    list(
      input_folders = input_folder_check,
      rds_sources = rds_source_check,
      csv_sources = csv_source_check,
      generated_figures = figure_generation_manifest_df,
      main_figures = main_figure_inventory,
      supplement_figures = supplement_figure_copy_manifest,
      table_dictionary = table_dictionary,
      table_copy_manifest = table_copy_manifest,
      paper_ready_inventory = paper_ready_inventory,
      recommended_main = recommended_main_outputs,
      recommended_supp = recommended_supplement_structure
    )
  ),
  file = output_excel,
  overwrite = TRUE
)

metadata_08 <- list(
  script_name = script_name,
  output_dir = script_output_dir,
  input_dirs = input_dirs,
  rds_sources = rds_sources,
  csv_sources = csv_sources,
  main_figure_inventory = main_figure_inventory,
  supplement_figure_dictionary = supplement_figure_dictionary,
  table_dictionary = table_dictionary,
  recommended_main_outputs = recommended_main_outputs,
  recommended_supplement_structure = recommended_supplement_structure,
  note = "Script 08 regenerates title-free paper-ready main figures and curates supplementary outputs. It does not introduce new substantive analyses."
)

metadata_08_path <- file.path(
  script_rds_dir,
  "08_paper_ready_metadata.rds"
)

saveRDS(
  metadata_08,
  metadata_08_path
)


# ------------------------------------------------------------
# 16. Plain-text reporting notes
# ------------------------------------------------------------

reporting_notes_path <- file.path(
  script_output_dir,
  "08_paper_ready_reporting_notes.md"
)

reporting_notes <- c(
  "# Paper-ready outputs: reporting notes",
  "",
  "This folder gathers curated outputs from scripts 02-07.",
  "",
  "## Main manuscript figures",
  "",
  "The main figures generated by this script do not include internal titles or subtitles.",
  "Titles should be written in the manuscript text or figure captions.",
  "",
  "1. Figure 1: Objective-position expected and residual PHQ-4.",
  "2. Figure 2: Subjective economic security and residual PHQ-4.",
  "3. Figure 3: Residual PHQ-4 by subjective insecurity within low/intermediate and high expected-distress strata.",
  "4. Figure 4: Objective-position expected PHQ-4 by residual excess.",
  "",
  "## Recommended main manuscript tables",
  "",
  "1. Table 1: Sample characteristics and PHQ-4 descriptives.",
  "2. Table 2: Objective-position model and residual decomposition.",
  "3. Table 3: Subjective economic security and residual PHQ-4.",
  "4. Table 4: Expected-residual diagnostic configurations.",
  "",
  "## Recommended supplement sections",
  "",
  "S1. PHQ-4 descriptives.",
  "S2. Objective-position model diagnostics.",
  "S3. Detailed subjective-security models.",
  "S4. Within-gradient heterogeneity diagnostics.",
  "S5. Expected-residual configurational diagnostics.",
  "S6. Robustness checks.",
  "",
  "## Conceptual logic",
  "",
  "The article should be presented as a study of within-gradient heterogeneity in psychological distress.",
  "The objective-position model estimates expected PHQ-4.",
  "Residual PHQ-4 identifies deviations from that expected level.",
  "Subjective economic-security appraisals are then used to characterize those deviations.",
  "",
  "## Preferred terminology",
  "",
  "- objective-position expected PHQ-4",
  "- residual PHQ-4",
  "- within-gradient heterogeneity",
  "- expected-residual configurations",
  "- configurational diagnostics",
  "- higher-than-expected distress",
  "- not higher-than-expected distress",
  "",
  "## Terms to avoid or use carefully",
  "",
  "- Avoid saying that the paper is based on residuals.",
  "- Avoid presenting residuals as unexplained psychological essence.",
  "- Avoid causal language such as subjective insecurity causes residual distress.",
  "- Avoid treating expected-residual configurations as natural types.",
  "",
  "## Figure formatting decisions",
  "",
  "- Main figures are title-free.",
  "- Main figures are exported as PNG and PDF.",
  "- PNG files are exported at 600 dpi.",
  "- Figure sizes are set for manuscript readability.",
  "- Figure captions should provide titles and explanatory notes in the manuscript."
)

writeLines(
  reporting_notes,
  con = reporting_notes_path
)


# ------------------------------------------------------------
# 17. Session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(script_logs_dir, "08_session_info.txt")
)


# ------------------------------------------------------------
# 18. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("08_paper_ready_tables_figures.R completed")
message("============================================================")
message("Output folder: ", script_output_dir)
message("Main figures folder: ", figures_main_dir)
message("Supplement figures folder: ", figures_supplement_dir)
message("Main tables folder: ", tables_main_dir)
message("Supplement tables folder: ", tables_supplement_dir)
message("Inventory Excel: ", output_excel)
message("Reporting notes: ", reporting_notes_path)
message("Metadata: ", metadata_08_path)
message("============================================================\n")

message("Generated main figures:")
print(
  main_figure_inventory %>%
    dplyr::select(paper_item, target_file_png, png_exists, pdf_exists)
)

message("Supplement figure copy summary:")
print(
  supplement_figure_copy_manifest %>%
    dplyr::count(copied, status)
)

message("Table copy summary:")
print(
  table_copy_manifest %>%
    dplyr::count(paper_role, copied, status)
)
