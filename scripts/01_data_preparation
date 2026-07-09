# ============================================================
# 01_data_preparation.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Build analytic variables for the residual-gradient strategy
# Author: Gonzalo Torres-Rosales
# ============================================================

# ------------------------------------------------------------
# 01. Setup
# ------------------------------------------------------------

source("scripts/00_setup.R")

options(survey.lonely.psu = "adjust")

script_name <- "01_data_preparation"

script_output_dir     <- file.path(output_dir, script_name)
script_tables_dir     <- file.path(script_output_dir, "tables")
script_figures_dir    <- file.path(script_output_dir, "figures")
script_csv_dir        <- file.path(script_output_dir, "csv")
script_rds_dir        <- file.path(script_output_dir, "rds")
script_logs_dir       <- file.path(script_output_dir, "logs")

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
# 02. Load raw data
# ------------------------------------------------------------

if (!file.exists(raw_data_file)) {
  stop(
    paste0(
      "Raw data file not found: ", raw_data_file, "\n",
      "Download the EBS 2023 data from the official source and place it in data_raw/."
    )
  )
}

objects_before <- ls()
load(raw_data_file)
objects_after <- setdiff(ls(), objects_before)

loaded_objects <- mget(objects_after)

data_candidates <- loaded_objects %>%
  purrr::keep(~ is.data.frame(.x) || tibble::is_tibble(.x))

if (length(data_candidates) == 0) {
  stop("No data.frame or tibble object was found in the RData file.")
}

data_name <- names(data_candidates)[
  which.max(purrr::map_int(data_candidates, nrow))
]

raw_data <- data_candidates[[data_name]] %>%
  tibble::as_tibble()

n_raw <- nrow(raw_data)

message("Loaded object: ", data_name)
message("Rows: ", nrow(raw_data), " | Columns: ", ncol(raw_data))


# ------------------------------------------------------------
# 03. Local helper functions
# ------------------------------------------------------------

to_numeric <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  suppressWarnings(as.numeric(x))
}

clean_dk_nr <- function(x) {
  x <- to_numeric(x)
  ifelse(x %in% c(-99, -88), NA_real_, x)
}

num <- function(var_name) {
  if (var_name %in% names(raw_data)) {
    clean_dk_nr(raw_data[[var_name]])
  } else {
    rep(NA_real_, n_raw)
  }
}

z_score <- function(x) {
  x <- as.numeric(x)
  
  if (
    all(is.na(x)) ||
    is.na(stats::sd(x, na.rm = TRUE)) ||
    stats::sd(x, na.rm = TRUE) == 0
  ) {
    return(rep(NA_real_, length(x)))
  }
  
  as.numeric(scale(x))
}

weighted_freq_local <- function(data, var_name, weight_var = "weight") {
  if (!var_name %in% names(data)) {
    return(
      tibble::tibble(
        variable = var_name,
        value = NA_character_,
        unweighted_n = NA_integer_,
        weighted_n = NA_real_,
        weighted_pct = NA_real_,
        missing_n = NA_integer_,
        missing_pct = NA_real_
      )
    )
  }
  
  if (!weight_var %in% names(data)) {
    stop("Weight variable not found: ", weight_var)
  }
  
  x <- data[[var_name]]
  w <- data[[weight_var]]
  
  non_missing_data <- data %>%
    dplyr::filter(!is.na(.data[[var_name]]), !is.na(.data[[weight_var]])) %>%
    dplyr::mutate(.value = as.character(.data[[var_name]]))
  
  if (nrow(non_missing_data) == 0) {
    return(
      tibble::tibble(
        variable = var_name,
        value = NA_character_,
        unweighted_n = 0L,
        weighted_n = 0,
        weighted_pct = NA_real_,
        missing_n = sum(is.na(x)),
        missing_pct = mean(is.na(x)) * 100
      )
    )
  }
  
  total_w <- sum(non_missing_data[[weight_var]], na.rm = TRUE)
  
  non_missing_data %>%
    dplyr::group_by(.value) %>%
    dplyr::summarise(
      unweighted_n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      variable = var_name,
      value = .value,
      weighted_pct = 100 * weighted_n / total_w,
      missing_n = sum(is.na(x)),
      missing_pct = mean(is.na(x)) * 100
    ) %>%
    dplyr::select(
      variable,
      value,
      unweighted_n,
      weighted_n,
      weighted_pct,
      missing_n,
      missing_pct
    ) %>%
    dplyr::arrange(variable, dplyr::desc(weighted_pct))
}

numeric_summary_local <- function(data, var_name, weight_var = "weight") {
  if (!var_name %in% names(data)) {
    return(
      tibble::tibble(
        variable = var_name,
        n_valid = NA_integer_,
        n_missing = NA_integer_,
        mean = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        p10 = NA_real_,
        p25 = NA_real_,
        median = NA_real_,
        p75 = NA_real_,
        p90 = NA_real_,
        max = NA_real_,
        weighted_mean = NA_real_,
        weighted_sd = NA_real_
      )
    )
  }
  
  x <- suppressWarnings(as.numeric(data[[var_name]]))
  
  if (all(is.na(x))) {
    return(
      tibble::tibble(
        variable = var_name,
        n_valid = 0L,
        n_missing = length(x),
        mean = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        p10 = NA_real_,
        p25 = NA_real_,
        median = NA_real_,
        p75 = NA_real_,
        p90 = NA_real_,
        max = NA_real_,
        weighted_mean = NA_real_,
        weighted_sd = NA_real_
      )
    )
  }
  
  w <- if (weight_var %in% names(data)) {
    suppressWarnings(as.numeric(data[[weight_var]]))
  } else {
    rep(1, length(x))
  }
  
  tibble::tibble(
    variable = var_name,
    n_valid = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = stats::sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    p10 = as.numeric(stats::quantile(x, 0.10, na.rm = TRUE, names = FALSE)),
    p25 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
    median = stats::median(x, na.rm = TRUE),
    p75 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
    p90 = as.numeric(stats::quantile(x, 0.90, na.rm = TRUE, names = FALSE)),
    max = max(x, na.rm = TRUE),
    weighted_mean = weighted_mean_approx(x, w),
    weighted_sd = weighted_sd_approx(x, w)
  ) %>%
    dplyr::mutate(
      dplyr::across(
        c(mean, sd, min, p10, p25, median, p75, p90, max, weighted_mean, weighted_sd),
        ~ ifelse(is.infinite(.x), NA_real_, .x)
      )
    )
}

alpha_from_items <- function(data, item_vars) {
  item_data <- data %>%
    dplyr::select(dplyr::all_of(item_vars))
  
  complete_item_data <- item_data %>%
    tidyr::drop_na()
  
  k <- length(item_vars)
  
  if (nrow(complete_item_data) == 0 || k < 2) {
    return(
      tibble::tibble(
        scale = "PHQ-4",
        n_complete = nrow(complete_item_data),
        items = k,
        cronbach_alpha = NA_real_
      )
    )
  }
  
  item_vars_sum <- sum(
    apply(complete_item_data, 2, stats::var, na.rm = TRUE)
  )
  
  total_var <- stats::var(
    rowSums(complete_item_data),
    na.rm = TRUE
  )
  
  alpha <- (k / (k - 1)) * (1 - item_vars_sum / total_var)
  
  tibble::tibble(
    scale = "PHQ-4",
    n_complete = nrow(complete_item_data),
    items = k,
    cronbach_alpha = alpha
  )
}


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

main_theme <- function(base_size = 14) {
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
# 05. Extract raw variables
# ------------------------------------------------------------

# Survey design
fexp    <- num("fexp")
estrato <- num("estrato_ebs")
varunit <- num("varunit")

# Objective socioeconomic position
dau     <- num("dau_casen")
educ    <- num("educ_recat_casen")
ten_viv <- num("ten_viv_casen")
s13     <- num("s13_casen")
region_raw <- num("region_casen")

# Age and labor position
tramo_age <- num("tramoebs2")
activ     <- num("activ_casen")
cat_ocup  <- num("cat_ocup_casen")
contrato  <- num("contrato_casen")
l10_a     <- num("l10_a")

# Household and demographic position
tipohogar <- num("tipohogar_casen")

sex_raw <- num("sexo")
if (all(is.na(sex_raw))) {
  sex_raw <- num("sexo_casen")
}

# Housing deprivation
hh_hacina  <- num("hh_d_hacina_casen")
hh_estado  <- num("hh_d_estado_casen")
hh_servbas <- num("hh_d_servbas_casen")

# Territorial exposure
v36a <- num("v36a_casen")
v36c <- num("v36c_casen")
v36d <- num("v36d_casen")
v36e <- num("v36e_casen")

# PHQ-4 outcome
ss7_a <- num("ss7_a")
ss7_b <- num("ss7_b")
ss7_c <- num("ss7_c")
ss7_d <- num("ss7_d")
phq4_existing <- num("phq4")

# Subjective economic security
yy2    <- num("yy2")
ss6    <- num("ss6")
rr2_e  <- num("rr2_e")
oo7_a1 <- num("oo7_a1")
yy5    <- num("yy5")
yy5_a  <- num("yy5_a")


# ------------------------------------------------------------
# 06. Build intermediate components
# ------------------------------------------------------------

# Housing deprivation count
housing_dep_count_raw <- rowSums(
  cbind(hh_hacina, hh_estado, hh_servbas),
  na.rm = TRUE
)

housing_dep_valid <- rowSums(
  !is.na(cbind(hh_hacina, hh_estado, hh_servbas))
)

housing_dep_count <- ifelse(
  housing_dep_valid == 0,
  NA_real_,
  housing_dep_count_raw
)

# Territorial insecurity count
territory_damage <- dplyr::case_when(
  v36a == 1 ~ 0,
  v36a %in% c(2, 3, 4) ~ 1,
  TRUE ~ NA_real_
)

territory_drugs <- dplyr::case_when(
  v36c == 1 ~ 0,
  v36c %in% c(2, 3, 4) ~ 1,
  TRUE ~ NA_real_
)

territory_fights <- dplyr::case_when(
  v36d == 1 ~ 0,
  v36d %in% c(2, 3, 4) ~ 1,
  TRUE ~ NA_real_
)

territory_shootings <- dplyr::case_when(
  v36e == 1 ~ 0,
  v36e %in% c(2, 3, 4) ~ 1,
  TRUE ~ NA_real_
)

territory_count_raw <- rowSums(
  cbind(territory_damage, territory_drugs, territory_fights, territory_shootings),
  na.rm = TRUE
)

territory_valid <- rowSums(
  !is.na(cbind(territory_damage, territory_drugs, territory_fights, territory_shootings))
)

territory_count <- ifelse(
  territory_valid == 0,
  NA_real_,
  territory_count_raw
)

# PHQ-4 items recoded from 1-4 to 0-3
phq4_item_a <- dplyr::case_when(ss7_a %in% 1:4 ~ ss7_a - 1, TRUE ~ NA_real_)
phq4_item_b <- dplyr::case_when(ss7_b %in% 1:4 ~ ss7_b - 1, TRUE ~ NA_real_)
phq4_item_c <- dplyr::case_when(ss7_c %in% 1:4 ~ ss7_c - 1, TRUE ~ NA_real_)
phq4_item_d <- dplyr::case_when(ss7_d %in% 1:4 ~ ss7_d - 1, TRUE ~ NA_real_)

phq4_valid_items <- rowSums(
  !is.na(cbind(phq4_item_a, phq4_item_b, phq4_item_c, phq4_item_d))
)

phq4_score_raw <- rowSums(
  cbind(phq4_item_a, phq4_item_b, phq4_item_c, phq4_item_d),
  na.rm = TRUE
)

phq4_score <- ifelse(
  phq4_valid_items == 4,
  phq4_score_raw,
  NA_real_
)

# Subjective economic-security binary indicators
health_unprotected_binary <- dplyr::case_when(
  ss6 %in% c(1, 2) ~ 1,
  ss6 %in% c(3, 4, 5) ~ 0,
  TRUE ~ NA_real_
)

no_support_binary <- dplyr::case_when(
  rr2_e %in% c(1, 2, 3) ~ 0,
  rr2_e == 4 ~ 1,
  TRUE ~ NA_real_
)

hardship_binary <- dplyr::case_when(
  yy2 %in% c(1, 2) ~ 1,
  yy2 %in% c(3, 4, 5) ~ 0,
  TRUE ~ NA_real_
)

debt_problem_binary <- dplyr::case_when(
  yy5 == 2 ~ 0,
  yy5 == 1 & yy5_a == 1 ~ 0,
  yy5 == 1 & yy5_a %in% c(2, 3) ~ 1,
  TRUE ~ NA_real_
)

job_loss_high_binary <- dplyr::case_when(
  activ == 1 & oo7_a1 %in% c(4, 5) ~ 1,
  activ == 1 & oo7_a1 %in% c(1, 2, 3) ~ 0,
  TRUE ~ NA_real_
)

job_loss_high_or_uncertain_binary <- dplyr::case_when(
  activ == 1 & oo7_a1 %in% c(3, 4, 5) ~ 1,
  activ == 1 & oo7_a1 %in% c(1, 2) ~ 0,
  activ %in% c(2, 3) ~ 0,
  TRUE ~ NA_real_
)

# Summary counts
subjective_insecurity_valid <- rowSums(
  !is.na(
    cbind(
      health_unprotected_binary,
      no_support_binary,
      hardship_binary,
      debt_problem_binary,
      job_loss_high_or_uncertain_binary
    )
  )
)

subjective_insecurity_count_raw <- rowSums(
  cbind(
    health_unprotected_binary,
    no_support_binary,
    hardship_binary,
    debt_problem_binary,
    job_loss_high_or_uncertain_binary
  ),
  na.rm = TRUE
)

subjective_insecurity_count <- ifelse(
  subjective_insecurity_valid >= 3,
  subjective_insecurity_count_raw,
  NA_real_
)

prospective_insecurity_valid <- rowSums(
  !is.na(
    cbind(
      health_unprotected_binary,
      no_support_binary,
      job_loss_high_or_uncertain_binary
    )
  )
)

prospective_insecurity_count_raw <- rowSums(
  cbind(
    health_unprotected_binary,
    no_support_binary,
    job_loss_high_or_uncertain_binary
  ),
  na.rm = TRUE
)

prospective_insecurity_count <- ifelse(
  prospective_insecurity_valid >= 2,
  prospective_insecurity_count_raw,
  NA_real_
)

current_financial_strain_valid <- rowSums(
  !is.na(
    cbind(
      hardship_binary,
      debt_problem_binary
    )
  )
)

current_financial_strain_count_raw <- rowSums(
  cbind(
    hardship_binary,
    debt_problem_binary
  ),
  na.rm = TRUE
)

current_financial_strain_count <- ifelse(
  current_financial_strain_valid >= 1,
  current_financial_strain_count_raw,
  NA_real_
)


# ------------------------------------------------------------
# 07. Build analytic dataset
# ------------------------------------------------------------

analytic <- raw_data %>%
  dplyr::mutate(
    # ID and survey design
    person_id_analysis = dplyr::row_number(),
    row_id_rebuild = person_id_analysis,
    
    weight = fexp,
    strata = estrato,
    psu = varunit,
    
    # Income position
    income_decile = dplyr::case_when(
      dau %in% 1:10 ~ dau,
      TRUE ~ NA_real_
    ),
    
    income_decile_f = factor(
      income_decile,
      levels = 1:10,
      labels = paste0("Decile ", 1:10)
    ),
    
    income_group_3cat = dplyr::case_when(
      income_decile %in% 1:3 ~ 1,
      income_decile %in% 4:7 ~ 2,
      income_decile %in% 8:10 ~ 3,
      TRUE ~ NA_real_
    ),
    
    income_group_3cat_f = factor(
      income_group_3cat,
      levels = c(1, 2, 3),
      labels = c(
        "Low income (deciles 1-3)",
        "Middle income (deciles 4-7)",
        "High income (deciles 8-10)"
      )
    ),
    
    # Education
    education_3cat = dplyr::case_when(
      educ == 1 ~ 1,
      educ == 2 ~ 2,
      educ == 3 ~ 3,
      TRUE ~ NA_real_
    ),
    
    education_3cat_f = factor(
      education_3cat,
      levels = c(1, 2, 3),
      labels = c(
        "Primary education or less",
        "Secondary education",
        "Higher education"
      )
    ),
    
    # Housing tenure
    homeownership = dplyr::case_when(
      ten_viv == 1 ~ 1,
      ten_viv %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),
    
    housing_tenure_3cat = dplyr::case_when(
      ten_viv == 1 ~ 1,
      ten_viv == 2 ~ 2,
      ten_viv %in% c(3, 4) ~ 3,
      TRUE ~ NA_real_
    ),
    
    housing_tenure_3cat_f = factor(
      housing_tenure_3cat,
      levels = c(1, 2, 3),
      labels = c(
        "Owner",
        "Renter",
        "Ceded, irregular or other tenure"
      )
    ),
    
    # Health system
    health_system_3cat = dplyr::case_when(
      s13 == 1 ~ 1,
      s13 == 2 ~ 2,
      s13 %in% c(3, 4, 5, 6, 7, -77) ~ 3,
      TRUE ~ NA_real_
    ),
    
    health_system_3cat_f = factor(
      health_system_3cat,
      levels = c(1, 2, 3),
      labels = c(
        "FONASA",
        "ISAPRE",
        "Other or no health system"
      )
    ),
    
    # Age and labor
    age_group_5cat = dplyr::case_when(
      tramo_age %in% 1:5 ~ tramo_age,
      TRUE ~ NA_real_
    ),
    
    age_group_5cat_f = factor(
      age_group_5cat,
      levels = 1:5,
      labels = c("18-29", "30-44", "45-59", "60-79", "80+")
    ),
    
    activity_status = dplyr::case_when(
      activ %in% 1:3 ~ activ,
      TRUE ~ NA_real_
    ),
    
    activity_status_f = factor(
      activity_status,
      levels = c(1, 2, 3),
      labels = c("Employed", "Unemployed", "Inactive")
    ),
    
    student_status = dplyr::case_when(
      l10_a == 11 ~ 1,
      !is.na(activ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    retired_status = dplyr::case_when(
      l10_a == 12 ~ 1,
      !is.na(activ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    life_labor_stage = dplyr::case_when(
      age_group_5cat == 1 & student_status == 1 ~ 1,
      age_group_5cat == 1 & student_status != 1 ~ 2,
      age_group_5cat %in% c(2, 3) & activ == 1 ~ 3,
      age_group_5cat %in% c(2, 3) & activ == 2 ~ 4,
      age_group_5cat %in% c(2, 3) & activ == 3 ~ 5,
      age_group_5cat %in% c(4, 5) & activ == 1 ~ 6,
      age_group_5cat %in% c(4, 5) & activ != 1 ~ 7,
      TRUE ~ NA_real_
    ),
    
    life_labor_stage_f = factor(
      life_labor_stage,
      levels = 1:7,
      labels = c(
        "Young student/inactive student",
        "Young adult, not student-coded",
        "Core working age, employed",
        "Core working age, unemployed",
        "Core working age, inactive",
        "Older adult, employed",
        "Older adult, not employed"
      )
    ),
    
    # Housing and territorial conditions
    housing_deprivation_count = housing_dep_count,
    
    housing_deprivation_3cat = dplyr::case_when(
      housing_deprivation_count == 0 ~ 0,
      housing_deprivation_count == 1 ~ 1,
      housing_deprivation_count >= 2 ~ 2,
      TRUE ~ NA_real_
    ),
    
    housing_deprivation_3cat_f = factor(
      housing_deprivation_3cat,
      levels = c(0, 1, 2),
      labels = c(
        "No housing deprivation",
        "One housing deprivation",
        "Two or more housing deprivations"
      )
    ),
    
    territory_insecurity_count = territory_count,
    
    territory_insecurity_3cat = dplyr::case_when(
      territory_insecurity_count == 0 ~ 0,
      territory_insecurity_count %in% c(1, 2) ~ 1,
      territory_insecurity_count >= 3 ~ 2,
      TRUE ~ NA_real_
    ),
    
    territory_insecurity_3cat_f = factor(
      territory_insecurity_3cat,
      levels = c(0, 1, 2),
      labels = c(
        "No reported territorial insecurity",
        "Moderate territorial insecurity",
        "High territorial insecurity"
      )
    ),
    
    # Region and macrozone
    region = dplyr::case_when(
      region_raw %in% 1:16 ~ region_raw,
      TRUE ~ NA_real_
    ),
    
    macrozone = dplyr::case_when(
      region %in% c(1, 2, 3, 4, 15, 16) ~ 1,
      region %in% c(5, 13, 6) ~ 2,
      region %in% c(7, 8, 9, 14, 10) ~ 3,
      region %in% c(11, 12) ~ 4,
      TRUE ~ NA_real_
    ),
    
    macrozone_f = factor(
      macrozone,
      levels = c(1, 2, 3, 4),
      labels = c(
        "North",
        "Central and Metropolitan",
        "South",
        "Austral"
      )
    ),
    
    # PHQ-4
    phq4_item_a = phq4_item_a,
    phq4_item_b = phq4_item_b,
    phq4_item_c = phq4_item_c,
    phq4_item_d = phq4_item_d,
    phq4_valid_items = phq4_valid_items,
    phq4_score = phq4_score,
    
    phq4_existing_clean = dplyr::case_when(
      phq4_existing >= 0 & phq4_existing <= 12 ~ phq4_existing,
      TRUE ~ NA_real_
    ),
    
    phq4_severity = dplyr::case_when(
      phq4_score %in% 0:2 ~ 1,
      phq4_score %in% 3:5 ~ 2,
      phq4_score %in% 6:8 ~ 3,
      phq4_score %in% 9:12 ~ 4,
      TRUE ~ NA_real_
    ),
    
    phq4_severity_f = factor(
      phq4_severity,
      levels = 1:4,
      labels = c("Normal", "Mild", "Moderate", "Severe")
    ),
    
    # Subjective economic security: making ends meet
    making_ends_meet_3cat = dplyr::case_when(
      yy2 %in% c(1, 2) ~ 1,
      yy2 == 3 ~ 2,
      yy2 %in% c(4, 5) ~ 3,
      TRUE ~ NA_real_
    ),
    
    making_ends_meet_3cat_f = factor(
      making_ends_meet_3cat,
      levels = c(1, 2, 3),
      labels = c(
        "Difficulty making ends meet",
        "Neither difficulty nor ease",
        "Ease making ends meet"
      )
    ),
    
    difficulty_making_ends_meet = hardship_binary,
    
    # Subjective economic security: health emergency protection
    health_financial_protection_3cat = dplyr::case_when(
      ss6 %in% c(1, 2) ~ 1,
      ss6 == 3 ~ 2,
      ss6 %in% c(4, 5) ~ 3,
      TRUE ~ NA_real_
    ),
    
    health_financial_protection_3cat_f = factor(
      health_financial_protection_3cat,
      levels = c(1, 2, 3),
      labels = c(
        "Financially unprotected in health emergency",
        "Neither protected nor unprotected in health emergency",
        "Financially protected in health emergency"
      )
    ),
    
    health_financially_unprotected = health_unprotected_binary,
    
    # Subjective economic security: emergency support
    emergency_money_support = dplyr::case_when(
      rr2_e %in% c(1, 2, 3) ~ 1,
      rr2_e == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    
    no_emergency_money_support = no_support_binary,
    
    no_emergency_money_support_f = factor(
      no_emergency_money_support,
      levels = c(0, 1),
      labels = c(
        "Has emergency money support",
        "No emergency money support"
      )
    ),
    
    # Subjective economic security: job-loss risk
    perceived_job_loss_risk_3cat = dplyr::case_when(
      activ == 1 & oo7_a1 %in% c(1, 2) ~ 1,
      activ == 1 & oo7_a1 == 3 ~ 2,
      activ == 1 & oo7_a1 %in% c(4, 5) ~ 3,
      TRUE ~ NA_real_
    ),
    
    perceived_job_loss_risk_3cat_f = factor(
      perceived_job_loss_risk_3cat,
      levels = c(1, 2, 3),
      labels = c(
        "Low perceived job-loss risk",
        "Uncertain perceived job-loss risk",
        "High perceived job-loss risk"
      )
    ),
    
    employment_risk_position = dplyr::case_when(
      activ == 1 & oo7_a1 %in% c(1, 2) ~ 1,
      activ == 1 & oo7_a1 == 3 ~ 2,
      activ == 1 & oo7_a1 %in% c(4, 5) ~ 3,
      activ %in% c(2, 3) ~ 4,
      TRUE ~ NA_real_
    ),
    
    employment_risk_position_f = factor(
      employment_risk_position,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Employed, low perceived job-loss risk",
        "Employed, uncertain perceived job-loss risk",
        "Employed, high perceived job-loss risk",
        "Outside current employment context"
      )
    ),
    
    high_or_uncertain_job_loss_risk = job_loss_high_or_uncertain_binary,
    
    # Subjective economic security: debt
    has_debt = dplyr::case_when(
      yy5 == 1 ~ 1,
      yy5 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    debt_status = dplyr::case_when(
      has_debt == 0 ~ 0,
      has_debt == 1 & yy5_a == 1 ~ 1,
      has_debt == 1 & yy5_a == 2 ~ 2,
      has_debt == 1 & yy5_a == 3 ~ 3,
      TRUE ~ NA_real_
    ),
    
    debt_status_f = factor(
      debt_status,
      levels = c(0, 1, 2, 3),
      labels = c(
        "No debt",
        "Debt, all payments on time",
        "Debt, some payment problems",
        "Debt, no payments on time"
      )
    ),
    
    debt_payment_problem = debt_problem_binary,
    
    # Summary subjective insecurity
    subjective_insecurity_count = subjective_insecurity_count,
    
    subjective_insecurity_level = dplyr::case_when(
      subjective_insecurity_count <= 1 ~ 1,
      subjective_insecurity_count == 2 ~ 2,
      subjective_insecurity_count >= 3 ~ 3,
      TRUE ~ NA_real_
    ),
    
    subjective_insecurity_level_f = factor(
      subjective_insecurity_level,
      levels = c(1, 2, 3),
      labels = c(
        "Low subjective insecurity",
        "Moderate subjective insecurity",
        "High subjective insecurity"
      )
    ),
    
    prospective_insecurity_count = prospective_insecurity_count,
    
    prospective_insecurity_level = dplyr::case_when(
      prospective_insecurity_count == 0 ~ 0,
      prospective_insecurity_count == 1 ~ 1,
      prospective_insecurity_count >= 2 ~ 2,
      TRUE ~ NA_real_
    ),
    
    prospective_insecurity_level_f = factor(
      prospective_insecurity_level,
      levels = c(0, 1, 2),
      labels = c(
        "No prospective insecurity",
        "One prospective insecurity",
        "Multiple prospective insecurities"
      )
    ),
    
    current_financial_strain_count = current_financial_strain_count,
    
    current_financial_strain_level = dplyr::case_when(
      current_financial_strain_count == 0 ~ 0,
      current_financial_strain_count == 1 ~ 1,
      current_financial_strain_count >= 2 ~ 2,
      TRUE ~ NA_real_
    ),
    
    current_financial_strain_level_f = factor(
      current_financial_strain_level,
      levels = c(0, 1, 2),
      labels = c(
        "No current financial strain",
        "One current financial strain",
        "Multiple current financial strains"
      )
    ),
    
    # Sex
    sex = dplyr::case_when(
      sex_raw %in% c(1, 2) ~ sex_raw,
      TRUE ~ NA_real_
    ),
    
    sex_f = factor(
      sex,
      levels = c(1, 2),
      labels = c("Men", "Women")
    ),
    
    # Household type, retained for diagnostics and possible extensions
    household_type = dplyr::case_when(
      tipohogar %in% 1:6 ~ tipohogar,
      TRUE ~ NA_real_
    ),
    
    household_type_f = factor(
      household_type,
      levels = 1:6,
      labels = c(
        "One-person household",
        "Nuclear single-parent household",
        "Nuclear two-parent household",
        "Extended household",
        "Composite household",
        "Household without nuclear family"
      )
    )
  )


# ------------------------------------------------------------
# 08. Variable families for downstream scripts
# ------------------------------------------------------------

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

objective_position_core_vars <- c(
  "income_group_3cat_f",
  "education_3cat_f"
)

objective_position_life_labor_vars <- c(
  objective_position_core_vars,
  "life_labor_stage_f"
)

objective_position_expanded_vars <- objective_position_vars

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
  "subjective_insecurity_count",
  "subjective_insecurity_level_f",
  "prospective_insecurity_count",
  "prospective_insecurity_level_f",
  "current_financial_strain_count",
  "current_financial_strain_level_f"
)

primary_outcome_vars <- c(
  "phq4_score",
  "phq4_severity_f"
)

design_vars <- c(
  "weight",
  "strata",
  "psu"
)

configuration_candidate_vars <- c(
  "subjective_insecurity_level_f",
  "prospective_insecurity_level_f",
  "current_financial_strain_level_f",
  "health_financially_unprotected",
  "no_emergency_money_support",
  "high_or_uncertain_job_loss_risk",
  "debt_payment_problem",
  "difficulty_making_ends_meet"
)

variable_groups <- list(
  design_vars = design_vars,
  primary_outcome_vars = primary_outcome_vars,
  objective_position_core_vars = objective_position_core_vars,
  objective_position_life_labor_vars = objective_position_life_labor_vars,
  objective_position_expanded_vars = objective_position_expanded_vars,
  prospective_protection_threat_vars = prospective_protection_threat_vars,
  current_financial_strain_vars = current_financial_strain_vars,
  subjective_security_specific_vars = subjective_security_specific_vars,
  subjective_security_summary_vars = subjective_security_summary_vars,
  configuration_candidate_vars = configuration_candidate_vars
)


# ------------------------------------------------------------
# 09. Variable maps and diagnostics
# ------------------------------------------------------------

raw_variables_used <- c(
  "fexp", "estrato_ebs", "varunit",
  "dau_casen", "educ_recat_casen", "ten_viv_casen", "s13_casen",
  "tramoebs2", "activ_casen", "cat_ocup_casen", "contrato_casen", "l10_a",
  "hh_d_hacina_casen", "hh_d_estado_casen", "hh_d_servbas_casen",
  "v36a_casen", "v36c_casen", "v36d_casen", "v36e_casen",
  "ss7_a", "ss7_b", "ss7_c", "ss7_d", "phq4",
  "yy2", "ss6", "rr2_e", "oo7_a1", "yy5", "yy5_a",
  "sexo", "sexo_casen", "tipohogar_casen", "region_casen"
)

constructed_var_map <- tibble::tribble(
  ~domain, ~variable, ~role, ~description,
  
  "Survey design", "weight", "Design", "Expansion factor from EBS 2023",
  "Survey design", "strata", "Design", "Survey strata",
  "Survey design", "psu", "Design", "Primary sampling unit",
  
  "Primary outcome", "phq4_score", "Primary outcome", "Constructed PHQ-4 score, 0 to 12",
  "Primary outcome", "phq4_severity_f", "Descriptive outcome", "PHQ-4 severity categories",
  
  "Objective position", "income_group_3cat_f", "Objective-position model", "Low, middle, and high income groups",
  "Objective position", "education_3cat_f", "Objective-position model", "Primary or less, secondary, higher education",
  "Objective position", "housing_tenure_3cat_f", "Objective-position model", "Owner, renter, ceded/irregular/other tenure",
  "Objective position", "life_labor_stage_f", "Objective-position model", "Age-by-labor stage",
  "Objective position", "health_system_3cat_f", "Objective-position model", "FONASA, ISAPRE, other/no health system",
  "Objective position", "housing_deprivation_3cat_f", "Objective-position model", "No, one, or two or more housing deprivations",
  "Objective position", "territory_insecurity_3cat_f", "Objective-position model", "No, moderate, or high territorial insecurity",
  "Objective position", "sex_f", "Objective-position model", "Men and women",
  "Objective position", "macrozone_f", "Objective-position model", "North, Central/Metropolitan, South, Austral",
  
  "Subjective economic security", "health_financial_protection_3cat_f", "Residual PHQ-4 model", "Perceived financial protection in a health emergency",
  "Subjective economic security", "no_emergency_money_support_f", "Residual PHQ-4 model", "Absence of emergency monetary support",
  "Subjective economic security", "employment_risk_position_f", "Residual PHQ-4 model", "Perceived job-loss risk among employed plus outside-employment category",
  "Subjective economic security", "debt_status_f", "Residual PHQ-4 model", "Debt exposure and payment status",
  "Subjective economic security", "making_ends_meet_3cat_f", "Residual PHQ-4 model", "Difficulty, neutral, or ease making ends meet",
  
  "Summary subjective insecurity", "subjective_insecurity_count", "Configuration summary", "Count of subjective insecurity indicators",
  "Summary subjective insecurity", "subjective_insecurity_level_f", "Configuration summary", "Low, moderate, or high subjective insecurity",
  "Summary subjective insecurity", "prospective_insecurity_count", "Supplementary summary", "Count of prospective insecurity indicators",
  "Summary subjective insecurity", "prospective_insecurity_level_f", "Supplementary summary", "No, one, or multiple prospective insecurities",
  "Summary subjective insecurity", "current_financial_strain_count", "Supplementary summary", "Count of current financial strain indicators",
  "Summary subjective insecurity", "current_financial_strain_level_f", "Supplementary summary", "No, one, or multiple current financial strains"
)

data_overview <- tibble::tibble(
  item = c(
    "Loaded object",
    "Rows in raw data",
    "Columns in raw data",
    "Rows in analytic data",
    "Columns in analytic data",
    "Project directory",
    "Raw data file",
    "Output directory"
  ),
  value = c(
    data_name,
    as.character(nrow(raw_data)),
    as.character(ncol(raw_data)),
    as.character(nrow(analytic)),
    as.character(ncol(analytic)),
    project_dir,
    raw_data_file,
    script_output_dir
  )
)

raw_availability <- tibble::tibble(variable = unique(raw_variables_used)) %>%
  dplyr::mutate(
    available = variable %in% names(raw_data),
    variable_label = purrr::map_chr(
      variable,
      ~ if (.x %in% names(raw_data)) safe_label(raw_data[[.x]]) else NA_character_
    ),
    n_missing_raw = purrr::map_int(
      variable,
      ~ if (.x %in% names(raw_data)) sum(is.na(raw_data[[.x]])) else NA_integer_
    ),
    pct_missing_raw = purrr::map_dbl(
      variable,
      ~ if (.x %in% names(raw_data)) mean(is.na(raw_data[[.x]])) * 100 else NA_real_
    )
  )

constructed_availability <- constructed_var_map %>%
  dplyr::mutate(
    available = variable %in% names(analytic),
    n_missing = purrr::map_int(
      variable,
      ~ if (.x %in% names(analytic)) sum(is.na(analytic[[.x]])) else NA_integer_
    ),
    pct_missing = purrr::map_dbl(
      variable,
      ~ if (.x %in% names(analytic)) mean(is.na(analytic[[.x]])) * 100 else NA_real_
    )
  )

constructed_categorical_vars <- c(
  "income_decile_f",
  "income_group_3cat_f",
  "education_3cat_f",
  "housing_tenure_3cat_f",
  "health_system_3cat_f",
  "age_group_5cat_f",
  "activity_status_f",
  "life_labor_stage_f",
  "housing_deprivation_3cat_f",
  "territory_insecurity_3cat_f",
  "macrozone_f",
  "phq4_severity_f",
  "making_ends_meet_3cat_f",
  "health_financial_protection_3cat_f",
  "no_emergency_money_support_f",
  "perceived_job_loss_risk_3cat_f",
  "employment_risk_position_f",
  "debt_status_f",
  "subjective_insecurity_level_f",
  "prospective_insecurity_level_f",
  "current_financial_strain_level_f",
  "sex_f",
  "household_type_f"
)

constructed_numeric_vars <- c(
  "income_decile",
  "homeownership",
  "housing_deprivation_count",
  "territory_insecurity_count",
  "phq4_score",
  "phq4_existing_clean",
  "health_financially_unprotected",
  "no_emergency_money_support",
  "high_or_uncertain_job_loss_risk",
  "has_debt",
  "debt_payment_problem",
  "difficulty_making_ends_meet",
  "subjective_insecurity_count",
  "prospective_insecurity_count",
  "current_financial_strain_count"
)

constructed_categorical_frequencies <- purrr::map_dfr(
  constructed_categorical_vars,
  ~ weighted_freq_local(analytic, .x, "weight")
)

constructed_numeric_summaries <- purrr::map_dfr(
  constructed_numeric_vars,
  ~ numeric_summary_local(analytic, .x, "weight")
)

missingness_by_domain <- constructed_availability %>%
  dplyr::group_by(domain, role) %>%
  dplyr::summarise(
    variables = dplyr::n(),
    mean_pct_missing = mean(pct_missing, na.rm = TRUE),
    max_pct_missing = max(pct_missing, na.rm = TRUE),
    variables_with_any_missing = sum(pct_missing > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_pct_missing))

phq4_validation <- analytic %>%
  dplyr::transmute(
    phq4_score,
    phq4_existing_clean,
    difference = phq4_score - phq4_existing_clean
  ) %>%
  dplyr::summarise(
    n_both_valid = sum(!is.na(phq4_score) & !is.na(phq4_existing_clean)),
    n_constructed_valid = sum(!is.na(phq4_score)),
    n_existing_valid = sum(!is.na(phq4_existing_clean)),
    mean_constructed = mean(phq4_score, na.rm = TRUE),
    mean_existing = mean(phq4_existing_clean, na.rm = TRUE),
    correlation = suppressWarnings(
      stats::cor(phq4_score, phq4_existing_clean, use = "complete.obs")
    ),
    max_abs_difference = max(abs(difference), na.rm = TRUE),
    pct_exact_match = mean(difference == 0, na.rm = TRUE) * 100
  ) %>%
  dplyr::mutate(
    max_abs_difference = ifelse(
      is.infinite(max_abs_difference),
      NA_real_,
      max_abs_difference
    )
  )

phq4_reliability <- alpha_from_items(
  analytic,
  c("phq4_item_a", "phq4_item_b", "phq4_item_c", "phq4_item_d")
)

sample_coverage <- tibble::tibble(
  sample_definition = c(
    "Full raw data",
    "Valid weight, PSU and strata",
    "Valid PHQ-4",
    "Valid objective-position baseline",
    "Valid PHQ-4 + objective-position baseline",
    "Valid PHQ-4 + objective-position baseline + subjective-security specific indicators",
    "Valid PHQ-4 + objective-position baseline + subjective-security summary indicators"
  ),
  condition = c(
    "No restriction",
    "!is.na(weight) & !is.na(psu) & !is.na(strata)",
    "!is.na(phq4_score)",
    "Complete objective-position baseline",
    "Valid PHQ-4 and complete objective-position baseline",
    "Valid PHQ-4, objective-position baseline, and specific subjective-security indicators",
    "Valid PHQ-4, objective-position baseline, and summary subjective-security indicators"
  ),
  unweighted_n = c(
    nrow(analytic),
    sum(!is.na(analytic$weight) & !is.na(analytic$psu) & !is.na(analytic$strata)),
    sum(!is.na(analytic$phq4_score)),
    sum(stats::complete.cases(analytic[, objective_position_vars])),
    sum(stats::complete.cases(analytic[, c("phq4_score", objective_position_vars)])),
    sum(stats::complete.cases(analytic[, c("phq4_score", objective_position_vars, subjective_security_specific_vars)])),
    sum(stats::complete.cases(analytic[, c("phq4_score", objective_position_vars, subjective_security_summary_vars)]))
  ),
  weighted_n = c(
    sum(analytic$weight, na.rm = TRUE),
    sum(analytic$weight[!is.na(analytic$weight) & !is.na(analytic$psu) & !is.na(analytic$strata)], na.rm = TRUE),
    sum(analytic$weight[!is.na(analytic$phq4_score)], na.rm = TRUE),
    sum(analytic$weight[stats::complete.cases(analytic[, objective_position_vars])], na.rm = TRUE),
    sum(analytic$weight[stats::complete.cases(analytic[, c("phq4_score", objective_position_vars)])], na.rm = TRUE),
    sum(analytic$weight[stats::complete.cases(analytic[, c("phq4_score", objective_position_vars, subjective_security_specific_vars)])], na.rm = TRUE),
    sum(analytic$weight[stats::complete.cases(analytic[, c("phq4_score", objective_position_vars, subjective_security_summary_vars)])], na.rm = TRUE)
  )
)

planned_downstream_outputs <- tibble::tribble(
  ~script, ~planned_output, ~source_variables,
  "02_objective_gradient_residual_phq4.R", "phq4_expected_objective", paste(objective_position_vars, collapse = "; "),
  "02_objective_gradient_residual_phq4.R", "phq4_residual_objective", "phq4_score; phq4_expected_objective",
  "02_objective_gradient_residual_phq4.R", "phq4_expected_tercile_f", "phq4_expected_objective",
  "02_objective_gradient_residual_phq4.R", "phq4_residual_position_f", "phq4_residual_objective_z",
  "03_subjective_security_residual_associations.R", "Residual PHQ-4 models", paste(subjective_security_specific_vars, collapse = "; "),
  "04_within_gradient_heterogeneity.R", "Within-gradient residual summaries", "phq4_expected_tercile_f; subjective_insecurity_level_f; phq4_residual_objective",
  "05_expected_residual_configurations.R", "Expected-residual configurations", "phq4_expected_tercile_f; phq4_residual_position_f; subjective insecurity summaries"
)


# ------------------------------------------------------------
# 10. Diagnostic figures
# ------------------------------------------------------------

missing_plot_data <- constructed_availability %>%
  dplyr::filter(
    role %in% c(
      "Primary outcome",
      "Descriptive outcome",
      "Objective-position model",
      "Residual PHQ-4 model",
      "Configuration summary",
      "Supplementary summary"
    )
  ) %>%
  dplyr::arrange(dplyr::desc(pct_missing)) %>%
  dplyr::mutate(
    variable = forcats::fct_reorder(variable, pct_missing)
  )

p_missing <- ggplot2::ggplot(
  missing_plot_data,
  ggplot2::aes(x = variable, y = pct_missing)
) +
  ggplot2::geom_col(
    width = 0.75,
    fill = article_palette["purple_mid"]
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Missingness in constructed analytic variables",
    subtitle = "EBS 2023 analytic variable construction",
    x = NULL,
    y = "Missing values (%)"
  ) +
  main_theme(base_size = 13)

missing_plot_path <- file.path(
  script_figures_dir,
  "01_missingness_constructed_variables.png"
)

ggplot2::ggsave(
  filename = missing_plot_path,
  plot = p_missing,
  width = 12,
  height = 9,
  dpi = 300
)

key_distribution_vars <- c(
  "income_group_3cat_f",
  "education_3cat_f",
  "housing_tenure_3cat_f",
  "life_labor_stage_f",
  "making_ends_meet_3cat_f",
  "debt_status_f",
  "subjective_insecurity_level_f",
  "current_financial_strain_level_f"
)

key_distribution_data <- constructed_categorical_frequencies %>%
  dplyr::filter(variable %in% key_distribution_vars) %>%
  dplyr::mutate(
    variable_label = dplyr::recode(
      variable,
      income_group_3cat_f = "Income group",
      education_3cat_f = "Education",
      housing_tenure_3cat_f = "Housing tenure",
      life_labor_stage_f = "Life-labor stage",
      making_ends_meet_3cat_f = "Making ends meet",
      debt_status_f = "Debt status",
      subjective_insecurity_level_f = "Subjective insecurity",
      current_financial_strain_level_f = "Current financial strain",
      .default = variable
    ),
    value = stringr::str_wrap(value, 28)
  )

p_key_dist <- ggplot2::ggplot(
  key_distribution_data,
  ggplot2::aes(x = value, y = weighted_pct)
) +
  ggplot2::geom_col(
    width = 0.75,
    fill = article_palette["mustard_mid"]
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ variable_label, scales = "free_y") +
  ggplot2::labs(
    title = "Weighted distributions of core constructed variables",
    subtitle = "Percentages use the EBS expansion factor",
    x = NULL,
    y = "Weighted percentage"
  ) +
  main_theme(base_size = 12)

key_distribution_plot_path <- file.path(
  script_figures_dir,
  "01_core_constructed_variable_distributions.png"
)

ggplot2::ggsave(
  filename = key_distribution_plot_path,
  plot = p_key_dist,
  width = 14,
  height = 10,
  dpi = 300
)

phq4_distribution_data <- analytic %>%
  dplyr::filter(!is.na(phq4_score), !is.na(weight)) %>%
  dplyr::count(phq4_score, wt = weight, name = "weighted_n") %>%
  dplyr::mutate(weighted_pct = weighted_n / sum(weighted_n) * 100)

p_phq4 <- ggplot2::ggplot(
  phq4_distribution_data,
  ggplot2::aes(x = phq4_score, y = weighted_pct)
) +
  ggplot2::geom_col(
    width = 0.8,
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

phq4_distribution_plot_path <- file.path(
  script_figures_dir,
  "01_phq4_distribution.png"
)

ggplot2::ggsave(
  filename = phq4_distribution_plot_path,
  plot = p_phq4,
  width = 10,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# 11. Export outputs
# ------------------------------------------------------------

analytic_rds_path <- file.path(
  script_rds_dir,
  "01_ebs_analytic.rds"
)

saveRDS(
  analytic,
  analytic_rds_path
)

variable_groups_rds_path <- file.path(
  script_rds_dir,
  "01_variable_groups.rds"
)

saveRDS(
  variable_groups,
  variable_groups_rds_path
)

excel_path <- file.path(
  script_tables_dir,
  "01_data_preparation_tables.xlsx"
)

openxlsx::write.xlsx(
  list(
    data_overview = data_overview,
    raw_availability = raw_availability,
    constructed_var_map = constructed_var_map,
    constructed_availability = constructed_availability,
    sample_coverage = sample_coverage,
    phq4_validation = phq4_validation,
    phq4_reliability = phq4_reliability,
    constructed_cat_freqs = constructed_categorical_frequencies,
    constructed_num_summaries = constructed_numeric_summaries,
    missingness_by_domain = missingness_by_domain,
    planned_downstream_outputs = planned_downstream_outputs
  ),
  file = excel_path,
  overwrite = TRUE
)

readr::write_csv(
  constructed_var_map,
  file.path(script_csv_dir, "01_constructed_variable_map.csv")
)

readr::write_csv(
  constructed_availability,
  file.path(script_csv_dir, "01_constructed_variable_availability.csv")
)

readr::write_csv(
  sample_coverage,
  file.path(script_csv_dir, "01_sample_coverage.csv")
)

readr::write_csv(
  planned_downstream_outputs,
  file.path(script_csv_dir, "01_planned_downstream_outputs.csv")
)

manifest <- tibble::tibble(
  output = c(
    "Analytic dataset",
    "Variable groups list",
    "Excel diagnostics",
    "Constructed variable map CSV",
    "Constructed variable availability CSV",
    "Sample coverage CSV",
    "Planned downstream outputs CSV",
    "Missingness figure",
    "Core variable distribution figure",
    "PHQ-4 distribution figure"
  ),
  path = c(
    analytic_rds_path,
    variable_groups_rds_path,
    excel_path,
    file.path(script_csv_dir, "01_constructed_variable_map.csv"),
    file.path(script_csv_dir, "01_constructed_variable_availability.csv"),
    file.path(script_csv_dir, "01_sample_coverage.csv"),
    file.path(script_csv_dir, "01_planned_downstream_outputs.csv"),
    missing_plot_path,
    key_distribution_plot_path,
    phq4_distribution_plot_path
  )
)

manifest_path <- file.path(
  script_csv_dir,
  "01_output_manifest.csv"
)

readr::write_csv(
  manifest,
  manifest_path
)


# ------------------------------------------------------------
# 12. Save session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(script_logs_dir, "01_session_info.txt")
)


# ------------------------------------------------------------
# 13. Console summary
# ------------------------------------------------------------

message("\n============================================================")
message("01_data_preparation.R completed")
message("============================================================")
message("Loaded object: ", data_name)
message("Rows in analytic data: ", nrow(analytic))
message("Columns in analytic data: ", ncol(analytic))
message("Valid PHQ-4 cases: ", sum(!is.na(analytic$phq4_score)))
message(
  "Valid objective-position baseline cases: ",
  sum(stats::complete.cases(analytic[, objective_position_vars]))
)
message(
  "Valid PHQ-4 + objective baseline cases: ",
  sum(stats::complete.cases(analytic[, c('phq4_score', objective_position_vars)]))
)
message("Main analytic dataset: ", analytic_rds_path)
message("Variable groups list: ", variable_groups_rds_path)
message("Excel diagnostics: ", excel_path)
message("Figures saved in: ", script_figures_dir)
message("CSV outputs saved in: ", script_csv_dir)
message("============================================================\n")
