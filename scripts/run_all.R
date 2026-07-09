# ============================================================
# run_all.R
# Project: Economic Security and Mental Health in Chile
# Purpose: Run the full replication pipeline from data preparation
#          to paper-ready figures and tables
# Author: Gonzalo Torres-Rosales
# ============================================================

# This script must be run from the root of the repository.
# Expected working directory:
# economic-security-mental-health-chile/

source("scripts/00_setup.R")

scripts_to_run <- c(
  "01_data_preparation.R",
  "02_phq4_and_descriptive_gradients.R",
  "03_objective_gradient_residual_phq4.R",
  "04_subjective_security_residual_associations.R",
  "05_within_gradient_heterogeneity.R",
  "06_configurational_diagnostics_expected_residual.R",
  "07_robustness_sensitivity_checks.R",
  "08_paper_ready_tables_figures.R"
)

for (script in scripts_to_run) {
  script_path <- file.path("scripts", script)
  
  message("\n============================================================")
  message("Running: ", script_path)
  message("============================================================\n")
  
  source(script_path)
}

message("\n============================================================")
message("Full replication pipeline completed successfully")
message("============================================================\n")
