# Scripts

This folder contains the R scripts used to reproduce the analysis for the article *Beyond Socioeconomic Gradients: Subjective Economic Security and Heterogeneity in Psychological Distress*.

The scripts are designed to be run from the root of the repository, not from inside this folder. The expected workflow is:

```r
source("scripts/00_setup.R")
source("scripts/01_data_preparation.R")
source("scripts/02_phq4_and_descriptive_gradients.R")
source("scripts/03_objective_gradient_residual_phq4.R")
source("scripts/04_subjective_security_residual_associations.R")
source("scripts/05_within_gradient_heterogeneity.R")
source("scripts/06_configurational_diagnostics_expected_residual.R")
source("scripts/07_robustness_sensitivity_checks.R")
source("scripts/08_paper_ready_tables_figures.R")
