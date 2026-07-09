# Scripts

This folder contains the R scripts used to reproduce the analysis for the article *Beyond Socioeconomic Gradients: Subjective Economic Security and Heterogeneity in Psychological Distress*.

The scripts are designed to be run from the root of the repository, not from inside this folder.

## Expected workflow

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
```

Raw data are not included in this repository. To run the pipeline, place the official EBS 2023 data file in `data_raw/` using the expected file name documented in `data_raw/README.md`.

## Script overview

| Script | Purpose |
|---|---|
| `00_setup.R` | Defines project paths, checks required packages, creates output folders, and loads shared helper functions. |
| `01_data_preparation.R` | Loads the raw EBS 2023 data and constructs the analytic variables used in the article. |
| `02_phq4_and_descriptive_gradients.R` | Documents PHQ-4 distribution, reliability, severity categories, and descriptive gradients. |
| `03_objective_gradient_residual_phq4.R` | Estimates objective-position models of PHQ-4 and constructs expected and residual PHQ-4. |
| `04_subjective_security_residual_associations.R` | Estimates associations between subjective economic-security indicators and residual PHQ-4. |
| `05_within_gradient_heterogeneity.R` | Examines residual PHQ-4 heterogeneity within objective-position expected distress strata. |
| `06_configurational_diagnostics_expected_residual.R` | Constructs expected-residual configurations and describes subjective-security profiles within them. |
| `07_robustness_sensitivity_checks.R` | Runs robustness and sensitivity checks for thresholds, model specifications, and alternative definitions. |
| `08_paper_ready_tables_figures.R` | Curates final figures, tables, inventories, and reporting notes for the manuscript and supplement. |

## Running the full pipeline

After confirming that the raw data file is in `data_raw/`, the full analysis can be run with:

```r
source("scripts/run_all.R")
```

Generated files are written to `output/`. These outputs are ignored by Git and are not included in the public repository.
