# Economic Security and Mental Health in Chile

Replication code for the manuscript:

**Beyond Socioeconomic Gradients: Subjective Economic Security and Heterogeneity in Psychological Distress**

This repository contains the R code used to reproduce the data preparation, analyses, figures, tables, supplementary materials, and robustness checks for a study using the 2023 Chilean Social Well-being Survey (*Encuesta de Bienestar Social*, EBS 2023).

## Data

The study uses publicly available secondary data from the 2023 Chilean Social Well-being Survey, made available by the Observatorio Social of the Ministry of Social Development and Family of Chile.

Raw survey data are not included in this repository. Users should download the original data and documentation from the official source and place the required file in the `data_raw/` directory.

The expected raw data file name is documented in `data_raw/README.md`.

## Repository structure

```text
economic-security-mental-health-chile/
  scripts/          R scripts used to reproduce the analysis
  data_raw/         Raw data folder; data are not tracked by Git
  data_processed/   Processed data folder; generated files are not tracked by Git
  output/           Generated tables, figures, logs, and intermediate files
  docs/             Additional documentation
```

Generated data, tables, figures, and logs are ignored by Git and are not included in the public repository.

## Reproducibility

The full pipeline should be run from the root of the repository, not from inside the `scripts/` folder.

After placing the official EBS 2023 data file in `data_raw/`, run:

```r
source("scripts/run_all.R")
```

This runs the full analysis pipeline from data preparation to paper-ready figures and tables.

For debugging or step-by-step replication, the scripts can also be run individually in numerical order:

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

## Script overview

| Script | Purpose |
|---|---|
| `scripts/00_setup.R` | Defines project paths, checks required packages, creates output folders, and loads shared helper functions. |
| `scripts/01_data_preparation.R` | Loads the raw EBS 2023 data and constructs the analytic variables used in the article. |
| `scripts/02_phq4_and_descriptive_gradients.R` | Documents PHQ-4 distribution, reliability, severity categories, and descriptive gradients. |
| `scripts/03_objective_gradient_residual_phq4.R` | Estimates objective-position models of PHQ-4 and constructs expected and residual PHQ-4. |
| `scripts/04_subjective_security_residual_associations.R` | Estimates associations between subjective economic-security indicators and residual PHQ-4. |
| `scripts/05_within_gradient_heterogeneity.R` | Examines residual PHQ-4 heterogeneity within objective-position expected distress strata. |
| `scripts/06_configurational_diagnostics_expected_residual.R` | Constructs expected-residual configurations and describes subjective-security profiles within them. |
| `scripts/07_robustness_sensitivity_checks.R` | Runs robustness and sensitivity checks for thresholds, model specifications, and alternative definitions. |
| `scripts/08_paper_ready_tables_figures.R` | Curates final figures, tables, inventories, and reporting notes for the manuscript and supplement. |
| `scripts/run_all.R` | Runs the full replication pipeline. |

## Outputs

Running the pipeline creates outputs in the `output/` directory.

The final paper-ready materials are written to:

```text
output/08_paper_ready_tables_figures/
```

This folder includes:

```text
figures_main/        Main manuscript figures
figures_supplement/  Supplementary figures
tables_main/         Main manuscript tables
tables_supplement/   Supplementary tables
csv/                 Inventories and machine-readable summaries
rds/                 Metadata objects
```

The generated outputs are ignored by Git and are not part of the public repository.

## Computational environment

The project uses R. Required packages are checked in `scripts/00_setup.R`.

Users should install the required packages before running the pipeline. If a package is missing, `00_setup.R` will stop and report the missing package names.

## License

This repository is released under the MIT License. See `LICENSE` for details.

## Citation

Citation information is provided in `CITATION.cff`.
