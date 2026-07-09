# Economic Security and Mental Health in Chile

Replication code for the manuscript:

**Beyond Socioeconomic Gradients: Subjective Economic Security and Heterogeneity in Psychological Distress**

This repository contains code to reproduce the data preparation, analyses, figures, tables, supplementary materials, and robustness checks for a study using the 2023 Chilean Social Well-being Survey (*Encuesta de Bienestar Social*, EBS 2023).

## Data

The study uses publicly available secondary data from the 2023 Chilean Social Well-being Survey, made available by the Observatorio Social of the Ministry of Social Development and Family of Chile.

Raw survey data are not included in this repository. Users should download the original data and documentation from the official source and place the required files in the `data_raw/` directory.

## Reproducibility

The analysis is organized as a sequence of R scripts. Scripts should be run in numerical order:

1. `scripts/00_setup.R`
2. `scripts/01_data_preparation.R`
3. `scripts/02_phq4_and_validation_outcomes.R`
4. `scripts/03_objective_gradient_residual_phq4.R`
5. `scripts/04_subjective_security_residual_associations.R`
6. `scripts/05_within_gradient_heterogeneity.R`
7. `scripts/06_configurational_diagnostics_expected_residual.R`
8. `scripts/07_robustness_sensitivity_checks.R`
9. `scripts/08_paper_ready_tables_figures.R`

Final outputs are generated in the corresponding `output_*` folders.

## Computational environment

The project uses R. Package dependencies are documented using `renv`. To restore the computational environment, run:

```r
renv::restore()
