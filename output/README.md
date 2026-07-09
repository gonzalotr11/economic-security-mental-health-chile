# output

This folder stores generated outputs from the replication pipeline, including tables, figures, logs, intermediate datasets, and metadata.

Generated files in this folder are ignored by Git and are not included in the public repository.

After running the full pipeline with:

```r
source("scripts/run_all.R")
```

the final paper-ready materials will be available in:

```text
output/08_paper_ready_tables_figures/
```
