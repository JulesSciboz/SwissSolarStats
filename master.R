# ===================================================================
# MASTER EXECUTION SCRIPT
# Project: Determinants of Solar Uptake in Switzerland
# ===================================================================

# 1. Initialize environment and dependencies
source("scripts/00_config.R")

# 2. Ingest and clean raw data (API calls, JSON parsing)
# Note: Execution takes ~2-3 minutes due to LINDAS API and BFS file size.
source("scripts/01_data_preparation.R")

# 3. Merge panel and serialize to RDS
source("scripts/02_data_assembly.R")

# -------------------------------------------------------------------
# ANALYTICAL BREAKPOINT
# Below scripts load data/processed/solar_growth_2018_2024_final.rds
# -------------------------------------------------------------------

# 4. Generate descriptive statistics and distributional audits
source("scripts/03_eda_visualizations.R")

# 5. Estimate OLS models, fixed effects, and robustness checks
source("scripts/04_regression_analysis.R")

print("Pipeline execution complete.")