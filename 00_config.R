# ===================================================================
# SCRIPT 00: ENVIRONMENT CONFIGURATION
# ===================================================================

# -------------------------------------------------------------------
# 1A. Essential Dependency Management
# -------------------------------------------------------------------
packages_required <- c(
  "readr", "dplyr", "lubridate", "jsonlite", "stringr", "here", 
  "httr", "ggplot2", "readxl", "broom", "stargazer", "tidyr", 
  "car"
)

for (pkg in packages_required) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# -------------------------------------------------------------------
# 1B. Establish Standard Directory Structure
# -------------------------------------------------------------------
# Anchors the project and ensures all output folders exist
dirs_to_create <- c(
  here("data", "raw"),      
  here("data", "processed"),
  here("plots"),
  here("scripts")
)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

print("Project environment optimized and initialized.")