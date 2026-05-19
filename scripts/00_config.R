# ===================================================================
# SCRIPT 00: ENVIRONMENT CONFIGURATION
# ===================================================================

print("Checking local Git status and remote repository links...")
system("git remote -v") 
system("git status")

# -------------------------------------------------------------------
# 1A. Dependency Management
# -------------------------------------------------------------------
packages_required <- c(
  "readr", "dplyr", "lubridate", "jsonlite", "stringr", "here", 
  "httr", "ggplot2", "readxl", "broom", "stargazer", "tidyr", 
  "patchwork", "car", "lmtest", "sandwich", "forcats"
)

for (pkg in packages_required) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# -------------------------------------------------------------------
# 1B. Load Libraries
# -------------------------------------------------------------------
library(stargazer) 
library(broom)     
library(ggplot2)   
library(tidyr)     
library(patchwork) 
library(forcats)   
library(readr)     
library(readxl)    
library(jsonlite)  
library(httr)      
library(here)      
library(dplyr)     
library(lubridate) 
library(stringr)   
library(car)       
library(sandwich)  
library(lmtest)    

# -------------------------------------------------------------------
# 1C. Establish Standard Directory Structure
# -------------------------------------------------------------------
print(paste("Project root established at:", here()))

dirs_to_create <- c(
  here("data"),             
  here("data", "raw"),      
  here("data", "processed"),
  here("plots"),
  here("scripts")
)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir)
    print(paste("Initialized missing directory:", dir))
  }
}