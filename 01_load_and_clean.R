# ===================================================================
# STEP 0: SETUP PROJECT ENVIRONMENT
# ===================================================================

# Verify version control status and remote repository links.
# Utilizes system commands to ensure the local environment is synchronized 
# prior to executing the data pipeline.
print("Checking local Git status and remote repository links...")
system("git remote -v") 
system("git status")

# -------------------------------------------------------------------
# STEP 1: LOAD LIBRARIES & INITIALIZE ENVIRONMENT
# -------------------------------------------------------------------

# 1A. Dependency Management
# Ensures cross-platform reproducibility by verifying and automatically 
# installing any missing packages required for the analysis.
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

# 1B. Load Libraries into the Environment

# --- Academic Formatting & Visualization ---
library(stargazer) # Generates publication-ready regression tables (HTML/LaTeX/Text)
library(broom)     # Converts statistical model objects into tidy data frames
library(ggplot2)   # Facilitates advanced data visualization
library(tidyr)     # Provides functions for data reshaping and restructuring
library(patchwork) # Enables the composition of multiple plots into unified layouts
library(forcats)   # Handles factor level manipulation for ordered visualizations

# --- Data Import & API Interaction ---
library(readr)     # Optimizes reading of flat data files (CSV, TSV)
library(readxl)    # Parses Microsoft Excel files (.xls, .xlsx)
library(jsonlite)  # Parses JSON data structures from federal data portals
library(httr)      # Facilitates HTTP requests for LINDAS SPARQL API querying
library(here)      # Anchors relative file paths to the project root directory

# --- Data Wrangling & Manipulation ---
library(dplyr)     # Core library for data manipulation and transformation
library(lubridate) # Standardizes and manipulates date-time objects
library(stringr)   # Provides regular expression and string manipulation tools

# --- Econometrics & Robustness ---
library(car)       # Computes Variance Inflation Factors (VIF) for multicollinearity diagnostics
library(sandwich)  # Computes heteroskedasticity- and autocorrelation-consistent (HAC) standard errors
library(lmtest)    # Facilitates hypothesis testing and applies clustered standard errors

# 1C. Establish Standard Directory Structure
# Anchors the working directory to ensure file path integrity across different operating systems.
print(paste("Project root established at:", here()))

# Define the standard directory architecture required for the data pipeline.
dirs_to_create <- c(
  here("data"),             # Root data directory
  here("data", "raw"),      # Unprocessed, source data files
  here("data", "processed"),# Cleaned and merged analytical datasets
  here("plots")             # Output directory for generated visualizations
)

# Iteratively verify and construct the directory tree to prevent output routing errors.
for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir)
    print(paste("Initialized missing directory:", dir))
  }
}

# -------------------------------------------------------------------
# STEP 2: IMPORT & PROCESS BFE SOLAR INFRASTRUCTURE DATA
# -------------------------------------------------------------------

# 1. Define File Path and Ingest Data
bfe_file_path <- here("data", "raw", "ElectricityProductionPlant.csv")
print(paste("Ingesting BFE infrastructure data from:", bfe_file_path))

# Enforce UTF-8 encoding to preserve Swiss-specific geospatial and administrative characters.
all_plants_raw <- read_csv(bfe_file_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# 2. Data Cleaning and Temporal Filtering Pipeline
solar_growth_clean <- all_plants_raw %>%
  
  # A. Isolate Photovoltaic (PV) Installations
  # Restricts the federal dataset exclusively to solar capacity ("subcat_2") 
  # per the official BFE technical categorization.
  filter(SubCategory == "subcat_2") %>% 
  
  # B. Standardize Commissioning Dates
  # Casts the operational start date to a standard date object for temporal subsetting.
  mutate(operation_date = ymd(BeginningOfOperation)) %>%
  
  # C. Apply Temporal Boundaries (The Study Period)
  # Isolates new capacity additions strictly commissioned during the 
  # defined operational window (2018-2024).
  filter(operation_date >= as.Date("2018-01-01") & operation_date <= as.Date("2024-12-31")) %>%
  
  # D. Feature Selection for Dimensionality Reduction
  # Retains only the spatial identifier (PostCode) for subsequent administrative 
  # mapping and the primary dependent variable input (TotalPower).
  select(PostCode, TotalPower)

# Output diagnostic logging for sample size verification
print(paste("Temporal filtering complete. New PV installations in study period:", nrow(solar_growth_clean)))

# -------------------------------------------------------------------
# STEP 3: SPATIAL RESOLUTION (SWISSTOPO POSTAL-TO-MUNICIPAL MAPPING)
# -------------------------------------------------------------------
# Methodological Note: The BFE solar dataset provides geolocation via 
# Postal Codes (PLZ). Because a single PLZ can physically overlap multiple 
# official municipalities (BFS_Nr), a deterministic mapping protocol is 
# required to prevent Cartesian inflation (row duplication) during data merging.

# 1. Ingest Swisstopo Official Locality Directory
swisstopo_file_path <- here("data", "raw", "AMTOVZ_CSV_LV95.csv")
print("Ingesting Swisstopo geospatial lookup table...")

# The official Swiss localities directory utilizes semicolon delimiters.
ortschaften_raw <- read_delim(swisstopo_file_path, delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# 2. Construct Deterministic Spatial Lookup Table
ortschaften_lookup <- ortschaften_raw %>%
  
  # A. Isolate Required Geospatial Identifiers
  select(PLZ, Ortschaftsname, Gemeindename, `BFS-Nr`, Zusatzziffer, contains("Kanton")) %>% 
  rename(BFS_Nr = `BFS-Nr`, Canton = contains("Kanton")) %>%
  
  # B. Generate Administrative Center Indicator
  # Evaluates if the specific locality name matches the overarching municipality 
  # name, serving as a secondary heuristic for assigning shared postal codes.
  mutate(Is_Main_Commune = (Ortschaftsname == Gemeindename)) %>%
  
  # C. Hierarchical Resolution Logic
  # Resolves 1-to-many PLZ conflicts using a strict dual-tier sorting protocol:
  # 1. Primary: Swiss Post routing index (Zusatzziffer, ascending). Lower 
  #    indices represent the primary logistical hub for that PLZ.
  # 2. Secondary: Administrative center match (Is_Main_Commune, descending).
  arrange(PLZ, Zusatzziffer, desc(Is_Main_Commune)) %>%
  
  # D. Deduplication via Top-Rank Retention
  # Because the dataset is strictly ordered by the hierarchy above, retaining 
  # the first distinct observation per PLZ locks in the mathematically optimal match.
  distinct(PLZ, .keep_all = TRUE) %>% 
  
  # E. Final Feature Selection
  # Retains only the variables necessary for the spatial join.
  select(PLZ, Gemeindename, BFS_Nr, Canton)

# Output diagnostic logging
print("Spatial mapping logic executed: Deterministic 1-to-1 PLZ-to-BFS lookup established.")

# -------------------------------------------------------------------
# STEP 4: DEMOGRAPHIC DATA INGESTION & HIERARCHICAL PARSING (JSON)
# -------------------------------------------------------------------
# Methodological Note: The BFS STAT-TAB database exports nested JSON 
# structures where Cantons, Districts, and Municipalities are combined 
# within a single string vector. Topological parsing and string extraction 
# via Regular Expressions (Regex) are required to isolate municipal observations.

# 1. Ingest Federal Population JSON
json_file_path <- here("data", "raw", "px-x-0102020000_201.json")
print("Ingesting and parsing Federal Population JSON...")

# Parse the nested JSON into a standard R list object
json_data <- fromJSON(json_file_path)

# 2. Extract Vectors from Nested Hierarchy
# Target the specific topological key used by the BFS structure
geo_dim_key <- "Kanton (-) / Bezirk (>>) / Gemeinde (......)"

# Isolate spatial labels and corresponding population values
geo_labels <- json_data$dataset$dimension[[geo_dim_key]]$category$label
values_list <- json_data$dataset$value

# 3. Construct Intermediate Data Frame
pop_raw <- data.frame(
  Label = unlist(geo_labels),
  Population = values_list,
  stringsAsFactors = FALSE
)

# 4. Isolate Municipal Observations via Regular Expressions
population_clean <- pop_raw %>%
  
  # A. Filter Administrative Levels
  # Excludes Cantons ("-") and Districts (">>"). Municipalities are 
  # strictly denoted by a leading six-dot string ("......").
  filter(str_starts(Label, "\\.\\.\\.\\.\\.\\.")) %>% 
  
  # B. Extract Standardized Spatial Identifiers
  mutate(
    # Utilizes a positive lookbehind Regex to extract the exact 4-digit BFS_Nr 
    # immediately following the municipal prefix.
    BFS_Nr = as.numeric(str_extract(Label, "(?<=\\.\\.\\.\\.\\.\\.)\\d{4}")),
    
    # Strip administrative prefixes and leading/trailing whitespace to isolate the string name
    Commune_Name_Pop = str_trim(str_remove(Label, "\\.\\.\\.\\.\\.\\.\\d{4} "))
  ) %>%
  
  # C. Feature Selection
  # Retains only the identifier for the spatial join and the absolute population 
  # required for calculating per-capita adoption metrics.
  select(BFS_Nr, Population)

# Output diagnostic logging
print("Demographic parsing complete: Municipal population vectors standardized.")

# -------------------------------------------------------------------
# STEP 4.1: FETCH ELCOM TARIFF DATA VIA LINDAS API (H1 & CLUSTERING)
# -------------------------------------------------------------------
# Methodological Note: To model the economic price pressure hypothesis (H1), 
# this step queries the Swiss Federal Linked Data Service (LINDAS) to extract 
# historical electricity prices (2013-2023) directly from the Federal Electricity 
# Commission (ElCom). Additionally, it extracts the Distribution System 
# Operator (DSO) mapped to each municipality to enable standard error clustering.

print("Executing LINDAS API SPARQL query for ElCom tariff data...")

# 1. Define the SPARQL Query
# Targets the standard household profile (H4) to ensure price comparability 
# across all Swiss municipalities.
sparql_query <- '
PREFIX schema: <http://schema.org/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX cube: <https://cube.link/>
PREFIX dim: <https://energy.ld.admin.ch/elcom/electricityprice/dimension/>

SELECT ?bfs_nr 
       (MAX(?operator_label) AS ?Operator_Name)
       (AVG(?total_price) AS ?Mean_Price_13_23)
       (MAX(?price_2023) AS ?Peak_Price_2023)
       ((MAX(?price_2023) - MAX(?price_2013)) AS ?Delta_23_13)
WHERE {
  <https://energy.ld.admin.ch/elcom/electricityprice> cube:observationSet/cube:observation ?obs .
  
  ?obs dim:period ?period ;
       dim:municipality ?municipality ;
       dim:operator ?operator_uri ;
       dim:category <https://energy.ld.admin.ch/elcom/electricityprice/category/H4> ;
       dim:product <https://energy.ld.admin.ch/elcom/electricityprice/product/standard> ;
       dim:total ?total_price .
       
  ?municipality schema:identifier ?bfs_nr .
  ?operator_uri schema:name ?operator_label .
  
  FILTER(str(?period) >= "2013" && str(?period) <= "2023")
  
  BIND(IF(str(?period) = "2023", ?total_price, 0) AS ?price_2023)
  BIND(IF(str(?period) = "2013", ?total_price, 0) AS ?price_2013)
}
GROUP BY ?bfs_nr
'

# 2. Configure API Endpoint
endpoint <- "https://ld.admin.ch/query"

# 3. Execute HTTP POST Request
response <- POST(
  url = endpoint,
  add_headers(Accept = "text/csv"),
  body = list(query = sparql_query),
  encode = "form"
)

# 4. Parse Response and Cleanse Data
if (status_code(response) == 200) {
  
  # Read the raw CSV response
  elcom_raw <- read_csv(content(response, "text", encoding = "UTF-8"), show_col_types = FALSE)
  
  elcom_clean <- elcom_raw %>%
    # A. Cleanse RDF-specific syntax
    # The SPARQL response includes RDF type tags (^^<...>) and quotation marks. 
    # These must be stripped to cast the variables to standard numeric types.
    mutate(across(everything(), ~ str_remove_all(., '\\^\\^<.*>'))) %>%
    mutate(across(everything(), ~ str_remove_all(., '"'))) %>%
    
    # B. Cast to Correct Data Types
    mutate(
      BFS_Nr = as.numeric(bfs_nr),
      Operator_Name = as.character(Operator_Name),
      Mean_Price_13_23 = as.numeric(Mean_Price_13_23),
      Peak_Price_2023 = as.numeric(Peak_Price_2023),
      Delta_23_13 = as.numeric(Delta_23_13)
    ) %>%
    
    # C. Handle Missing Data Artifacts (THE FIX)
    # The SPARQL BIND function returns 0 if a year is missing. We convert 
    # these artificial zeros to NA so they do not distort the OLS regression.
    mutate(
      Peak_Price_2023 = ifelse(Peak_Price_2023 == 0, NA, Peak_Price_2023),
      Delta_23_13 = ifelse(Peak_Price_2023 == 0 | Delta_23_13 == 0, NA, Delta_23_13)
    ) %>%
    
    # D. Feature Selection
    select(BFS_Nr, Operator_Name, Mean_Price_13_23, Peak_Price_2023, Delta_23_13)
  
  print(paste("API integration successful: Extracted economic drivers and DSO mapping for", nrow(elcom_clean), "municipalities."))
  
} else {
  stop(paste("CRITICAL ERROR: Failed to fetch data from LINDAS API. HTTP Status:", status_code(response)))
}

# -------------------------------------------------------------------
# STEP 4.2: INGESTION OF FEDERAL REFERENDUM DATA (CLIMATE POLICY)
# -------------------------------------------------------------------
# Methodological Note: Aggregates municipal voting outcomes across three 
# major federal climate referendums to construct a continuous ideological 
# control variable representing local environmental sentiment.

print("Parsing BFS voting records to construct Climate Policy Index...")

# 1. Define Standardized Parsing Function
clean_vote_file <- function(file_name, vote_col_name) {
  raw_data <- read_excel(here("data", "raw", file_name))
  
  clean_data <- raw_data %>%
    # A. Positional Renaming 
    # Mitigates schema variations and inconsistent header spacing across 
    # different BFS export years.
    rename(
      Area_Code = 1,
      Area_Name = 2,
      Vote_ID = 3,
      Vote_Date = 4,
      Yes_Percent = 5
    ) %>%
    
    # B. Isolate Municipal Observations
    # Excludes Cantonal and District aggregates using strict string matching.
    filter(str_detect(Area_Name, "^\\.\\.\\.\\.\\.\\.")) %>%
    
    mutate(
      BFS_Nr = as.numeric(Area_Code),
      # C. Numeric Coercion & European Decimal Standardization
      # Casts European comma-decimals to standard periods for R computation.
      Yes_Percent = as.numeric(str_replace(as.character(Yes_Percent), ",", "."))
    ) %>%
    select(BFS_Nr, Yes_Percent) %>%
    
    # D. Dynamic Feature Naming
    rename(!!vote_col_name := Yes_Percent)
  
  return(clean_data)
}

# 2. Execute Parsing Function
vote_2017 <- clean_vote_file("2017Energy.Act_Outcome_YESSHARE.xlsx", "Yes_2017")
vote_2021 <- clean_vote_file("2021CO2.Act_Outcome_YESSHARE.xlsx", "Yes_2021")
vote_2024 <- clean_vote_file("2024Climate.Protection.Act_Outcome_YESSHARE.xlsx", "Yes_2024")

# 3. Construct Aggregate Climate Index
green_index_clean <- vote_2017 %>%
  # Utilize full joins to preserve municipalities that underwent administrative 
  # mergers or name changes during the 2017-2024 operational window.
  full_join(vote_2021, by = "BFS_Nr") %>%
  full_join(vote_2024, by = "BFS_Nr") %>%
  
  # Compute the row-wise mean to handle unbalanced panels (e.g., if a 
  # newly formed municipality missed the 2017 vote).
  rowwise() %>%
  mutate(Green_Index = mean(c_across(starts_with("Yes_")), na.rm = TRUE)) %>%
  ungroup() %>% 
  
  select(BFS_Nr, Yes_2017, Yes_2021, Yes_2024, Green_Index)

print(paste("Climate Index successfully aggregated for", nrow(green_index_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.3: IMPORT NATIONAL COUNCIL ELECTIONS (H3: LEFT-GREEN IDEOLOGY)
# -------------------------------------------------------------------
# Methodological Note: Extracts the combined voting share of the SP and GPS 
# parties from the 2023 National Council elections to operationalize the 
# primary political ideology hypothesis (H3).

print("Parsing 2023 National Council Election JSON...")

json_file_path <- here("data", "raw", "NRW_2023_Dataset.json")

if (!file.exists(json_file_path)) {
  stop(paste("CRITICAL ERROR: Data file not located at", json_file_path))
}

elections_raw <- fromJSON(json_file_path)
elections_df <- elections_raw$level_gemeinden

# 1. Clean and Aggregate Voting Shares
left_green_clean <- elections_df %>%
  mutate(
    BFS_Nr = as.numeric(gemeinde_nummer),
    Votes = as.numeric(stimmen_liste),
    Partei_ID = as.numeric(partei_id)
  ) %>%
  group_by(BFS_Nr) %>%
  summarise(
    Total_Votes = sum(Votes, na.rm = TRUE),
    # Isolate SP and GPS party identifiers (3, 13, 31)
    Left_Green_Votes = sum(Votes[Partei_ID %in% c(3, 13, 31)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # 2. Compute Relative Share and Handle Structural Zeros
  mutate(
    Left_Green_Share_2023 = (Left_Green_Votes / Total_Votes) * 100,
    # Coerce absolute zeros to NA to prevent artificial skew from 
    # municipalities that failed to report specific party splits.
    Left_Green_Share_2023 = na_if(Left_Green_Share_2023, 0)
  ) %>%
  
  select(BFS_Nr, Left_Green_Share_2023)

print(paste("Left-Green voting share extracted for", nrow(left_green_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.4: IMPORT SOLAR IRRADIATION (ENVIRONMENTAL CONTROL)
# -------------------------------------------------------------------
# Methodological Note: Introduces physical solar potential as an exogenous 
# control variable. This isolates socio-economic and policy variance by 
# controlling for baseline geographical sunlight disparities.

print("Ingesting Solar Irradiation Control Data...")

irradiation_file <- here("data", "raw", "solar_radiation_per_municipality.xlsx")

if (!file.exists(irradiation_file)) {
  stop(paste("CRITICAL ERROR: Data file not located at", irradiation_file))
}

irradiation_raw <- read_excel(irradiation_file)

irradiation_clean <- irradiation_raw %>%
  mutate(
    BFS_Nr = as.numeric(bfs_nummer),
    # Directly extract the physical irradiation metric (kWh/m2)
    Irradiation_kWh_m2 = as.numeric(radiation_kWh_m2) 
  ) %>%
  select(BFS_Nr, Irradiation_kWh_m2)

print(paste("Environmental control (Irradiation) mapped for", nrow(irradiation_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.5: OPERATIONALIZE PEER EFFECTS (BASELINE PV DENSITY < 2018)
# -------------------------------------------------------------------
# Methodological Note: To test for path dependency and localized spatial 
# spillover effects ("Neighborhood Effect"), this step calculates the exact 
# density of solar capacity (Watts/Capita) existing prior to the study window (2018-2024).

print("Calculating pre-treatment Baseline PV Density (2017) for Peer Effects...")

# 1. Isolate Historical Infrastructure Data
baseline_plants <- all_plants_raw %>%
  mutate(
    Commissioning_Date = as.Date(BeginningOfOperation),
    PostCode = as.numeric(PostCode) 
  ) %>%
  
  # A. Pre-Treatment Temporal Truncation
  # Strictly isolate installations commissioned prior to January 1, 2018.
  filter(Commissioning_Date < as.Date("2018-01-01")) %>%
  
  # B. Spatial Mapping
  # Utilize the Swisstopo deterministic lookup to map PLZ to BFS_Nr.
  left_join(ortschaften_lookup, by = c("PostCode" = "PLZ")) %>%
  filter(!is.na(BFS_Nr))

# 2. Aggregate Historical Capacity (NOT Counts)
baseline_capacity <- baseline_plants %>%
  group_by(BFS_Nr) %>%
  summarise(
    # Aggregate TotalPower (assuming raw Pronovo data is in kW) 
    # and multiply by 1000 to convert to Watts.
    Baseline_Total_Watts_2017 = sum(TotalPower, na.rm = TRUE) * 1000,
    .groups = "drop"
  )

# 3. Standardize to Density Metric (Watts/Capita)
peer_effects_clean <- baseline_capacity %>%
  left_join(population_clean, by = "BFS_Nr") %>%
  mutate(
    # Ensure perfect unit alignment with the Dependent Variable:
    # Divide total pre-2018 Watts by the municipal population.
    Baseline_PV_Density_2017 = Baseline_Total_Watts_2017 / Population
  ) %>%
  select(BFS_Nr, Baseline_PV_Density_2017)

print(paste("Pre-treatment peer effects baseline (Watts/Capita) calculated for", nrow(peer_effects_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.6: INGEST FEDERAL TAX DATA (HOUSEHOLD WEALTH PROXY)
# -------------------------------------------------------------------
# Methodological Note: Extracts per-taxpayer taxable income to serve as 
# the primary proxy for municipal household wealth and capital availability.

print("Ingesting ESTV Taxable Income Data...")

wealth_file_path <- here("data", "raw", "27598_DE.csv")

if (!file.exists(wealth_file_path)) {
  stop(paste("CRITICAL ERROR: Data file not located at", wealth_file_path))
}

wealth_raw <- read_delim(wealth_file_path, delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

wealth_clean <- wealth_raw %>%
  # Resolving Dimensionality Conflicts
  # Exclude aggregate cantonal/national rows (denoted by "Mio" for millions) 
  # to isolate strictly granular, per-taxpayer metrics.
  filter(!str_detect(VARIABLE, "Mio")) %>%
  
  select(
    BFS_Nr = GEO_ID,
    Gemeindename_ESTV = GEO_NAME, 
    Taxable_Income = VALUE
  ) %>%
  mutate(
    BFS_Nr = as.numeric(BFS_Nr),
    Taxable_Income = as.numeric(Taxable_Income)
  ) %>%
  filter(!is.na(BFS_Nr))

print(paste("Socio-economic control (Household Wealth) mapped for", nrow(wealth_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.7: IMPORT POPULATION DENSITY (URBANIZATION/SCARCITY PROXY)
# -------------------------------------------------------------------
# Methodological Note: Utilizes population density as a structural control 
# variable to proxy roof scarcity and the prevalence of multi-family 
# renter-occupied housing environments (the "split-incentive" problem).

print("Ingesting Population Density Data...")

density_file_path <- here("data", "raw", "population_density_2018_2023(in).csv")

if (!file.exists(density_file_path)) {
  stop(paste("CRITICAL ERROR: Data file not located at", density_file_path))
}

density_raw <- read_csv(density_file_path, show_col_types = FALSE)

density_clean <- density_raw %>%
  mutate(BFS_Nr = as.numeric(bfs_nummer)) %>%
  
  # Cross-Sectional Compression
  # Collapse the longitudinal panel structure by calculating the 
  # period-average density for the study window.
  group_by(BFS_Nr) %>%
  summarise(
    Population_Density = mean(population_density, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(BFS_Nr))

print(paste("Structural control (Population Density) mapped for", nrow(density_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.8: INGEST BUILDING STRUCTURE DATA (SINGLE-FAMILY HOMES)
# -------------------------------------------------------------------
# Methodological Note: Extracts the proportion of single-family homes per 
# municipality to act as a structural proxy for homeownership. This isolates 
# the "split-incentive" barrier, allowing the model to disentangle political 
# ideology from the physical capacity to install PV systems.

print("Ingesting and optimizing BFS Building Structure Data (SFH Share)...")

# Define file path to the 700MB GWS export
sfh_file_path <- here("data", "raw", "CH1.GWS,DF_GWS_REG1,1.0.0+all.csv")

if (!file.exists(sfh_file_path)) {
  stop(paste("CRITICAL ERROR: Data file not located at", sfh_file_path))
}

# 1. Memory-Optimized Ingestion
# We read only the 4 essential columns to prevent RAM overload from the 700MB file.
sfh_raw <- read_delim(
  sfh_file_path, 
  delim = ",", 
  col_select = c(GEMEINDENAME, TIME_PERIOD, `Building category`, OBS_VALUE),
  show_col_types = FALSE
)

# 2. Cross-Sectional Filtering & Aggregation
sfh_clean <- sfh_raw %>%
  # A. Temporal Truncation: Isolate the most recent reliable cross-section 
  # (Change to 2022 if available in your dataset, otherwise 2021 is fine)
  filter(TIME_PERIOD == 2021) %>%
  
  # B. Standardize Identifiers
  mutate(BFS_Nr = as.numeric(GEMEINDENAME)) %>%
  filter(!is.na(BFS_Nr)) %>%
  
  # C. Aggregate Building Counts per Municipality
  # Because the data is split by "Construction Period" (e.g., pre-1919), 
  # we must group and sum them to get the total contemporary building stock.
  group_by(BFS_Nr) %>%
  summarise(
    # Total Residential Buildings (Sum of all rows for this municipality)
    Total_Residential = sum(OBS_VALUE, na.rm = TRUE),
    
    # Single-Family Homes (Sum only where the category explicitly matches)
    # Using str_detect allows flexibility if the BFS text changes slightly
    SFH_Count = sum(OBS_VALUE[str_detect(`Building category`, "Single-family house")], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  
  # D. Construct the Structural Homeownership Proxy
  mutate(
    Share_SFH = (SFH_Count / Total_Residential) * 100,
    # Impute structural zeros for missing data
    Share_SFH = coalesce(Share_SFH, 0)
  ) %>%
  
  # E. Final Feature Selection
  select(BFS_Nr, Share_SFH)

print(paste("Structural control (Single-Family Home Share) successfully mapped for", nrow(sfh_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 5: RELATIONAL DATA MERGE & DEPENDENT VARIABLE CONSTRUCTION
# -------------------------------------------------------------------
# Methodological Note: Integrates the disparate spatial, demographic, 
# economic, and political indices into a unified cross-sectional analytical 
# dataframe via deterministic spatial joins (BFS_Nr).

print("Executing relational data merge to construct Master Panel...")

# 1. Spatial Integration of Dependent Variable
solar_mapped <- solar_growth_clean %>%
  mutate(PostCode = as.numeric(PostCode)) %>% 
  # Map postal codes to administrative municipal identifiers
  left_join(ortschaften_lookup, by = c("PostCode" = "PLZ")) %>%
  filter(!is.na(BFS_Nr))

# 2. Municipal Aggregation
solar_agg_commune <- solar_mapped %>%
  group_by(BFS_Nr, Gemeindename, Canton) %>%
  summarise(
    # Aggregate absolute capacity additions for the 2018-2024 window
    New_Solar_kW = sum(TotalPower, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Constructing the Analytical Master Panel
final_dataset <- solar_agg_commune %>%
  # Execute sequential left-joins to bind all structural predictors and controls
  left_join(population_clean, by = "BFS_Nr") %>%
  left_join(elcom_clean, by = "BFS_Nr") %>%          
  left_join(green_index_clean, by = "BFS_Nr") %>%   
  left_join(left_green_clean, by = "BFS_Nr") %>%    
  left_join(irradiation_clean, by = "BFS_Nr") %>%   
  left_join(peer_effects_clean, by = "BFS_Nr") %>%  
  left_join(wealth_clean, by = "BFS_Nr") %>%        
  left_join(density_clean, by = "BFS_Nr") %>%        
  left_join(sfh_clean, by = "BFS_Nr") %>%
  
  mutate(
    # Impute structural zeros: Municipalities with no pre-2018 installations 
    # correctly have a baseline density of 0, not NA.
    Baseline_PV_Density_2017 = coalesce(Baseline_PV_Density_2017, 0),
    
    # Standardize Core Dependent Variable
    # Convert kilowatts to watts and scale by population to allow for 
    # unbiased cross-sectional comparison between urban and rural communes.
    New_Watts_per_Capita = (New_Solar_kW * 1000) / Population
  ) %>%
  
  # A. Exclusion Criterion 1: Micro-Populations
  # Exclude municipalities with fewer than 100 residents to mitigate 
  # extreme per-capita outlier distortion and heteroskedasticity.
  filter(!is.na(Population) & Population > 100) %>%
  
  # B. Exclusion Criterion 2: Missing Institutional Infrastructure
  # Observations lacking DSO identification must be dropped, as they cannot 
  # be mapped into the clustered standard error matrix in the final OLS model.
  filter(!is.na(Operator_Name))

print(paste("Analytical Master Panel constructed. Final N =", nrow(final_dataset), "municipalities."))

# -------------------------------------------------------------------
# STEP 6: DATA SERIALIZATION
# -------------------------------------------------------------------
# Methodological Note: The finalized dataframe is serialized as an RDS object 
# to perfectly preserve factor levels, date formats, and numeric classes for 
# subsequent econometric modeling.

saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))

print("Data processing pipeline complete. Analytical dataset serialized to data/processed/.")

# -------------------------------------------------------------------
# STEP 7: GENERATE DESCRIPTIVE STATISTICS
# -------------------------------------------------------------------
# Methodological Note: Computes summary statistics across the complete 
# analytical panel to establish baseline distributional characteristics 
# prior to multivariate regression analysis.

print("Generating Descriptive Statistics Matrix...")

# 1. Isolate Analytical Variables
desc_data <- final_dataset %>%
  select(
    # DEPENDENT VARIABLE
    `New PV Capacity (Watts/Capita)` = New_Watts_per_Capita,
    
    # H1: ECONOMIC INCENTIVES
    `Peak Elec. Price 2023 (Rp/kWh) [H1]` = Peak_Price_2023,
    
    # H2: SOCIAL MOMENTUM
    `Baseline PV Density 2017 (Watts/Capita) [H2]` = Baseline_PV_Density_2017, 
    
    # H3: POLITICAL IDEOLOGY
    `Left-Green Party Share (%) [H3]` = Left_Green_Share_2023,
    
    # CONTROL VARIABLES
    `Taxable Income (CHF/Taxpayer)` = Taxable_Income,
    `Population Density (Inh./km2)` = Population_Density,
    `Solar Irradiation (kWh/m2)` = Irradiation_kWh_m2,
    `Single-Family Home Share (%)` = Share_SFH
  ) %>%
  as.data.frame()

# 2. Output to Console (Verification)
stargazer(
  desc_data, 
  type = "text", 
  title = "Table 1: Descriptive Statistics of Municipal Variables",
  digits = 2
)

# 3. Output to Final Document (HTML/Word)
stargazer(
  desc_data, 
  type = "html", 
  out = here("data", "processed", "Table1_Descriptive_Statistics.doc"),
  title = "Table 1: Descriptive Statistics of Municipal Variables",
  digits = 2
)

# -------------------------------------------------------------------
# STEP 7.1: BIVARIATE CORRELATION ANALYSIS (SCATTERPLOTS)
# -------------------------------------------------------------------
# Methodological Note: Visualizes unadjusted, bivariate linear trends 
# between individual predictors and the dependent variable to assess 
# structural directionality before applying covariate controls.

print("Executing Bivariate Visualizations...")

# 1. Transform Data for Faceted Plotting
scatter_data <- final_dataset %>%
  mutate(
    `H1: Peak Price 2023` = Peak_Price_2023,
    `H2: Baseline PV Density 2017 (Log)` = log(Baseline_PV_Density_2017 + 1),
    `H3: Left-Green Share (%)` = Left_Green_Share_2023,
    `Ctrl: Solar Irradiation` = Irradiation_kWh_m2,
    `Ctrl: Taxable Income (Log)` = log(Taxable_Income),
    `Ctrl: Pop. Density (Log)` = log(Population_Density),
    `Ctrl: SFH Share (%)` = Share_SFH
  ) %>%
  select(New_Watts_per_Capita, starts_with("H"), starts_with("Ctrl")) %>%
  pivot_longer(cols = -New_Watts_per_Capita, names_to = "Predictor", values_to = "Value")

# 2. Render Structural Correlation Plot
scatter_plot <- ggplot(scatter_data, aes(x = Value, y = New_Watts_per_Capita)) +
  # Mitigate overplotting via alpha blending for high-N municipal observations
  geom_point(alpha = 0.2, color = "#2ecc71", size = 1) + 
  # Overlay unadjusted linear models
  geom_smooth(method = "lm", color = "#c0392b", fill = "#e74c3c", alpha = 0.2) +
  facet_wrap(~ Predictor, scales = "free_x", ncol = 4) + 
  theme_minimal() +
  labs(
    title = "Structural Correlation Analysis: Unadjusted Bivariate Trends",
    subtitle = "Assessing directionality of H1-H3 and Controls prior to multivariate adjustments",
    x = "Independent Variable (Unit-Specific or Log Transformed)",
    y = "New PV Capacity (Watts/Capita)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

print(scatter_plot)
ggsave(here("plots", "EDA_2_Scatterplots_Final.png"), plot = scatter_plot, width = 14, height = 8, dpi = 300)

# -------------------------------------------------------------------
# STEP 7.2: DISTRIBUTIONAL AUDIT (HISTOGRAMS)
# -------------------------------------------------------------------
# Methodological Note: Validates variable distributions to justify the 
# log-transformations applied to highly skewed structural covariates 
# (Wealth and Population Density) required by OLS assumptions.

print("Executing Distributional Audits...")

# 1. Transform Data for Faceted Auditing
hist_clean_dataset <- final_dataset %>%
  mutate(
    `Dep. Var: New PV Watts/Capita` = New_Watts_per_Capita,
    `H1: Peak Price 2023` = Peak_Price_2023,
    `H2: Baseline PV Density 2017 (Log)` = log(Baseline_PV_Density_2017 + 1),
    `H3: Left-Green Party Share` = Left_Green_Share_2023,
    `Control: Taxable Income (Log)` = log(Taxable_Income),
    `Control: Population Density (Log)` = log(Population_Density),
    `Control: Solar Irradiation` = Irradiation_kWh_m2,
    `Control: SFH Share (%)` = Share_SFH
  ) %>%
  select(
    starts_with("Dep."), starts_with("H1"), starts_with("H2"), 
    starts_with("H3"), starts_with("Control")
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# 2. Render Distribution Grid
hist_plot <- ggplot(hist_clean_dataset, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#2c3e50", color = "white", alpha = 0.8) +
  # Independent facet scaling allows visualization across heterogenous unit ranges
  facet_wrap(~ Variable, scales = "free", ncol = 3) + 
  theme_minimal() +
  labs(
    title = "Distributional Audit of Primary and Control Variables",
    subtitle = "Validating the application of logarithmic transformations for skewed structural proxies",
    x = "Observation Value",
    y = "Municipal Frequency"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(hist_plot)
ggsave(here("plots", "EDA_1_Histograms_Final.png"), plot = hist_plot, width = 12, height = 10, dpi = 300)

# -------------------------------------------------------------------
# STEP 8: PRIMARY MULTIVARIATE OLS REGRESSION (BASELINE)
# -------------------------------------------------------------------
# Methodological Note: Estimates the primary multivariate OLS specification 
# assessing the impact of economic, social, and political drivers on PV adoption.
# DSO clustering has been temporarily removed for the baseline draft.

print("Estimating Baseline OLS Model...")

# 1. Base Regression Specification
model_final <- lm(
  New_Watts_per_Capita ~ Peak_Price_2023 + 
    log(Baseline_PV_Density_2017 + 1) +
    Left_Green_Share_2023 + 
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH + as.factor(Canton), 
  data = final_dataset
)

# 2. Data Transformation for Coefficient Visualization
model_results <- tidy(model_final, conf.int = TRUE) %>%
  # Exclude the intercept and cantonal dummy variables from the plot
  filter(term != "(Intercept)" & !str_detect(term, "as.factor\\(Canton\\)")) %>%
  mutate(
    term = case_when(
      term == "Peak_Price_2023" ~ "Peak Elec. Price 2023 [H1]",
      term == "log(Baseline_PV_Density_2017 + 1)" ~ "Baseline PV Density 2017 (Log) [H2]",
      term == "Left_Green_Share_2023" ~ "Left-Green Party Share [H3]",
      term == "Irradiation_kWh_m2" ~ "Control: Solar Irradiation",
      term == "log(Taxable_Income)" ~ "Control: Taxable Income (Log)",
      term == "log(Population_Density)" ~ "Control: Population Density (Log)",
      term == "Share_SFH" ~ "Control: Single-Family Home Share (%)",
      TRUE ~ term
    ),
    # Create grouping for facets to solve the scale disparity
    Variable_Group = ifelse(str_detect(term, "\\[H"), "Primary Hypotheses", "Control Variables"),
    # Evaluate Statistical Significance based on standard CI
    Significant = ifelse(conf.low > 0 | conf.high < 0, "Significant (p < 0.05)", "Not Significant")
  ) %>%
  mutate(
    term = factor(term, levels = rev(c(
      "Peak Elec. Price 2023 [H1]",
      "Baseline PV Density 2017 (Log) [H2]",
      "Left-Green Party Share [H3]",
      "Control: Solar Irradiation",
      "Control: Taxable Income (Log)",
      "Control: Population Density (Log)",
      "Control: Single-Family Home Share (%)"
    ))),
    Variable_Group = factor(Variable_Group, levels = c("Primary Hypotheses", "Control Variables"))
  )

# 3. Render Coefficient Forest Plot (Faceted & Color-Coded)
coef_plot <- ggplot(model_results, aes(x = estimate, y = term, color = Significant)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = round(estimate, 1)), vjust = -1.5, size = 3.5, fontface = "bold", color = "black") +
  # Split into two panels with independent X-axes
  facet_wrap(~Variable_Group, scales = "free", ncol = 1) +
  # Define custom colors for significance
  scale_color_manual(values = c("Significant (p < 0.05)" = "#2c3e50", "Not Significant" = "#bdc3c7")) +
  labs(
    title = "Structural Drivers of Swiss Municipal Solar Adoption",
    subtitle = "Baseline Multivariate OLS Model | Error Bars: 95% CI (Standard OLS)", 
    x = "Estimated Effect on New Capacity (Watts per Capita)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(coef_plot)
ggsave(here("plots", "Baseline_Regression_Plot.png"), plot = coef_plot, width = 11, height = 8, dpi = 300)

# -------------------------------------------------------------------
# STEP 8.1: ACADEMIC TABLE GENERATION (MAIN RESULTS)
# -------------------------------------------------------------------

# Standardize nomenclature for publication-ready outputs
final_labels_clean <- c(
  "Peak Elec. Price 2023 (Rp/kWh) [H1]",
  "Baseline PV Density 2017 (Log) [H2]",
  "Left-Green Party Share (%) [H3]",
  "Control: Solar Irradiation (kWh/m2)",
  "Control: Taxable Income (Log)",
  "Control: Population Density (Log)",
  "Control: Single-Family Home Share (%)"
)

# 1. Output to Console (Verification)
stargazer(
  model_final,
  type = "text",
  # SE argument removed
  title = "Table 4: Regression Analysis of Swiss Municipal Solar Adoption",
  dep.var.labels = "New PV Capacity (Watts/Capita)",
  covariate.labels = final_labels_clean,
  column.labels = "Optimized Model",
  # Force layout to match final_labels_clean
  order = c("Peak_Price", "Baseline", "Left_Green", "Irradiation", "Taxable_Income", "Population_Density", "Share_SFH"),
  omit = "Canton",
  add.lines = list(c("Cantonal Fixed Effects Included?", "Yes")),
  keep.stat = c("n", "adj.rsq"),
  digits = 2,
  notes = "Note: Standard OLS errors utilized for baseline draft."
)

# 2. Output to Word Document (Thesis Appendix/Results)
stargazer(
  model_final,
  type = "html",
  # SE argument removed
  out = here("data", "processed", "Table4_Final_Regression_Analysis.doc"), 
  title = "Table 4: Regression Analysis of Swiss Municipal Solar Adoption",
  dep.var.labels = "New PV Capacity (Watts/Capita)",
  covariate.labels = final_labels_clean,
  column.labels = "2018-2024",
  order = c("Peak_Price", "Baseline", "Left_Green", "Irradiation", "Taxable_Income", "Population_Density", "Share_SFH"),
  omit = "Canton",
  add.lines = list(c("Cantonal Fixed Effects Included?", "Yes")),
  keep.stat = c("n", "adj.rsq"),
  digits = 2,
  notes = "Note: Standard OLS errors utilized for baseline draft."
)

# -------------------------------------------------------------------
# STEP 8.2: METHODOLOGICAL JUSTIFICATION TABLE (FE ONLY)
# -------------------------------------------------------------------
# Methodological Note: Demonstrates model evolution to justify the inclusion 
# of Cantonal Fixed Effects (mitigating omitted variable bias).

print("Generating Model Evolution Table (Naive vs. FE)...")

# Model 1: Naive OLS (No Canton Fixed Effects, Standard SEs)
model_naive <- lm(
  New_Watts_per_Capita ~ Peak_Price_2023 + 
    log(Baseline_PV_Density_2017 + 1) +
    Left_Green_Share_2023 + 
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH, 
  data = final_dataset
)

# Model 2: Fixed Effects Included 
# (This is model_final from Step 8, which already uses standard SEs)

# Extract Standard Errors for comparative reporting
se_naive <- summary(model_naive)$coefficients[, 2]
se_fe <- summary(model_final)$coefficients[, 2]

# 1. Output to Console (Verification)
stargazer(
  list(model_naive, model_final),
  type = "text",
  se = list(se_naive, se_fe), 
  title = "Table 3: Model Evolution (The Impact of Cantonal Fixed Effects)",
  dep.var.labels = "New PV Capacity (Watts/Capita)",
  column.labels = c("Naive OLS", "+ Cantonal FE (Baseline)"),
  covariate.labels = final_labels_clean,
  omit = "Canton",
  add.lines = list(c("Cantonal Fixed Effects Included?", "No", "Yes")),
  keep.stat = c("n", "adj.rsq"),
  digits = 2,
  notes = "Note: Standard OLS errors utilized for baseline draft."
)

# 2. Output to Word Document
stargazer(
  list(model_naive, model_final),
  type = "html",
  se = list(se_naive, se_fe), 
  out = here("data", "processed", "Table_A0_Model_Evolution.doc"),
  title = "Table 3: Model Evolution (The Impact of Cantonal Fixed Effects)",
  dep.var.labels = "New PV Capacity (Watts/Capita)",
  column.labels = c("Naive OLS", "+ Cantonal FE (Baseline)"),
  covariate.labels = final_labels_clean,
  omit = "Canton",
  add.lines = list(c("Cantonal Fixed Effects Included?", "No", "Yes")),
  keep.stat = c("n", "adj.rsq"),
  digits = 2,
  notes = "Note: Standard OLS errors utilized for baseline draft."
)
# ===================================================================
# STEP 9: ROBUSTNESS CHECKS & SENSITIVITY ANALYSIS
# ===================================================================
# Methodological Note: Evaluates the structural stability of the core 
# findings by substituting the primary economic proxy and mitigating 
# the potential distortion of high-leverage outliers.

print("Executing Robustness Checks (Long-Term Price & Trimmed Panel)...")

# -------------------------------------------------------------------
# ROBUSTNESS MODEL 1: LONG-TERM PRICE PROXY (BEHAVIORAL SENSITIVITY)
# -------------------------------------------------------------------
# Methodological Note: Substitutes the acute 2023 peak price with the 
# 10-year historical average to test for 'status quo bias' versus 
# acute price shock salience.
model_robust_mean <- lm(
  New_Watts_per_Capita ~ Mean_Price_13_23 + 
    log(Baseline_PV_Density_2017 + 1) + Left_Green_Share_2023 +
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH + as.factor(Canton), 
  data = final_dataset
)

# -------------------------------------------------------------------
# ROBUSTNESS MODEL 2: HIGH-LEVERAGE OUTLIER EXCLUSION (TRIMMED PANEL)
# -------------------------------------------------------------------
# Methodological Note: Re-estimates the primary specification after 
# excluding the 99th percentile of the dependent variable to ensure 
# coefficients are not artificially driven by extreme "super-adopters."
threshold_99 <- quantile(final_dataset$New_Watts_per_Capita, 0.99, na.rm = TRUE)

data_trimmed <- final_dataset %>% filter(New_Watts_per_Capita <= threshold_99)

model_robust_trimmed <- lm(
  New_Watts_per_Capita ~ Peak_Price_2023 + 
    log(Baseline_PV_Density_2017 + 1) + Left_Green_Share_2023 + 
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH + as.factor(Canton), 
  data = data_trimmed
)

# -------------------------------------------------------------------
# GENERATE ROBUSTNESS TABLE (MAIN APPENDIX) Table A1
# -------------------------------------------------------------------
robust_labels <- c(
  "Peak Elec. Price 2023 [H1]",
  "10-Year Average Price (13-23) [Alt H1]",
  "Baseline PV Density 2017 (Log) [H2]",
  "Left-Green Party Share (%) [H3]",
  "Control: Solar Irradiation",
  "Control: Taxable Income (Log)",
  "Control: Population Density (Log)",
  "Control: Single-Family Home Share (%)"
)

# 1. Output to Console (Verification)
stargazer(
  list(model_final, model_robust_mean, model_robust_trimmed),
  type = "text",
  title = "Table A1: Robustness Checks (Average Price & Trimmed Sample)",
  dep.var.labels = "New PV Capacity (Watts/Capita)",
  column.labels = c("Main Model", "Long-Term Average", "Trimmed (No Top 1%)"),
  covariate.labels = robust_labels,
  order = c("Peak_Price", "Mean_Price", "Baseline", "Left_Green", "Irradiation", "Taxable_Income", "Population_Density", "Share_SFH"),
  omit = "Canton",
  add.lines = list(c("Cantonal Fixed Effects Included?", "Yes", "Yes", "Yes")),
  keep.stat = c("n", "adj.rsq"),
  digits = 2,
  notes = "Note: Standard OLS errors utilized for baseline draft. Model (3) excludes the 99th percentile."
)

# 2. Output to Word Document (Thesis Appendix)
stargazer(
  list(model_final, model_robust_mean, model_robust_trimmed),
  type = "html",
  out = here("data", "processed", "Table_A1_Robustness_Checks.doc"),
  title = "Table A1: Robustness Checks (Average Price & Trimmed Sample)",
  dep.var.labels = "New PV Capacity (Watts/Capita)",
  column.labels = c("Main Model", "Long-Term Average", "Trimmed (No Top 1%)"),
  covariate.labels = robust_labels,
  order = c("Peak_Price", "Mean_Price", "Baseline", "Left_Green", "Irradiation", "Taxable_Income", "Population_Density", "Share_SFH"),
  omit = "Canton",
  add.lines = list(c("Cantonal Fixed Effects Included?", "Yes", "Yes", "Yes")),
  keep.stat = c("n", "adj.rsq"),
  digits = 2,
  notes = "Note: Standard OLS errors utilized for baseline draft. Model (3) excludes the 99th percentile."
)
