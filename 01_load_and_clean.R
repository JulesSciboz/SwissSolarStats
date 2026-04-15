# ===================================================================
# STEP 0: SETUP PROJECT ENVIRONMENT
# ===================================================================

# Check Git status and GitHub remote connection
# This uses the system() function to pass terminal commands directly to the OS.
# It ensures that your local environment is correctly linked to your GitHub 
# repository and reminds you if there are uncommitted changes before running.
print("Checking local Git status and GitHub remote link...")
system("git remote -v") 
system("git status")

# -------------------------------------------------------------------
# STEP 1: LOAD LIBRARIES
# -------------------------------------------------------------------

# 1A. Auto-Install Missing Packages
# The if(!require(...)) logic checks if a package is installed. 
# If it is missing, R will automatically install it. This makes the script 
# highly reproducible on other computers.
if(!require("readr")) install.packages("readr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("lubridate")) install.packages("lubridate")
if(!require("jsonlite")) install.packages("jsonlite")
if(!require("stringr")) install.packages("stringr")
if(!require("here")) install.packages("here")
if(!require("httr")) install.packages("httr")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("readxl")) install.packages("readxl")
if(!require("broom")) install.packages("broom")
if(!require("stargazer")) install.packages("stargazer")
if(!require("tidyr")) install.packages("tidyr")
if(!require("patchwork")) install.packages("patchwork")
if(!require("car")) install.packages("car")



# 1B. Load Libraries into the Environment
# --- Academic Formatting & Visualization ---
library(stargazer) # Generates publication-ready regression tables (HTML/LaTeX/Text)
library(broom)     # Converts messy regression model objects into tidy data frames
library(ggplot2)   # The standard package for creating advanced data visualizations
library(tidyr)     # TC.... 
library(patchwork) # TC...


# --- Data Import ---
library(readr)     # Fast and friendly way to read flat files (CSV, TSV)
library(readxl)    # Reads Microsoft Excel files (.xls and .xlsx)
library(jsonlite)  # Parses JSON data (used for the BFS election and population data)
library(httr)      # Handles HTTP requests for interacting with web APIs (LINDAS)

# --- Data Wrangling & Manipulation ---
library(dplyr)     # Core package for data manipulation (filter, mutate, select, join)
library(lubridate) # Simplifies working with dates and times (used for commissioning dates)
library(stringr)   # Provides tools for cleaning and manipulating text/character strings
library(car)       # Required for VIF (Variance Inflation Factor) multicollinearity tests


# --- Project Management ---
library(here)      # Manages file paths dynamically relative to the project root

# 1C. Establish Standard Directory Structure
# The 'here' package anchors the working directory to the project root.
print(paste("Project root automatically set to:", here()))

# Define the folders required for this data pipeline
dirs_to_create <- c(
  here("data"),             # Main data folder
  here("data", "raw"),      # Untouched, original downloaded data files
  here("data", "processed"),# Cleaned datasets ready for regression analysis
  here("plots")             # Output folder for generated charts and graphs
)

# Loop through the list and create any folders that do not currently exist.
# This prevents file-path errors later in the script when saving outputs.
for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir)
    print(paste("Created missing directory:", dir))
  }
}

# -------------------------------------------------------------------
# STEP 2: IMPORT BFE SOLAR DATA
# -------------------------------------------------------------------

# 1. Define File Path
# Using the here() function ensures that the file path works regardless of 
# which computer or operating system runs the script. It looks for the "data" 
# folder relative to the main project directory.
bfe_file_path <- here("data", "raw", "ElectricityProductionPlant.csv")
print(paste("Reading BFE file from:", bfe_file_path))

# 2. Load the Raw Data
# read_csv() is much faster than base R's read.csv(). 
# The locale = locale(encoding = "UTF-8") argument is absolutely critical here 
# because Swiss data frequently contains special characters (ä, ö, ü, é, à). 
# Without UTF-8 encoding, town names and addresses will be corrupted.
all_plants_raw <- read_csv(bfe_file_path, locale = locale(encoding = "UTF-8"))

# 3. Clean and Filter the Data (The Processing Pipeline)
# We use the %>% (pipe) operator to pass the data through a sequence of steps.
solar_growth_clean <- all_plants_raw %>%
  
  # A. Isolate Solar Power
  # The BFE dataset contains all types of power plants (hydro, wind, nuclear).
  # Filtering for "subcat_2" restricts our dataset strictly to Photovoltaic 
  # installations based on the official BFE catalogue mapping.
  filter(SubCategory == "subcat_2") %>% 
  
  # B. Standardize the Date Format
  # The BeginningOfOperation column might be imported as a generic character string.
  # We use lubridate's ymd() (Year-Month-Day) function to force it into a 
  # strict, calculable Date object.
  mutate(operation_date = ymd(BeginningOfOperation)) %>%
  
  # C. Apply the Study Period Time Boundary
  # *** CRITICAL FILTER: Start of 2018 to End of 2024 ***
  # We are only interested in *new* growth during our specific study period,
  # cutting off historical installations and anything commissioned after 2024.
  filter(operation_date >= "2018-01-01" & operation_date <= "2024-12-31") %>%
  
  # D. Trim the Fat
  # The raw dataset has dozens of columns we don't need (exact coordinates, etc.).
  # We select only the PostCode (for spatial mapping) and TotalPower (for our DV).
  # Dropping the rest saves massive amounts of RAM and speeds up later joins.
  select(PostCode, TotalPower)

# Output a diagnostic message to confirm how many installations survived the filter
print(paste("Solar data loaded. Installations in period:", nrow(solar_growth_clean)))

# -------------------------------------------------------------------
# STEP 3: IMPORT SWISSTOPO (FIXED: PRIORITIZE OFFICIAL INDEX)
# -------------------------------------------------------------------
# THE PROBLEM: The Swiss Federal Office of Energy (BFE) solar data only 
# provides Postal Codes (PLZ). However, in Switzerland, a single PLZ can 
# physically span across multiple official municipalities (BFS_Nr). 
# If we don't resolve these 1-to-many relationships, joining the data 
# will cause massive row duplication.

# 1. Define File Path & Import the Official Directory
swisstopo_file_path <- here("data", "raw", "AMTOVZ_CSV_LV95.csv")

print("Reading Swisstopo lookup file...")
# The official Swiss localities directory is semicolon-separated.
# Again, UTF-8 is critical to preserve French, German, and Italian names.
ortschaften_raw <- read_delim(swisstopo_file_path, delim = ";", locale = locale(encoding = "UTF-8"))

# 2. Build the Bulletproof Lookup Table
# We use a strict hierarchical sorting logic to decide which BFS_Nr gets 
# to "claim" a shared PLZ.
ortschaften_lookup <- ortschaften_raw %>%
  
  # A. Isolate the necessary geographic identifiers
  select(PLZ, Ortschaftsname, Gemeindename, `BFS-Nr`, Zusatzziffer, contains("Kanton")) %>% 
  
  # B. Standardize column names for seamless joining later
  rename(BFS_Nr = `BFS-Nr`, Canton = contains("Kanton")) %>%
  
  # C. Create a "Tie-Breaker" Flag
  # If the specific locality name (Ortschaft) exactly matches the overarching 
  # municipality name (Gemeinde), it gets a TRUE flag. This usually means it 
  # is the administrative center of that postal code.
  mutate(Is_Main_Commune = (Ortschaftsname == Gemeindename)) %>%
  
  # D. THE FIX: The Hierarchical Sorting Engine
  # This is the core logic that solves the shared PLZ problem.
  # 1. Group everything by PLZ first.
  # 2. Zusatzziffer (Ascending): The Swiss Post assigns a routing index. 
  #    Lower numbers indicate the primary sorting hub. We trust this official status first.
  #    (e.g., Fixes the Champoz issue: Valbirse [Zusatzziffer 2] beats Champoz [Zusatzziffer 3])
  # 3. Is_Main_Commune (Descending): If the routing indices are tied (both are 0), 
  #    we give priority to the exact name match.
  #    (e.g., Fixes Mont-Tramelan: Mont-Tramelan [0] beats Tramelan [0] for its specific PLZ)
  arrange(PLZ, Zusatzziffer, desc(Is_Main_Commune)) %>%
  
  # E. The Executioner: Lock in the Winner
  # Because we perfectly sorted the data above, the "correct" municipality for 
  # every PLZ is sitting at the very top of its group. distinct() scans down the 
  # PLZ column, keeps the first one it sees, and permanently deletes the losers.
  distinct(PLZ, .keep_all = TRUE) %>% 
  
  # F. Clean up the final output
  # We only keep the bare minimum needed to translate a PLZ into a BFS_Nr.
  select(PLZ, Gemeindename, BFS_Nr, Canton)

# Output a diagnostic message to confirm the logic executed
print("Swisstopo lookup created (Logic: Index > Name Match).")

# -------------------------------------------------------------------
# STEP 4: IMPORT POPULATION DATA (JSON)
# -------------------------------------------------------------------
# THE PROBLEM: The BFS STAT-TAB database exports JSON files with a highly 
# nested, complex hierarchy. Furthermore, Cantons, Districts, and Municipalities 
# are all crammed into a single column, separated only by text-based symbols 
# (e.g., "-" for Cantons, ">>" for Districts, and "......" for Municipalities).

# 1. Define File Path & Read the JSON
json_file_path <- here("data", "raw", "px-x-0102020000_201.json")

print("Reading Population JSON...")
# fromJSON automatically parses the nested lists into an R list object
json_data <- fromJSON(json_file_path)

# 2. Extract Data from the Nested JSON Tree
# The specific key where the geographical hierarchy is stored in the BFS JSON structure
geo_dim_key <- "Kanton (-) / Bezirk (>>) / Gemeinde (......)"

# Digging down into the JSON branches to extract the raw text labels 
# (e.g., "......0001 Aeugst am Albis")
geo_labels <- json_data$dataset$dimension[[geo_dim_key]]$category$label

# Digging down into the JSON branches to extract the actual numerical population counts
values_list <- json_data$dataset$value

# 3. Build the Raw Data Frame
# Combine the two extracted lists into a standard, flat R data frame
pop_raw <- data.frame(
  Label = unlist(geo_labels),
  Population = values_list,
  stringsAsFactors = FALSE
)

# 4. Clean and Extract the Municipality Data (Regex Parsing)
population_clean <- pop_raw %>%
  
  # A. Filter out Cantons and Districts
  # We ONLY want municipalities. In the BFS format, municipalities always 
  # start with exactly 6 dots ("......"). str_starts() drops everything else.
  filter(str_starts(Label, "\\.\\.\\.\\.\\.\\.")) %>% 
  
  # B. Extract the BFS_Nr and Name using Regular Expressions (Regex)
  mutate(
    # The Regex "(?<=\\.\\.\\.\\.\\.\\.)\\d{4}" uses a "positive lookbehind".
    # It tells R: "Look for the 6 dots, ignore them, and grab the exactly 
    # 4 digits (\\d{4}) that come immediately after."
    BFS_Nr = as.numeric(str_extract(Label, "(?<=\\.\\.\\.\\.\\.\\.)\\d{4}")),
    
    # We remove the 6 dots and the 4 numbers from the string to isolate the name,
    # and then use str_trim() to chop off any accidental leading/trailing spaces.
    Commune_Name_Pop = str_trim(str_remove(Label, "\\.\\.\\.\\.\\.\\.\\d{4} "))
  ) %>%
  
  # C. Trim the Fat
  # We only keep the BFS_Nr (for joining) and the Population (for calculating 
  # per-capita metrics later). The names are dropped to avoid clutter.
  select(BFS_Nr, Population)

# Output a diagnostic message to confirm the parsing worked
print("Population data extracted.")

# -------------------------------------------------------------------
# STEP 4.1: FETCH ELCOM DATA VIA LINDAS API (H1.A & H1.B)
# -------------------------------------------------------------------
# H1.A (Price Effect): Measured via Peak Price and Delta 2013-2023.
# H1.B (Profitability/FiT Proxy): Since direct FiT data is unavailable, 
# we fetch the Network Operator (DSO) name. Small/local operators act as 
# a proxy for higher feed-in tariffs and community-led adoption.

print("Fetching ElCom Prices and Operator Names via LINDAS API...")

# 1. Define the SPARQL Query
# We added dim:operator and schema:name to fetch the utility provider's label.
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

# 2. API Endpoint
endpoint <- "https://ld.admin.ch/query"

# 3. Make the API Request
response <- POST(
  url = endpoint,
  add_headers(Accept = "text/csv"),
  body = list(query = sparql_query),
  encode = "form"
)

# 4. Parse Response and Generate H1.B Proxy
if (status_code(response) == 200) {
  
  elcom_raw <- read_csv(content(response, "text", encoding = "UTF-8"), show_col_types = FALSE)
  
  elcom_clean <- elcom_raw %>%
    # Clean SPARQL RDF tags and quotes from the response
    mutate(across(everything(), ~ str_remove_all(., '\\^\\^<.*>'))) %>%
    mutate(across(everything(), ~ str_remove_all(., '"'))) %>%
    
    # Coerce to correct types
    mutate(
      BFS_Nr = as.numeric(bfs_nr),
      Operator_Name = as.character(Operator_Name),
      Mean_Price_13_23 = as.numeric(Mean_Price_13_23),
      Peak_Price_2023 = as.numeric(Peak_Price_2023),
      Delta_23_13 = as.numeric(Delta_23_13)
    ) %>%
    
    # --- H1.B PROXY LOGIC ---
    # We identify local community utilities (coops/communal SI) vs large regional monopolies.
    # Small local operators are a proxy for higher FiT/community-led profitability.
    group_by(Operator_Name) %>%
    mutate(Municipalities_Served = n()) %>%
    ungroup() %>%
    mutate(
      is_local_coop = ifelse(str_detect(Operator_Name, 
                                        "Genossenschaft|Services Industriels|Elektra|Communal|Gemeindebetriebe"), 1, 0),
      is_large_regional = ifelse(Municipalities_Served > 25, 1, 0)
    ) %>%
    
    select(BFS_Nr, Operator_Name, Mean_Price_13_23, Peak_Price_2023, Delta_23_13, is_local_coop, is_large_regional)
  
  print(paste("Successfully fetched Price (H1.A) and Operator Proxy (H1.B) for", nrow(elcom_clean), "municipalities."))
  
} else {
  stop(paste("Failed to fetch data from LINDAS API. Status code:", status_code(response)))
}

# -------------------------------------------------------------------
# STEP 4.2: IMPORT BFS VOTING DATA & BUILD GREEN INDEX (H2)
# -------------------------------------------------------------------
# THE PROBLEM: We have three separate Excel files representing three different 
# federal referendums on climate policy. Writing the exact same cleaning code 
# three times is inefficient and prone to errors.
# THE SOLUTION: We write a custom function that takes the file name and the 
# desired output column name, cleans the data, and returns a neat table.

print("Reading 3 BFS Voting Excel files and building Green Index...")

# 1. Define the Custom Cleaning Function
clean_vote_file <- function(file_name, vote_col_name) {
  # Load the raw Excel file
  raw_data <- read_excel(here("data", "raw", file_name))
  
  clean_data <- raw_data %>%
    # A. Rename columns by their index (1, 2, 3) instead of their exact names.
    # Excel headers from the BFS often change slightly or have weird spacing. 
    # Renaming by position ensures the script doesn't break.
    rename(
      Area_Code = 1,
      Area_Name = 2,
      Vote_ID = 3,
      Vote_Date = 4,
      Yes_Percent = 5
    ) %>%
    
    # B. Filter for Municipalities Only
    # Just like the population data, the BFS indents municipality names with 
    # exactly 6 dots ("......"). We use str_detect and Regex "^\\.\\.\\.\\.\\.\\." 
    # (starts with 6 dots) to drop Cantons and Districts.
    filter(str_detect(Area_Name, "^\\.\\.\\.\\.\\.\\.")) %>%
    
    mutate(
      BFS_Nr = as.numeric(Area_Code),
      
      # C. The European Decimal Fix
      # Sometimes Excel imports Swiss numbers as text with commas (e.g., "54,2").
      # R cannot do math on commas. We force it to character, swap the comma 
      # for a period with str_replace, and then convert to numeric.
      Yes_Percent = as.numeric(str_replace(as.character(Yes_Percent), ",", "."))
    ) %>%
    select(BFS_Nr, Yes_Percent) %>%
    
    # D. Dynamic Renaming
    # The '!!' (bang-bang) operator combined with ':=' allows us to use the 
    # string we passed into the function (vote_col_name) as an actual column name.
    rename(!!vote_col_name := Yes_Percent)
  
  return(clean_data)
}

# 2. Apply the Function
# One line of code per file! Much cleaner.
vote_2017 <- clean_vote_file("2017Energy.Act_Outcome_YESSHARE.xlsx", "Yes_2017")
vote_2021 <- clean_vote_file("2021CO2.Act_Outcome_YESSHARE.xlsx", "Yes_2021")
vote_2024 <- clean_vote_file("2024Climate.Protection.Act_Outcome_YESSHARE.xlsx", "Yes_2024")

# 3. Build the Green Index (The Core Variable for H2)
green_index_clean <- vote_2017 %>%
  # We use full_join instead of left_join. If a municipality didn't exist in 
  # 2017 but merged and existed in 2021, full_join keeps the data!
  full_join(vote_2021, by = "BFS_Nr") %>%
  full_join(vote_2024, by = "BFS_Nr") %>%
  
  # Group the dataframe row-by-row so the mean() function calculates the average 
  # ACROSS the columns for each specific municipality, rather than down the column.
  rowwise() %>%
  # c_across(starts_with("Yes_")) grabs all three vote columns automatically.
  # na.rm = TRUE ensures that if a town missed one vote, the average is just 
  # calculated from the other two.
  mutate(Green_Index = mean(c_across(starts_with("Yes_")), na.rm = TRUE)) %>%
  ungroup() %>% # Always ungroup after a rowwise operation to prevent slow performance later
  
  select(BFS_Nr, Yes_2017, Yes_2021, Yes_2024, Green_Index)

print(paste("Green Index successfully calculated for", nrow(green_index_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.3: IMPORT NATIONAL COUNCIL ELECTIONS (H3: Left-Green Strength)
# -------------------------------------------------------------------
print("Reading 2023 National Council Election JSON...")

json_file_path <- here("data", "raw", "NRW_2023_Dataset.json")

if (!file.exists(json_file_path)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", json_file_path))
}

elections_raw <- fromJSON(json_file_path)
elections_df <- elections_raw$level_gemeinden

# 4. Clean and aggregate the voting shares
left_green_clean <- elections_df %>%
  mutate(
    BFS_Nr = as.numeric(gemeinde_nummer),
    Votes = as.numeric(stimmen_liste),
    Partei_ID = as.numeric(partei_id)
  ) %>%
  group_by(BFS_Nr) %>%
  summarise(
    Total_Votes = sum(Votes, na.rm = TRUE),
    Left_Green_Votes = sum(Votes[Partei_ID %in% c(3, 13, 31)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # A. Calculate the raw percentage
  mutate(Left_Green_Share_2023 = (Left_Green_Votes / Total_Votes) * 100) %>%
  
  # B. THE FIX: Convert artificial 0% to NA (Missing Data)
  # na_if() automatically searches the column and replaces any exact 0 with NA.
  mutate(Left_Green_Share_2023 = na_if(Left_Green_Share_2023, 0)) %>%
  
  # C. Keep the final variables
  select(BFS_Nr, Left_Green_Share_2023)

print(paste("Left-Green voting share calculated for", nrow(left_green_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.4: IMPORT SOLAR IRRADIATION (Control Variable)
# -------------------------------------------------------------------
# THE PURPOSE: To prove that solar adoption isn't just driven by geography 
# (e.g., "sunny places build more panels"), we need a physical control variable. 
# This isolates the political and economic effects from the weather.

print("Reading Solar Irradiation Excel file...")

irradiation_file <- here("data", "raw", "solar_radiation_per_municipality.xlsx")

# 1. Safety Check
# Ensure the file exists before attempting to read it, throwing a clear error if missing.
if (!file.exists(irradiation_file)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", irradiation_file))
}

# 2. Read and Clean the Data
irradiation_raw <- read_excel(irradiation_file)

irradiation_clean <- irradiation_raw %>%
  mutate(
    BFS_Nr = as.numeric(bfs_nummer),
    # We directly extract the physical irradiation metric (kWh/m2) to act 
    # as our geographical control variable in the regression model.
    Irradiation_kWh_m2 = as.numeric(radiation_kWh_m2) 
  ) %>%
  select(BFS_Nr, Irradiation_kWh_m2)

print(paste("Solar irradiation data loaded for", nrow(irradiation_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.5: CALCULATE PEER EFFECTS (Baseline PV Density < 2018)
# -------------------------------------------------------------------
# THE PURPOSE: To test the "Neighborhood Effect" (Path Dependency). 
# Does seeing solar panels on your neighbors' roofs accelerate new adoption?
# We calculate the exact density of solar panels that existed *before* our 
# 2018-2024 study period even began.

print("Calculating 2017 Baseline PV Density for Peer Effects...")

library(lubridate)

# 1. Filter the raw BFE dataset & Match PostCodes to BFS_Nr
baseline_plants <- all_plants_raw %>%
  mutate(
    Commissioning_Date = as.Date(BeginningOfOperation),
    PostCode = as.numeric(PostCode) 
  ) %>%
  # *** TIME TRAVEL FILTER ***
  # We strictly isolate installations commissioned before January 1, 2018.
  filter(Commissioning_Date < as.Date("2018-01-01")) %>%
  
  # Use our previously built Swisstopo lookup to translate the PostCode 
  # into the official BFS_Nr, dropping any unmatchable rows.
  left_join(ortschaften_lookup, by = c("PostCode" = "PLZ")) %>%
  filter(!is.na(BFS_Nr))

# 2. Aggregate the Historical Baseline
baseline_counts <- baseline_plants %>%
  group_by(BFS_Nr) %>%
  summarise(
    # Count the total absolute number of physical installations per municipality
    Baseline_Installations_2017 = n(),
    .groups = "drop"
  )

# 3. Calculate Density (Installations per 1,000 inhabitants in 2017)
peer_effects_clean <- baseline_counts %>%
  left_join(population_clean, by = "BFS_Nr") %>%
  mutate(
    # Absolute counts are biased toward large cities. We divide by population 
    # to create a standardized "Visual Density" metric (panels per 1,000 people).
    Baseline_PV_Density_2017 = (Baseline_Installations_2017 / Population) * 1000
  ) %>%
  select(BFS_Nr, Baseline_PV_Density_2017)

print(paste("Peer effects baseline calculated for", nrow(peer_effects_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.6: IMPORT ESTV HOUSEHOLD WEALTH (Replacing Stadelmann)
# -------------------------------------------------------------------
print("Reading ESTV Taxable Income (Household Wealth Proxy)...")

wealth_file_path <- here("data", "raw", "27598_DE.csv")

if (!file.exists(wealth_file_path)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", wealth_file_path))
}

wealth_raw <- read_delim(wealth_file_path, delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

wealth_clean <- wealth_raw %>%
  
  # *** THE FIX: Squash the Cartesian Join ***
  # Keep ONLY the per-taxpayer rows by excluding any row with "Mio" (Millions) in the variable name
  filter(!str_detect(VARIABLE, "Mio")) %>%
  
  select(
    BFS_Nr = GEO_ID,
    Gemeindename_ESTV = GEO_NAME,   # <--- ADDED THE MUNICIPALITY NAME HERE!
    Taxable_Income = VALUE
  ) %>%
  mutate(
    BFS_Nr = as.numeric(BFS_Nr),
    Taxable_Income = as.numeric(Taxable_Income)
  ) %>%
  filter(!is.na(BFS_Nr))

print(paste("Household wealth data loaded for", nrow(wealth_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.7: IMPORT POPULATION DENSITY (The Roof Scarcity Proxy)
# -------------------------------------------------------------------
print("Reading Population Density Data...")

density_file_path <- here("data", "raw", "population_density_2018_2023(in).csv")

if (!file.exists(density_file_path)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", density_file_path))
}

# Assuming standard comma separation. If it fails, change to read_delim(..., delim = ";")
density_raw <- read_csv(density_file_path, show_col_types = FALSE)

density_clean <- density_raw %>%
  mutate(BFS_Nr = as.numeric(bfs_nummer)) %>%
  # Squash the longitudinal data: Calculate the average density across the study period
  group_by(BFS_Nr) %>%
  summarise(
    Population_Density = mean(population_density, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(BFS_Nr))

print(paste("Population density data loaded for", nrow(density_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.8: IMPORT HEAT PUMP DATA (Sector Coupling Proxy)
# -------------------------------------------------------------------
print("Reading Heat Pump Data...")

# REPLACE WITH YOUR EXACT FILENAME
hp_file_path <- here("data", "raw", "heating_pumps(in).csv") 

if (!file.exists(hp_file_path)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", hp_file_path))
}

hp_raw <- read_csv(hp_file_path, show_col_types = FALSE)

hp_clean <- hp_raw %>%
  mutate(BFS_Nr = as.numeric(bfs_nummer)) %>%
  # Squash the panel data by taking the mean across the available years
  group_by(BFS_Nr) %>%
  summarise(
    Heat_Pump_Share = mean(heating_pumps, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(BFS_Nr))

print(paste("Heat pump data loaded for", nrow(hp_clean), "municipalities."))


# -------------------------------------------------------------------
# STEP 5: JOIN EVERYTHING & CALCULATE DEPENDENT VARIABLES
# -------------------------------------------------------------------
print("Joining datasets...")

# 5a. Join Solar Data to Swisstopo
solar_mapped <- solar_growth_clean %>%
  mutate(PostCode = as.numeric(PostCode)) %>% 
  left_join(ortschaften_lookup, by = c("PostCode" = "PLZ")) %>%
  filter(!is.na(BFS_Nr))

# 5b. Aggregate Solar GROWTH per Commune
solar_agg_commune <- solar_mapped %>%
  group_by(BFS_Nr, Gemeindename, Canton) %>%
  summarise(
    New_Solar_kW = sum(TotalPower, na.rm = TRUE),
    New_Installations_Count = n(),
    .groups = "drop"
  )

# 5c. The Master Join
final_dataset <- solar_agg_commune %>%
  left_join(population_clean, by = "BFS_Nr") %>%
  left_join(elcom_clean, by = "BFS_Nr") %>%         
  left_join(green_index_clean, by = "BFS_Nr") %>%   
  left_join(left_green_clean, by = "BFS_Nr") %>%    
  left_join(irradiation_clean, by = "BFS_Nr") %>%   
  left_join(peer_effects_clean, by = "BFS_Nr") %>%  
  left_join(wealth_clean, by = "BFS_Nr") %>%        
  left_join(density_clean, by = "BFS_Nr") %>%      
  left_join(hp_clean, by = "BFS_Nr") %>%
  
  mutate(
    # Ensure baseline is 0 if no data exists
    Baseline_PV_Density_2017 = coalesce(Baseline_PV_Density_2017, 0),
    
    # Core Dependent Variables
    New_Watts_per_Capita = (New_Solar_kW * 1000) / Population,
    Adoption_Intensity = (New_Installations_Count / Population) * 1000
  ) %>%
  # Filter 1: Remove municipalities with fewer than 100 people (statistical noise)
  filter(!is.na(Population) & Population > 100) %>%
  # Filter 2: Ensure we have the operator data for H1.B
  filter(!is.na(Operator_Name))

print(paste("Final dataset created with", nrow(final_dataset), "municipalities."))

# -------------------------------------------------------------------
# STEP 6: SAVE RESULTS
# -------------------------------------------------------------------
saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))

print("Analysis complete. Final regression dataset saved to data/processed/.")

# -------------------------------------------------------------------
# STEP 7: SAVE MASTER DATASET & GENERATE DESCRIPTIVE STATISTICS
# -------------------------------------------------------------------

# 1. Save the Final Analytical Dataset
saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))
print("Analysis complete. Final dataset saved to data/processed/.")

print("Generating Descriptive Statistics...")

# 2. Prepare Data for Descriptives (Organized by Research Design)
desc_data <- final_dataset %>%
  select(
    # DEPENDENT VARIABLE
    New_Watts_per_Capita,
    
    # H1: ECONOMIC (Price & Profitability Proxy)
    Peak_Price_2023,          # H1.A
    is_local_coop,            # H1.B (Proxy for FiT/Profitability)
    
    # H2: SOCIAL (Peer Effects / Path Dependency)
    Baseline_PV_Density_2017, # The "Neighbor Effect"
    
    # H3: POLITICAL (Ideology)
    Left_Green_Share_2023,    
    
    # CONTROLS (Sector Coupling & Demographics)
    Heat_Pump_Share,          
    Taxable_Income,           
    Population_Density        
  ) %>%
  as.data.frame()

# 3. Define Clean Labels in Exactly the Same Order
final_labels <- c(
  "New PV Capacity (Watts/Capita)",           # Dependent
  "Peak Elec. Price 2023 (Rp/kWh) [H1.A]",    # H1
  "Community Utility Proxy (1/0) [H1.B]",     # H1
  "Baseline PV Density 2017 [H2]",            # H2
  "Left-Green Party Share (%) [H3]",          # H3
  "Heat Pump Share (%)",                      # Control
  "Taxable Income (CHF/Taxpayer)",            # Control
  "Population Density (Inh./km2)"             # Control
)

# 4. Print Descriptive Table to Console
stargazer(
  desc_data, 
  type = "text", 
  title = "Table 1: Descriptive Statistics by Hypothesis Grouping",
  digits = 2,
  covariate.labels = final_labels
)

# 5. Save Descriptive Table to Word for the Final Paper
stargazer(
  desc_data, 
  type = "html", 
  out = here("data", "processed", "Table1_Descriptive_Statistics.doc"),
  title = "Table 1: Descriptive Statistics",
  digits = 2,
  covariate.labels = final_labels
)

# -------------------------------------------------------------------
# STEP 7.1: FULL STRIP PLOTS
# -------------------------------------------------------------------
print("Generating Full Strip Plots for outlier detection...")

# 1. Prepare data with Hypothesis tags and Controls
strip_data_full <- final_dataset %>%
  select(
    `DepVar: New Watts` = New_Watts_per_Capita,
    `H1: Peak Price` = Peak_Price_2023, 
    `H2: Baseline 2017` = Baseline_PV_Density_2017, 
    `H3: Left-Green Share` = Left_Green_Share_2023,
    `Ctrl: Heat Pumps` = Heat_Pump_Share,
    `Ctrl: Income` = Taxable_Income,
    `Ctrl: Pop Density` = Population_Density,
    `Ctrl: Irradiation` = Irradiation_kWh_m2
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# 2. Render the Multi-Facet Plot
strip_plot_full <- ggplot(strip_data_full, aes(x = Variable, y = Value)) +
  # Jittered dots represent each unique Swiss municipality
  geom_jitter(width = 0.2, alpha = 0.25, color = "#34495e", size = 0.6) +
  # Boxplot provides the "Anchor" (Median and Interquartile Range)
  geom_boxplot(outlier.shape = NA, fill = "#e67e22", alpha = 0.5, color = "#d35400", width = 0.4) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(
    title = "Structural Audit: Distribution of All Model Variables",
    subtitle = "Spotting 'Funny' Data: Extreme vertical outliers indicate potential leverage points.",
    x = "",
    y = "Absolute Value"
  ) +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.major.x = element_blank()
  )

print(strip_plot_full)
ggsave(here("plots", "EDA_3_Strip_Plots_Full.png"), plot = strip_plot_full, width = 14, height = 10, dpi = 300)

# -------------------------------------------------------------------
# STEP 7.2: EXPLORATORY DATA ANALYSIS (Final Design Version)
# -------------------------------------------------------------------
print("Generating Exploratory Distributions (Histograms)...")

hist_clean_dataset <- final_dataset %>%
  # 1. Create clean, labeled, and logged versions mapped to Hypotheses
  mutate(
    `Dep. Var: New PV Watts/Capita` = New_Watts_per_Capita,
    `H1.A: Peak Price 2023` = Peak_Price_2023,
    `H1.B: Community Utility Proxy` = is_local_coop,
    `H2: Baseline PV Density 2017` = Baseline_PV_Density_2017,
    `H3: Left-Green Party Share` = Left_Green_Share_2023,
    `Control: Heat Pump Share` = Heat_Pump_Share,
    `Control: Taxable Income (Log)` = log(Taxable_Income),
    `Control: Population Density (Log)` = log(Population_Density),
    `Control: Solar Irradiation` = Irradiation_kWh_m2
  ) %>%
  # 2. Select ONLY the clean H-labeled names
  select(
    starts_with("Dep."), starts_with("H1"), starts_with("H2"), 
    starts_with("H3"), starts_with("Control")
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# --- THE PLOT ---
hist_plot <- ggplot(hist_clean_dataset, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#2c3e50", color = "white", alpha = 0.8) +
  # Use facet_wrap with 3 columns to keep the H-categories somewhat grouped
  facet_wrap(~ Variable, scales = "free", ncol = 3) + 
  theme_minimal() +
  labs(
    title = "Distribution of Primary and Control Variables across Swiss Municipalities",
    subtitle = "Visualizing Hypothesis-Driven Variables [H1-H3] and Log-Transformed Controls",
    x = "Value / Log Value",
    y = "Count of Municipalities"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(hist_plot)
ggsave(here("plots", "EDA_1_Histograms_Final.png"), plot = hist_plot, width = 12, height = 10, dpi = 300)

# -------------------------------------------------------------------
# STEP 8: FINAL REGRESSION MODELS (H1.A, H1.B, H2, H3)
# -------------------------------------------------------------------
print("Running Final Multivariate Regression Models...")

# 1. Run the OLS Regressions
# Note: log(Taxable_Income) and log(Population_Density) are permanent 
# to normalize distributions and handle extreme Swiss outliers.

# MODEL 1: Optimized Clean Model (Testing H1.A via Price Delta)
model_clean <- lm(
  New_Watts_per_Capita ~ Delta_23_13 + is_local_coop + Left_Green_Share_2023 + 
    Baseline_PV_Density_2017 + Heat_Pump_Share + 
    log(Taxable_Income) + log(Population_Density) + as.factor(Canton), 
  data = final_dataset
)

# MODEL 2: The "Full" Research Design Model (Includes Multicollinear Controls)
model_full <- lm(
  New_Watts_per_Capita ~ Delta_23_13 + is_local_coop + Left_Green_Share_2023 + 
    Baseline_PV_Density_2017 + Heat_Pump_Share + 
    log(Taxable_Income) + log(Population_Density) + 
    Green_Index + Irradiation_kWh_m2 + as.factor(Canton), 
  data = final_dataset
)

# MODEL 3: The "Peak Price" Model (Testing H1.A Scare Factor + H1.B Proxy)
# This is our strongest model based on previous console tests.
model_peak <- lm(
  New_Watts_per_Capita ~ Peak_Price_2023 + is_local_coop + Left_Green_Share_2023 + 
    Baseline_PV_Density_2017 + Heat_Pump_Share + 
    log(Taxable_Income) + log(Population_Density) + as.factor(Canton), 
  data = final_dataset
)

# -------------------------------------------------------------------
# STEP 8.1: DIAGNOSTICS (Multicollinearity / VIF)
# -------------------------------------------------------------------
print("Checking for Multicollinearity (VIF) on Optimized Model...")
vif_results <- vif(model_clean)
print(vif_results)

# 2. Prepare Data for the Plot (Plotting the Peak Price Model)
model_results <- tidy(model_peak, conf.int = TRUE) %>%
  filter(term != "(Intercept)" & !str_detect(term, "as.factor\\(Canton\\)")) %>% 
  mutate(
    term = case_when(
      term == "Peak_Price_2023" ~ "Peak Price 2023 [H1.A]",
      term == "is_local_coop" ~ "Community Utility Proxy [H1.B]",
      term == "Baseline_PV_Density_2017" ~ "Baseline PV Density 2017 [H2]",
      term == "Left_Green_Share_2023" ~ "Left-Green Party Share [H3]",
      term == "Heat_Pump_Share" ~ "Control: Heat Pump Share",
      term == "log(Taxable_Income)" ~ "Control: Taxable Income (Log)",
      term == "log(Population_Density)" ~ "Control: Population Density (Log)",
      TRUE ~ term
    )
  ) %>%
  # --- SORTING FOR THE PLOT ---
  # Force Hypothesis variables to the top, Controls to the bottom.
  mutate(term = factor(term, levels = rev(c(
    "Peak Price 2023 [H1.A]",
    "Community Utility Proxy [H1.B]",
    "Baseline PV Density 2017 [H2]",
    "Left-Green Party Share [H3]",
    "Control: Heat Pump Share",
    "Control: Population Density (Log)",
    "Control: Taxable Income (Log)"
  ))))

# 3. Render the Coefficient Forest Plot
coef_plot <- ggplot(model_results, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#2c3e50", linewidth = 1) +
  geom_point(size = 4, color = "#e67e22") +
  # Add numeric labels to dots to ensure clarity despite axis scaling
  geom_text(aes(label = round(estimate, 1)), vjust = -1.5, size = 3) +
  labs(
    title = "Predictors of Swiss Solar Adoption (Peak Price Model)",
    subtitle = "Primary Hypotheses [H1-H3] vs. Structural Controls | Cantonal FE Included",
    x = "Estimated Effect on Solar Capacity (Watts per Capita)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

print(coef_plot)
ggsave(here("plots", "Final_Regression_Forest_Plot.png"), plot = coef_plot, width = 11, height = 8, dpi = 300)

# 4. Generate the Academic Table (3-Way Comparison)
print("Generating Final Academic Regression Table...")

table_labels_final <- c(
  "Price Shock (Delta 2013-2023) [H1.A]",
  "Community Utility Proxy (1/0) [H1.B]",
  "Left-Green Party Share (%) [H3]",
  "Baseline PV Density 2017 [H2]",
  "Heat Pump Share (%)",
  "Control: Taxable Income (Log)",
  "Control: Population Density (Log)",
  "Peak Price 2023 (Absolute) [H1.A]",
  "Green Voting Index (Referendums)",
  "Control: Solar Irradiation (kWh/m2)"
)

# Output for the console (Markdown/Obsidian compatible)
stargazer(
  list(model_clean, model_full, model_peak),
  type = "text", 
  title = "Table 4: Regression Models for Swiss Municipal Solar Growth (2018-2024)",
  column.labels = c("Delta Model", "Full Design", "Peak Price Model"),
  dep.var.labels = c("New PV Capacity (Watts/Capita)"),
  covariate.labels = table_labels_final,
  omit = "Canton", 
  omit.labels = c("Cantonal Fixed Effects Included?"),
  keep.stat = c("n", "adj.rsq"),
  digits = 2
)

# Output for Word
stargazer(
  list(model_clean, model_full, model_peak),
  type = "html", 
  out = here("data", "processed", "Table4_Solar_Regression_Final.doc"),
  title = "Table 4: Final Regression Analysis",
  column.labels = c("Delta Model", "Full Design", "Peak Price Model"),
  dep.var.labels = c("New PV Capacity (Watts/Capita)"),
  covariate.labels = table_labels_final,
  omit = "Canton",
  omit.labels = c("Cantonal Fixed Effects Included?"),
  keep.stat = c("n", "adj.rsq"),
  digits = 2
)