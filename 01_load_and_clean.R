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

# 1B. Load Libraries into the Environment
# --- Academic Formatting & Visualization ---
library(stargazer) # Generates publication-ready regression tables (HTML/LaTeX/Text)
library(broom)     # Converts messy regression model objects into tidy data frames
library(ggplot2)   # The standard package for creating advanced data visualizations

# --- Data Import ---
library(readr)     # Fast and friendly way to read flat files (CSV, TSV)
library(readxl)    # Reads Microsoft Excel files (.xls and .xlsx)
library(jsonlite)  # Parses JSON data (used for the BFS election and population data)
library(httr)      # Handles HTTP requests for interacting with web APIs (LINDAS)

# --- Data Wrangling & Manipulation ---
library(dplyr)     # Core package for data manipulation (filter, mutate, select, join)
library(lubridate) # Simplifies working with dates and times (used for commissioning dates)
library(stringr)   # Provides tools for cleaning and manipulating text/character strings

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
# STEP 4.1: FETCH ELCOM ELECTRICITY PRICES VIA LINDAS API (H1.A)
# -------------------------------------------------------------------
# THE PROBLEM: Swiss electricity prices (ElCom) are stored in a massive, 
# multidimensional Linked Data cube on the federal LINDAS server. 
# Downloading the raw data would be gigabytes of unnecessary data. 
# THE SOLUTION: We write a SPARQL query to calculate the exact metrics we 
# need directly on the government's servers, returning only a tiny, clean CSV.

print("Fetching ElCom Historical Electricity Prices (2013-2023) via LINDAS API...")

# 1. Define the SPARQL Query
# SPARQL is a query language used for databases that store data as "triples" 
# (subject-predicate-object). 
sparql_query <- '
# A. PREFIXES: These map long URLs to short prefixes so the code is readable.
PREFIX schema: <http://schema.org/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX cube: <https://cube.link/>
PREFIX dim: <https://energy.ld.admin.ch/elcom/electricityprice/dimension/>

# B. SELECT: What columns do we want to get back?
SELECT ?bfs_nr 
       (AVG(?total_price) AS ?Mean_Price_13_23)
       (MAX(?price_2023) AS ?Peak_Price_2023)
       # We calculate the Price Shock directly on the server!
       ((MAX(?price_2023) - MAX(?price_2013)) AS ?Delta_23_13)
       
# C. WHERE: How to navigate the LINDAS data cube
WHERE {
  # 1. Look inside the ElCom electricity price observation set.
  <https://energy.ld.admin.ch/elcom/electricityprice> cube:observationSet/cube:observation ?obs .
  
  # 2. Define the specific "slices" of the data cube we want:
  ?obs dim:period ?period ;
       dim:municipality ?municipality ;
       # "H4" is the standard Swiss household profile (4,500 kWh/year, 5-room apartment).
       dim:category <https://energy.ld.admin.ch/elcom/electricityprice/category/H4> ;
       # "standard" ignores special eco-power or cheap night-tariffs.
       dim:product <https://energy.ld.admin.ch/elcom/electricityprice/product/standard> ;
       dim:total ?total_price .
       
  # 3. Translate the municipality URL into the official BFS number.
  ?municipality schema:identifier ?bfs_nr .
  
  # 4. Filter for our specific study window (2013 to 2023).
  FILTER(str(?period) >= "2013" && str(?period) <= "2023")
  
  # 5. The BIND Trick: 
  # We create temporary variables holding ONLY the 2013 and 2023 prices. 
  # This allows the SELECT statement above to subtract them for the Delta.
  BIND(IF(str(?period) = "2023", ?total_price, 0) AS ?price_2023)
  BIND(IF(str(?period) = "2013", ?total_price, 0) AS ?price_2013)
}
# D. GROUP BY: Collapse the 10 years of data into exactly 1 row per municipality.
GROUP BY ?bfs_nr
'

# 2. Define the API Endpoint
# This is the official SPARQL endpoint for the Swiss Federal Administration.
endpoint <- "https://ld.admin.ch/query"

# 3. Make the API Request
# We use a POST request because SPARQL queries can be very long.
# We explicitly ask the server to Accept="text/csv" because parsing a CSV 
# in R is much faster and cleaner than parsing a deeply nested JSON response.
response <- POST(
  url = endpoint,
  add_headers(Accept = "text/csv"),
  body = list(query = sparql_query),
  encode = "form"
)

# 4. Parse the Response and Clean the Data
# First, check if the server responded with "200 OK".
if (status_code(response) == 200) {
  
  # Extract the text from the response and read it directly into a tibble.
  elcom_raw <- read_csv(content(response, "text", encoding = "UTF-8"), show_col_types = FALSE)
  
  elcom_clean <- elcom_raw %>%
    # The API returns everything as text/characters. We must coerce them 
    # to numeric so our regression model can do math on them later.
    mutate(
      BFS_Nr = as.numeric(bfs_nr),
      Mean_Price_13_23 = as.numeric(Mean_Price_13_23),
      Peak_Price_2023 = as.numeric(Peak_Price_2023),
      Delta_23_13 = as.numeric(Delta_23_13)
    ) %>%
    # Keep only the correctly formatted columns.
    select(BFS_Nr, Mean_Price_13_23, Peak_Price_2023, Delta_23_13)
  
  print(paste("Successfully fetched price metrics for", nrow(elcom_clean), "municipalities."))
  
} else {
  # If the server crashes or the query is bad, stop the script and throw an error 
  # rather than silently continuing with missing data.
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

# 1. Safety Check
# Since JSON paths can be fragile, we explicitly check if the file exists 
# and stop the script with a clear error if it doesn't.
if (!file.exists(json_file_path)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", json_file_path))
}

# 2. Load the Standard JSON
elections_raw <- fromJSON(json_file_path)

# 3. Extract the Hidden Data Frame
# The BFS election JSON doesn't just load as a table; it loads as a list of 
# lists. The actual municipal voting data is buried inside the "level_gemeinden" branch.
elections_df <- elections_raw$level_gemeinden

# 4. Clean and Aggregate the Voting Shares
left_green_clean <- elections_df %>%
  mutate(
    BFS_Nr = as.numeric(gemeinde_nummer),
    Votes = as.numeric(stimmen_liste),
    Partei_ID = as.numeric(partei_id)
  ) %>%
  
  # Group by municipality to calculate totals
  group_by(BFS_Nr) %>%
  summarise(
    # First, calculate the absolute total of ALL valid votes cast in the town
    Total_Votes = sum(Votes, na.rm = TRUE),
    
    # Second, isolate and sum the votes for our specific target parties.
    # Official BFS Party IDs: SP (Social Democrats) = 3, GPS (Greens) = 13.
    # Adding GLP (Green Liberals) = 31 provides a complete picture of the eco-friendly vote.
    Left_Green_Votes = sum(Votes[Partei_ID %in% c(3, 13, 31)], na.rm = TRUE),
    .groups = "drop" # Clean up grouping
  ) %>%
  
  # 5. Calculate the Final Percentage Metric
  # Divide the target votes by the total votes to get the 0-100% share.
  mutate(Left_Green_Share_2023 = (Left_Green_Votes / Total_Votes) * 100) %>%
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
# STEP 4.6: IMPORT STADELMANN POLICY & STRUCTURAL DATA (H4)
# -------------------------------------------------------------------
# THE PURPOSE: To test Cantonal policies (subsidies, taxes, red tape) and 
# solve the "Urban Renter Paradox" by controlling for single-family homes.
# THE PROBLEM: The source .RData file is messy. It contains either a dataframe 
# or a bunch of loose column vectors floating around.

print("Loading Stadelmann Policy Data (.RData)...")

# 1. Define the specific absolute path to the .RData file
stadelmann_file <- "/mnt/truenas/1_backup hors site/Denneris/Rstudio/SwissSolarStats/data/raw/Data and R-File of the regression analysis/DataMunicipalitiesOSF.RData"

if (!file.exists(stadelmann_file)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", stadelmann_file))
}

# 2. The "Quarantine" Trick
# load() dumps objects directly into the global environment, which is dangerous.
# We create a temporary, isolated environment (stadelmann_env) to safely hold 
# the incoming data without overwriting our existing variables.
stadelmann_env <- new.env()
load(stadelmann_file, envir = stadelmann_env)

# 3. Handle the messy academic format
# We scan the quarantine environment to see if a neat dataframe exists inside.
df_objects <- Filter(function(x) is.data.frame(get(x, envir = stadelmann_env)), ls(stadelmann_env))

if (length(df_objects) > 0) {
  # If they saved a proper dataframe, grab it!
  stadelmann_raw <- get(df_objects[1], envir = stadelmann_env)
} else {
  # If they saved loose vectors instead, we manually bind them together 
  # into a structured dataframe.
  stadelmann_raw <- as.data.frame(as.list(stadelmann_env))
}

# 4. Select and clean the crucial variables
stadelmann_clean <- stadelmann_raw %>%
  mutate(BFS_Nr = as.numeric(bfs_nummer)) %>%
  select(
    BFS_Nr,
    
    # A. Structural Control (Fixes the Urban Renter Paradox)
    # Renaming 'ä' to 'ae' prevents the lm() function from crashing due to umlauts!
    Anteil_Einfamilienhaeuser = Anteil_Einfamilienhäuser, 
    
    # B. Ecosystem & Wealth Controls
    FinancialPower,
    Anteil_Waermepumpen = Anteil_Wärmepumpen,
    Anteil_Elektroautos = Anteil_Elektroautos_Personenwagenbestand,
    ANZAHL_SOLARTEURE,
    
    # C. Cantonal Policy Levers (To test H4)
    CantSubsidy = CantSubsidy01, 
    taxPV = taxPV01,
    Regulativ_Index,
    Schutzzonen = Schutzzonen01
  ) %>%
  # D. Quality Control
  # Ensure strict uniqueness for BFS_Nr to prevent duplicate row explosions 
  # when we join this to our master dataset later.
  distinct(BFS_Nr, .keep_all = TRUE)

print(paste("Stadelmann policy data prepared for", nrow(stadelmann_clean), "municipalities."))

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
  # Group by Canton as well to keep it!
  group_by(BFS_Nr, Gemeindename, Canton) %>%
  summarise(
    New_Solar_kW = sum(TotalPower, na.rm = TRUE),
    New_Installations_Count = n(),
    .groups = "drop"
  )

# 5c. Join all datasets (REMOVED complete.cases)
final_dataset <- solar_agg_commune %>%
  left_join(population_clean, by = "BFS_Nr") %>%
  left_join(elcom_clean, by = "BFS_Nr") %>%         
  left_join(green_index_clean, by = "BFS_Nr") %>%   
  left_join(left_green_clean, by = "BFS_Nr") %>%    
  left_join(irradiation_clean, by = "BFS_Nr") %>%   
  left_join(peer_effects_clean, by = "BFS_Nr") %>%  
  left_join(stadelmann_clean, by = "BFS_Nr") %>%    
  mutate(
    Baseline_PV_Density_2017 = coalesce(Baseline_PV_Density_2017, 0),
    New_Watts_per_Capita = (New_Solar_kW * 1000) / Population,
    Adoption_Intensity = (New_Installations_Count / Population) * 1000
  ) %>%
  filter(!is.na(Population)) %>%
  filter(Population > 100)

print(paste("Final dataset created with", nrow(final_dataset), "complete municipalities."))
glimpse(final_dataset)

# -------------------------------------------------------------------
# STEP 6: SAVE RESULTS
# -------------------------------------------------------------------
saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))

print("Analysis complete. Final regression dataset saved to data/processed/.")

# -------------------------------------------------------------------
# STEP 7: SAVE MASTER DATASET & GENERATE DESCRIPTIVE STATISTICS
# -------------------------------------------------------------------
# THE PURPOSE: Before running the final regressions, we permanently save the 
# clean data and generate a "Table 1" (Descriptive Statistics) to show the 
# reader the baseline characteristics (mean, min, max, std. dev) of our sample.

# 1. Save the Final Analytical Dataset
# We use saveRDS() instead of write_csv() because RDS is a native R format. 
# It perfectly preserves column types (like Dates and Factors) so you don't 
# have to re-parse them if you close and reopen RStudio tomorrow.
saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))
print("Analysis complete. Final dataset saved to data/processed/.")

print("Generating Descriptive Statistics...")

# 2. Prepare Data for Descriptives
desc_data <- final_dataset %>%
  select(
    New_Watts_per_Capita,
    Delta_23_13,
    Green_Index,
    Left_Green_Share_2023,
    Irradiation_kWh_m2,     
    Population
  ) %>%
  # *** CRITICAL FIX ***
  # The tidyverse creates "tibbles" (advanced data frames). Stargazer is an 
  # older package and absolutely hates tibbles. We must downgrade it to a 
  # classic data.frame here, otherwise Stargazer will throw a mysterious error.
  as.data.frame()

# 3. Print Descriptive Table to Console (For quick visual inspection)
stargazer(
  desc_data, 
  type = "text", 
  title = "Table 1: Descriptive Statistics",
  digits = 2,
  covariate.labels = c(
    "New PV Capacity (Watts/Capita)",
    "Price Shock (Delta 2013-2023)",
    "Green Voting Index (%)",
    "Left-Green Party Share (%)",
    "Solar Irradiation (kWh/m2)",
    "Population"
  )
)

# 4. Save Descriptive Table to Word
# By setting type = "html" but saving the file extension as ".doc", Microsoft 
# Word will automatically translate the HTML into a perfectly formatted, 
# editable Word table for your paper.
stargazer(
  desc_data, 
  type = "html", 
  out = here("data", "processed", "Table1_Descriptive_Statistics.doc"),
  title = "Table 1: Descriptive Statistics",
  digits = 2,
  covariate.labels = c(
    "New PV Capacity (Watts/Capita)",
    "Price Shock (Delta 2013-2023)",
    "Green Voting Index (%)",
    "Left-Green Party Share (%)",
    "Solar Irradiation (kWh/m2)",
    "Population"
  )
)


# -------------------------------------------------------------------
# STEP 8: THE POLICY & STRUCTURE REGRESSION MODEL
# -------------------------------------------------------------------
# THE PURPOSE: This is the ultimate mathematical test. We throw ideology, 
# economics, physical structure, and policy into one model to see which 
# variables survive when forced to compete against each other.

print("Running the Multivariate Regression Model...")

# 1. Run the Ordinary Least Squares (OLS) Regression
# NOTE: We intentionally removed ANZAHL_SOLARTEURE from the formula. 
# The Stadelmann dataset was missing installer data for ~1,500 municipalities. 
# Removing it saves our sample size and keeps N > 1,700.
model_ultimate <- lm(
  New_Watts_per_Capita ~ Delta_23_13 + Green_Index + Left_Green_Share_2023 + 
    Irradiation_kWh_m2 + Baseline_PV_Density_2017 + log(Population) +
    Anteil_Einfamilienhaeuser + FinancialPower + 
    CantSubsidy + taxPV + Regulativ_Index, 
  data = final_dataset
)

# Print the classic summary to the console to check the raw p-values and R-squared
print(summary(model_ultimate))

# 2. Prepare Data for the Coefficient Plot (Forest Plot)
# The broom::tidy() function extracts the math from the lm() object and puts 
# it into a clean table. conf.int = TRUE automatically calculates the 95% 
# Confidence Intervals needed for the error bars on the plot.
model_results <- tidy(model_ultimate, conf.int = TRUE) %>%
  # We remove the Intercept because its value is usually massive and will 
  # completely ruin the visual scale of the X-axis.
  filter(term != "(Intercept)") %>% 
  mutate(
    # Translate ugly database variable names into beautiful, publication-ready labels
    term = case_when(
      term == "Delta_23_13" ~ "Price Shock (Delta 2013-2023)",
      term == "Green_Index" ~ "Green Voting Index (Referendums)",
      term == "Left_Green_Share_2023" ~ "Eco-Left Party Share",
      term == "Irradiation_kWh_m2" ~ "Solar Irradiation (kWh/m2)",
      term == "Baseline_PV_Density_2017" ~ "Baseline PV Density 2017",
      term == "log(Population)" ~ "Population (Log)",
      term == "Anteil_Einfamilienhaeuser" ~ "Single-Family Homes (%)",
      term == "FinancialPower" ~ "Financial Power (Wealth)",
      term == "CantSubsidy" ~ "Cantonal Subsidy (Yes=1)",
      term == "taxPV" ~ "Cantonal Tax Deduction (Yes=1)",
      term == "Regulativ_Index" ~ "Regulatory Friction Index",
      TRUE ~ term
    )
  )

# 3. Render the Coefficient Forest Plot
# We use reorder(term, estimate) so the variables are sorted visually by 
# the strength of their impact, rather than just alphabetically.
coef_plot <- ggplot(model_results, aes(x = estimate, y = reorder(term, estimate))) +
  
  # A. The "Zero" Line
  # Add a vertical red dashed line at Zero. If a variable's error bar crosses 
  # this line, it means the effect is NOT statistically significant.
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  
  # B. The Data Points & Error Bars
  # Draw the 95% Confidence Intervals (the horizontal lines)
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#2c3e50", linewidth = 1) +
  # Draw the actual Point Estimate (the dot in the middle)
  geom_point(size = 4, color = "#e67e22") +
  
  # C. Apply clean, academic styling and labels
  labs(
    title = "Structural & Policy Predictors of Swiss Solar Adoption",
    subtitle = "Dependent Variable: New PV Capacity (Watts per Capita, 2018-2024)",
    x = "Estimated Effect on Solar Capacity (Watts per Capita)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

# Display the plot in the RStudio Viewer
print(coef_plot)

# 4. Generate the Academic Regression Table (Stargazer)
print("Generating Ultimate Academic Regression Table...")

# Define the exact labels for our 11 Independent Variables + 1 Constant Intercept.
# This ensures they match the formula order precisely.
table_labels <- c(
  "Price Shock (Delta 2013-2023)",
  "Green Voting Index",
  "Eco-Left Party Share",
  "Solar Irradiation (kWh/m2)",
  "Baseline PV Density 2017", 
  "Population (Log)",
  "Single-Family Homes (%)",
  "Financial Power (Wealth)",
  "Cantonal Subsidy (Yes=1)",
  "Cantonal Tax Deduction (Yes=1)",
  "Regulatory Friction Index",
  "Constant"
)

# A. Print text version to the console for quick reading
stargazer(
  model_ultimate,
  type = "text", 
  title = "Table 4: Structural & Policy Predictors of Solar Adoption",
  dep.var.labels = c("New PV Capacity (Watts/Capita)"),
  covariate.labels = table_labels,
  keep.stat = c("n", "rsq", "adj.rsq", "f"), # Outputs N, R-squared, Adj R-squared, and F-Stat
  digits = 2,
  star.cutoffs = c(0.05, 0.01, 0.001)        # Standard academic significance levels
)

# B. Save HTML version as a .doc file for Microsoft Word
stargazer(
  model_ultimate,
  type = "html", 
  out = here("data", "processed", "Table1_Regression.doc"),
  title = "Table 4: Structural & Policy Predictors of Solar Adoption",
  dep.var.labels = c("New PV Capacity (Watts/Capita)"),
  covariate.labels = table_labels,
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  digits = 2,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

print("Success! Ultimate Plot rendered and Table 1 saved.")