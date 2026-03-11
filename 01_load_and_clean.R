# ===================================================================
# STEP 0: SETUP PROJECT ENVIRONMENT
# ===================================================================

# Check Git status and GitHub remote connection
print("Checking local Git status and GitHub remote link...")
system("git remote -v") 
system("git status")

# -------------------------------------------------------------------
# STEP 1: LOAD LIBRARIES
# -------------------------------------------------------------------

if(!require("readr")) install.packages("readr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("lubridate")) install.packages("lubridate")
if(!require("jsonlite")) install.packages("jsonlite")
if(!require("stringr")) install.packages("stringr")
if(!require("here")) install.packages("here") # NEW: For relative file paths
if(!require("httr")) install.packages("httr")
if(!require("ggplot2")) install.packages("ggplot2")

library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(stringr)
library(httr)
library(here) # Initialize here

print(paste("Project root automatically set to:", here()))

# Auto-create necessary folders if they don't exist
dirs_to_create <- c(
  here("data"),
  here("data", "raw"),
  here("data", "processed"),
  here("plots")
)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir)
    print(paste("Created missing directory:", dir))
  }
}

# -------------------------------------------------------------------
# STEP 2: IMPORT BFE SOLAR DATA
# -------------------------------------------------------------------
bfe_file_path <- here("data", "raw", "ElectricityProductionPlant.csv")

print(paste("Reading BFE file from:", bfe_file_path))
all_plants_raw <- read_csv(bfe_file_path, locale = locale(encoding = "UTF-8"))

# Clean & filter for Solar GROWTH (2018-2024)
solar_growth_clean <- all_plants_raw %>%
  filter(SubCategory == "subcat_2") %>% # Filter for Photovoltaic only [cite: 234, 235]
  mutate(operation_date = ymd(BeginningOfOperation)) %>%
  # *** CRITICAL FILTER: Start of 2018 to End of 2024 ***
  filter(operation_date >= "2018-01-01" & operation_date <= "2024-12-31") %>%
  select(PostCode, TotalPower)

print(paste("Solar data loaded. Installations in period:", nrow(solar_growth_clean)))

# -------------------------------------------------------------------
# STEP 3: IMPORT SWISSTOPO (FIXED: PRIORITIZE OFFICIAL INDEX)
# -------------------------------------------------------------------
swisstopo_file_path <- here("data", "raw", "AMTOVZ_CSV_LV95.csv")

print("Reading Swisstopo lookup file...")
ortschaften_raw <- read_delim(swisstopo_file_path, delim = ";", locale = locale(encoding = "UTF-8"))

# FIX: Refined Logic for Shared PLZs
ortschaften_lookup <- ortschaften_raw %>%
  select(PLZ, Ortschaftsname, Gemeindename, `BFS-Nr`, Zusatzziffer, contains("Kanton")) %>% 
  rename(BFS_Nr = `BFS-Nr`, Canton = contains("Kanton")) %>%
  
  # Create Priority Column (for tie-breaking)
  mutate(Is_Main_Commune = (Ortschaftsname == Gemeindename)) %>%
  
  # *** THE FIX IS HERE ***
  # 1. Zusatzziffer (Ascending): Trust the official "Main" status first.
  #    (Fixes Champoz: Valbirse [2] beats Champoz [3])
  # 2. Is_Main_Commune (Descending): If indices are tied, use name match.
  #    (Fixes Mont-Tramelan: Mont-Tramelan [0] beats Tramelan [0])
  arrange(PLZ, Zusatzziffer, desc(Is_Main_Commune)) %>%
  
  # Lock in the single best match
  distinct(PLZ, .keep_all = TRUE) %>% 
  
  select(PLZ, Gemeindename, BFS_Nr, Canton)

print("Swisstopo lookup created (Logic: Index > Name Match).")


# -------------------------------------------------------------------
# STEP 4: IMPORT POPULATION DATA (JSON)
# -------------------------------------------------------------------
json_file_path <- here("data", "raw", "px-x-0102020000_201.json")

print("Reading Population JSON...")
json_data <- fromJSON(json_file_path)

geo_dim_key <- "Kanton (-) / Bezirk (>>) / Gemeinde (......)"
geo_labels <- json_data$dataset$dimension[[geo_dim_key]]$category$label
values_list <- json_data$dataset$value

pop_raw <- data.frame(
  Label = unlist(geo_labels),
  Population = values_list,
  stringsAsFactors = FALSE
)

population_clean <- pop_raw %>%
  filter(str_starts(Label, "\\.\\.\\.\\.\\.\\.")) %>% 
  mutate(
    BFS_Nr = as.numeric(str_extract(Label, "(?<=\\.\\.\\.\\.\\.\\.)\\d{4}")),
    Commune_Name_Pop = str_trim(str_remove(Label, "\\.\\.\\.\\.\\.\\.\\d{4} "))
  ) %>%
  select(BFS_Nr, Population)

print("Population data extracted.")

# -------------------------------------------------------------------
# STEP 4.5: FETCH ELCOM ELECTRICITY PRICES VIA LINDAS API (H1.A)
# -------------------------------------------------------------------
print("Fetching ElCom Historical Electricity Prices (2013-2023) via LINDAS API...")

# Define the SPARQL query
# Note: Text names for operators are removed to ensure strictly 1 row per BFS_Nr
sparql_query <- '
PREFIX schema: <http://schema.org/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX cube: <https://cube.link/>
PREFIX dim: <https://energy.ld.admin.ch/elcom/electricityprice/dimension/>

SELECT ?bfs_nr 
       (AVG(?total_price) AS ?Mean_Price_13_23)
       (MAX(?price_2023) AS ?Peak_Price_2023)
       ((MAX(?price_2023) - MAX(?price_2013)) AS ?Delta_23_13)
WHERE {
  <https://energy.ld.admin.ch/elcom/electricityprice> cube:observationSet/cube:observation ?obs .
  
  ?obs dim:period ?period ;
       dim:municipality ?municipality ;
       dim:category <https://energy.ld.admin.ch/elcom/electricityprice/category/H4> ;
       dim:product <https://energy.ld.admin.ch/elcom/electricityprice/product/standard> ;
       dim:total ?total_price .
       
  ?municipality schema:identifier ?bfs_nr .
  
  FILTER(str(?period) >= "2013" && str(?period) <= "2023")
  
  BIND(IF(str(?period) = "2023", ?total_price, 0) AS ?price_2023)
  BIND(IF(str(?period) = "2013", ?total_price, 0) AS ?price_2013)
}
GROUP BY ?bfs_nr
'

# The official LINDAS cached endpoint
endpoint <- "https://ld.admin.ch/query"

# Make the POST request
response <- POST(
  url = endpoint,
  add_headers(Accept = "text/csv"),
  body = list(query = sparql_query),
  encode = "form"
)

# Parse the response directly into our dataframe
if (status_code(response) == 200) {
  elcom_raw <- read_csv(content(response, "text", encoding = "UTF-8"), show_col_types = FALSE)
  
  elcom_clean <- elcom_raw %>%
    mutate(
      BFS_Nr = as.numeric(bfs_nr),
      Mean_Price_13_23 = as.numeric(Mean_Price_13_23),
      Peak_Price_2023 = as.numeric(Peak_Price_2023),
      Delta_23_13 = as.numeric(Delta_23_13)
    ) %>%
    select(BFS_Nr, Mean_Price_13_23, Peak_Price_2023, Delta_23_13)
  
  print(paste("Successfully fetched price metrics for", nrow(elcom_clean), "municipalities."))
} else {
  stop(paste("Failed to fetch data from LINDAS API. Status code:", status_code(response)))
}

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

# 5c. Join with Population, ElCom Data & Calculate Metrics
final_dataset <- solar_agg_commune %>%
  left_join(population_clean, by = "BFS_Nr") %>%
  left_join(elcom_clean, by = "BFS_Nr") %>%   # <--- ADDED: ElCom Electricity Prices (H1.A)
  mutate(
    # DV 1: New Capacity Density (Watts per Capita)
    New_Watts_per_Capita = (New_Solar_kW * 1000) / Population,
    
    # DV 2: Adoption Intensity (Installations per 1,000 inhabitants)
    Adoption_Intensity = (New_Installations_Count / Population) * 1000
  ) %>%
  filter(!is.na(Population)) %>%
  # Optional: Filter out tiny communes to avoid outliers (e.g., Pop < 100)
  filter(Population > 100)

print("Final dataset created.")
glimpse(final_dataset)

# -------------------------------------------------------------------
# STEP 6: ANALYSIS - THE NEW RANKINGS
# -------------------------------------------------------------------

# RANKING 1: By New Capacity Density (The "Power" Leaders)
# sorting by: New_Watts_per_Capita
top_capacity <- final_dataset %>%
  arrange(desc(New_Watts_per_Capita)) %>%
  select(Gemeindename, Canton, Population, New_Watts_per_Capita, Adoption_Intensity)

print("--- TOP 20: NEW CAPACITY DENSITY (Watts/Capita) ---")
print(head(top_capacity, 20))


# RANKING 2: By Adoption Intensity (The "Frequency" Leaders)
# sorting by: Adoption_Intensity
top_intensity <- final_dataset %>%
  arrange(desc(Adoption_Intensity)) %>%
  select(Gemeindename, Canton, Population, Adoption_Intensity, New_Watts_per_Capita)

print("--- TOP 20: ADOPTION INTENSITY (Installations/1000 ppl) ---")
print(head(top_intensity, 20))

# -------------------------------------------------------------------
# STEP 7: SAVE RESULTS
# -------------------------------------------------------------------
saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))
write_csv(top_capacity, here("data", "processed", "ranking_by_capacity.csv"))
write_csv(top_intensity, here("data", "processed", "ranking_by_intensity.csv"))

print("Analysis complete. Both rankings saved to data/processed/.")


# -------------------------------------------------------------------
# STEP 8: TEMPORARY VISUALIZATION (DATA SANITY CHECK)
# -------------------------------------------------------------------
print("Generating temporary scatter plot to visually check H1.A...")

# Temporary Scatter Plot: Price Shock (Delta) vs PV Adoption
temp_plot <- ggplot(final_dataset, aes(x = Delta_23_13, y = New_Watts_per_Capita)) +
  geom_point(alpha = 0.4, color = "#4682B4") + # Semi-transparent points for overlapping communes
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Red trend line
  labs(
    title = "Sanity Check: Electricity Price Shock vs. PV Adoption",
    subtitle = "Does a higher electricity price increase (2013-2023) lead to more solar?",
    x = "Price Shock (Delta 2023 - 2013 in Rp./kWh)",
    y = "New PV Capacity (Watts per Capita)"
  ) +
  theme_minimal()

# Display the plot in the RStudio Viewer
print(temp_plot)

# Note: STEP 9 (README Auto-Update) has been permanently removed as requested.