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
if(!require("here")) install.packages("here")
if(!require("httr")) install.packages("httr")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("readxl")) install.packages("readxl")

library(readxl)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(stringr)
library(httr)
library(here)

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
# STEP 4.7: IMPORT BFS VOTING DATA & BUILD GREEN INDEX (H2)
# -------------------------------------------------------------------

print("Reading 3 BFS Voting Excel files and building Green Index...")

# Funktion zum Einlesen und Säubern einer einzelnen Abstimmungsdatei
clean_vote_file <- function(file_name, vote_col_name) {
  raw_data <- read_excel(here("data", "raw", file_name))
  
  clean_data <- raw_data %>%
    rename(
      Area_Code = 1,
      Area_Name = 2,
      Vote_ID = 3,
      Vote_Date = 4,
      Yes_Percent = 5
    ) %>%
    # Nur Gemeinden behalten (anhand der 6 Punkte "......" erkennen)
    filter(str_detect(Area_Name, "^\\.\\.\\.\\.\\.\\.")) %>%
    mutate(
      BFS_Nr = as.numeric(Area_Code),
      # Kommas in Punkte umwandeln, falls Excel sie als Text importiert hat
      Yes_Percent = as.numeric(str_replace(as.character(Yes_Percent), ",", "."))
    ) %>%
    select(BFS_Nr, Yes_Percent) %>%
    rename(!!vote_col_name := Yes_Percent)
  
  return(clean_data)
}

# Wende die Funktion auf deine 3 Excel-Dateien an
vote_2017 <- clean_vote_file("2017Energy.Act_Outcome_YESSHARE.xlsx", "Yes_2017")
vote_2021 <- clean_vote_file("2021CO2.Act_Outcome_YESSHARE.xlsx", "Yes_2021")
vote_2024 <- clean_vote_file("2024Climate.Protection.Act_Outcome_YESSHARE.xlsx", "Yes_2024")

# Füge die 3 Abstimmungen zusammen und berechne den Durchschnitt (Green Index)
green_index_clean <- vote_2017 %>%
  full_join(vote_2021, by = "BFS_Nr") %>%
  full_join(vote_2024, by = "BFS_Nr") %>%
  rowwise() %>%
  mutate(Green_Index = mean(c_across(starts_with("Yes_")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(BFS_Nr, Yes_2017, Yes_2021, Yes_2024, Green_Index)

print(paste("Green Index successfully calculated for", nrow(green_index_clean), "municipalities."))

# -------------------------------------------------------------------
# STEP 4.8: IMPORT NATIONAL COUNCIL ELECTIONS (H3: Left-Green Strength)
# -------------------------------------------------------------------
print("Reading 2023 National Council Election JSON...")

json_file_path <- here("data", "raw", "NRW_2023_Dataset.json")

# 1. Sicherheitscheck
if (!file.exists(json_file_path)) {
  stop(paste("FEHLER: Datei nicht gefunden unter", json_file_path))
}

# 2. Standard-JSON einlesen
elections_raw <- fromJSON(json_file_path)

# 3. DEN VERSTECKTEN DATENSATZ EXTRAHIEREN!
# Hier sagen wir R explizit, dass es die Tabelle aus "level_gemeinden" nehmen soll
elections_df <- elections_raw$level_gemeinden

# 4. Daten bereinigen & aggregieren
left_green_clean <- elections_df %>%
  mutate(
    BFS_Nr = as.numeric(gemeinde_nummer),
    Votes = as.numeric(stimmen_liste),
    Partei_ID = as.numeric(partei_id)
  ) %>%
  group_by(BFS_Nr) %>%
  summarise(
    Total_Votes = sum(Votes, na.rm = TRUE),
    
    # BFS Standard-IDs: SP = 3, GPS = 13
    Left_Green_Votes = sum(Votes[Partei_ID %in% c(3, 13)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Berechne den prozentualen Anteil (0 bis 100%)
  mutate(Left_Green_Share_2023 = (Left_Green_Votes / Total_Votes) * 100) %>%
  select(BFS_Nr, Left_Green_Share_2023)

print(paste("Left-Green voting share calculated for", nrow(left_green_clean), "municipalities."))

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
  left_join(elcom_clean, by = "BFS_Nr") %>%   # <--- h1a
  left_join(green_index_clean, by = "BFS_Nr") %>% # <--- h2a
  left_join(left_green_clean, by = "BFS_Nr") %>%  # <--- h3a
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
# STEP 6: SAVE RESULTS
# -------------------------------------------------------------------
saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))

print("Analysis complete. Final regression dataset saved to data/processed/.")

# -------------------------------------------------------------------
# STEP 7: SAVE FINAL REGRESSION DATASET
# -------------------------------------------------------------------
saveRDS(final_dataset, here("data", "processed", "solar_growth_2018_2024_final.rds"))
print("Analysis complete. Final dataset saved to data/processed/.")


# -------------------------------------------------------------------
# STEP 8: MULTIVARIATE REGRESSION & COEFFICIENT PLOT
# -------------------------------------------------------------------
if(!require("broom")) install.packages("broom")
library(broom)

print("Running Multivariate Regression Model...")

# 1. Run the OLS Regression
# DV: New Watts per Capita
# IVs: Price Shock (H1), Green Index (H2), Left-Green Party Share (H3), Control: log(Population)
model_1 <- lm(
  New_Watts_per_Capita ~ Delta_23_13 + Green_Index + Left_Green_Share_2023 + log(Population), 
  data = final_dataset
)

# Print the classic summary to the console so you can see the p-values
print(summary(model_1))

# 2. Prepare data for the Coefficient Plot using 'broom'
# This extracts the estimates and 95% confidence intervals
model_results <- tidy(model_1, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% # We remove the intercept as it skews the plot scale
  mutate(
    # Rename variables for a beautiful plot
    term = case_when(
      term == "Delta_23_13" ~ "Price Shock (Delta 2013-2023)",
      term == "Green_Index" ~ "Green Voting Index (Referendums)",
      term == "Left_Green_Share_2023" ~ "Left-Green Party Share (Elections)",
      term == "log(Population)" ~ "Population (Log)",
      TRUE ~ term
    )
  )

# 3. Create the Coefficient Forest Plot
coef_plot <- ggplot(model_results, aes(x = estimate, y = reorder(term, estimate))) +
  # Add a vertical red line at Zero (the "No Effect" line)
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  
  # Add the estimates and confidence intervals
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#2c3e50", linewidth = 1) +
  geom_point(size = 4, color = "#e67e22") +
  
  # Labels and styling
  labs(
    title = "Predictors of Solar Adoption in Swiss Municipalities",
    subtitle = "Dependent Variable: New PV Capacity (Watts per Capita) | 95% Confidence Intervals",
    x = "Estimated Effect on Solar Capacity (Watts per Capita)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

# 4. Display the plot
print(coef_plot)

# -------------------------------------------------------------------
# STEP 9: ACADEMIC REGRESSION TABLE
# -------------------------------------------------------------------
if(!require("stargazer")) install.packages("stargazer")
library(stargazer)

print("Generating standard academic regression table...")

# 1. Print to the Console (for quick reading)
stargazer(
  model_1,
  type = "text", 
  title = "Table 1: Predictors of Solar Adoption (2018-2024)",
  dep.var.labels = c("New PV Capacity (Watts/Capita)"),
  covariate.labels = c(
    "Price Shock (Delta 2013-2023)",
    "Green Voting Index",
    "Left-Green Party Share",
    "Population (Log)",
    "Constant"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f"), # Shows N, R-squared, and F-statistic
  digits = 2,
  star.cutoffs = c(0.05, 0.01, 0.001) # Standard significance stars (* p<0.05, ** p<0.01, *** p<0.001)
)

# 2. Save to a Word-compatible document (for your paper)
stargazer(
  model_1,
  type = "html", # Saving as HTML inside a .doc file makes it open perfectly in MS Word
  out = here("data", "processed", "Table1_Regression_Results.doc"),
  title = "Table 1: Predictors of Solar Adoption (2018-2024)",
  dep.var.labels = c("New PV Capacity (Watts/Capita)"),
  covariate.labels = c(
    "Price Shock (Delta 2013-2023)",
    "Green Voting Index",
    "Left-Green Party Share",
    "Population (Log)",
    "Constant"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  digits = 2,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

print("Success! Academic table printed to console and saved to data/processed/Table1_Regression_Results.doc")

