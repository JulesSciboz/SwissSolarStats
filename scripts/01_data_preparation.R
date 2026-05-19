# ===================================================================
# SCRIPT 01: DATA PREPARATION & INGESTION
# ===================================================================

# -------------------------------------------------------------------
# 1. BFE SOLAR INFRASTRUCTURE
# -------------------------------------------------------------------
bfe_file_path <- here("data", "raw", "ElectricityProductionPlant.csv")
print(paste("Ingesting BFE infrastructure data from:", bfe_file_path))

all_plants_raw <- read_csv(bfe_file_path, 
                           col_select = c(SubCategory, BeginningOfOperation, PostCode, TotalPower),
                           locale = locale(encoding = "UTF-8"), 
                           show_col_types = FALSE)

solar_growth_clean <- all_plants_raw %>%
  filter(SubCategory == "subcat_2") %>% 
  mutate(operation_date = ymd(BeginningOfOperation)) %>%
  filter(operation_date >= as.Date("2018-01-01") & operation_date <= as.Date("2024-12-31")) %>%
  select(PostCode, TotalPower)

print(paste("Temporal filtering complete. New PV installations in study period:", nrow(solar_growth_clean)))

# -------------------------------------------------------------------
# 2. SPATIAL RESOLUTION (Swisstopo)
# -------------------------------------------------------------------
print("Ingesting Swisstopo geospatial lookup table...")
ortschaften_lookup <- read_delim(here("data", "raw", "AMTOVZ_CSV_LV95.csv"), 
                                 delim = ";", locale = locale(encoding = "UTF-8"), 
                                 show_col_types = FALSE) %>%
  select(PLZ, Ortschaftsname, Gemeindename, BFS_Nr = `BFS-Nr`, Zusatzziffer, contains("Kanton")) %>% 
  rename(Canton = contains("Kanton")) %>%
  mutate(Is_Main_Commune = (Ortschaftsname == Gemeindename)) %>%
  arrange(PLZ, Zusatzziffer, desc(Is_Main_Commune)) %>%
  distinct(PLZ, .keep_all = TRUE) %>% 
  select(PLZ, Gemeindename, BFS_Nr, Canton)

# -------------------------------------------------------------------
# 3. ELCOM TARIFF DATA (LINDAS API)
# -------------------------------------------------------------------
print("Executing LINDAS API SPARQL query for ElCom tariff data...")

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

endpoint <- "https://ld.admin.ch/query"
response <- POST(url = endpoint, add_headers(Accept = "text/csv"), body = list(query = sparql_query), encode = "form")

if (status_code(response) == 200) {
  elcom_clean <- read_csv(content(response, "text", encoding = "UTF-8"), show_col_types = FALSE) %>%
    mutate(across(everything(), ~ str_remove_all(., '\"|\\^\\^<.*>'))) %>%
    mutate(across(c(bfs_nr, Mean_Price_13_23, Peak_Price_2023, Delta_23_13), as.numeric)) %>%
    rename(BFS_Nr = bfs_nr) %>%
    mutate(Peak_Price_2023 = na_if(Peak_Price_2023, 0)) %>%
    select(BFS_Nr, Operator_Name, Mean_Price_13_23, Peak_Price_2023)
}

# -------------------------------------------------------------------
# 4. DEMOGRAPHIC DATA (JSON)
# -------------------------------------------------------------------
json_file_path <- here("data", "raw", "px-x-0102020000_201.json")
print("Ingesting and parsing Federal Population JSON...")

json_data <- fromJSON(json_file_path)
geo_dim_key <- "Kanton (-) / Bezirk (>>) / Gemeinde (......)"
geo_labels <- json_data$dataset$dimension[[geo_dim_key]]$category$label
values_list <- json_data$dataset$value

pop_raw <- data.frame(Label = unlist(geo_labels), Population = values_list, stringsAsFactors = FALSE)

population_clean <- pop_raw %>%
  filter(str_starts(Label, "\\.\\.\\.\\.\\.\\.")) %>% 
  mutate(
    BFS_Nr = as.numeric(str_extract(Label, "(?<=\\.\\.\\.\\.\\.\\.)\\d{4}")),
    Commune_Name_Pop = str_trim(str_remove(Label, "\\.\\.\\.\\.\\.\\.\\d{4} "))
  ) %>%
  select(BFS_Nr, Population)

# -------------------------------------------------------------------
# 4. POLITICAL & ENVIRONMENTAL DRIVERS (H3 & Controls)
# -------------------------------------------------------------------
# National Council Elections (H3: Left-Green Share)
elections_df <- fromJSON(here("data", "raw", "NRW_2023_Dataset.json"))$level_gemeinden

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
  mutate(Left_Green_Share_2023 = na_if((Left_Green_Votes / Total_Votes) * 100, 0)) %>%
  select(BFS_Nr, Left_Green_Share_2023)

# -------------------------------------------------------------------
# 4.1 SOLAR IRRADIATION
# -------------------------------------------------------------------
# Solar Irradiation (Geographical Control)
irradiation_clean <- read_excel(here("data", "raw", "solar_radiation_per_municipality.xlsx")) %>%
  mutate(
    BFS_Nr = as.numeric(bfs_nummer), 
    Irradiation_kWh_m2 = as.numeric(radiation_kWh_m2)
  ) %>%
  select(BFS_Nr, Irradiation_kWh_m2)

# -------------------------------------------------------------------
# 5. STRUCTURAL CONTROLS (Housing, Wealth, Density, Peer Effects)
# -------------------------------------------------------------------
# Building Structure (SFH Share)
sfh_clean <- read_delim(here("data", "raw", "CH1.GWS,DF_GWS_REG1,1.0.0+all.csv"), 
                        delim = ",", col_select = c(GEMEINDENAME, TIME_PERIOD, `Building category`, OBS_VALUE), show_col_types = FALSE) %>%
  filter(TIME_PERIOD == 2021) %>%
  mutate(BFS_Nr = as.numeric(GEMEINDENAME)) %>%
  group_by(BFS_Nr) %>%
  summarise(Total_Res = sum(OBS_VALUE, na.rm = TRUE),
            SFH_Count = sum(OBS_VALUE[str_detect(`Building category`, "Single-family house")], na.rm = TRUE), .groups = "drop") %>%
  mutate(Share_SFH = (SFH_Count / Total_Res) * 100) %>% select(BFS_Nr, Share_SFH)

# Peer Effects (H2 Baseline)
peer_effects_clean <- all_plants_raw %>%
  mutate(Commissioning_Date = as.Date(BeginningOfOperation)) %>%
  filter(Commissioning_Date < as.Date("2018-01-01")) %>%
  left_join(ortschaften_lookup, by = c("PostCode" = "PLZ")) %>%
  filter(!is.na(BFS_Nr)) %>%
  group_by(BFS_Nr) %>%
  summarise(Baseline_Total_Watts_2017 = sum(TotalPower, na.rm = TRUE) * 1000, .groups = "drop")

# Wealth & Density
wealth_clean <- read_delim(here("data", "raw", "27598_DE.csv"), delim = ";", show_col_types = FALSE) %>%
  filter(!str_detect(VARIABLE, "Mio")) %>%
  transmute(BFS_Nr = as.numeric(GEO_ID), Taxable_Income = as.numeric(VALUE))

density_clean <- read_csv(here("data", "raw", "population_density_2018_2023(in).csv"), show_col_types = FALSE) %>%
  group_by(BFS_Nr = as.numeric(bfs_nummer)) %>%
  summarise(Population_Density = mean(population_density, na.rm = TRUE), .groups = "drop")

print("Fully restored and optimized Data preparation complete.")