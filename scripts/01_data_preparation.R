# ===================================================================
# SCRIPT 01: DATA PREPARATION & INGESTION
# ===================================================================

# -------------------------------------------------------------------
# 2. BFE SOLAR INFRASTRUCTURE DATA
# -------------------------------------------------------------------
bfe_file_path <- here("data", "raw", "ElectricityProductionPlant.csv")
print(paste("Ingesting BFE infrastructure data from:", bfe_file_path))

all_plants_raw <- read_csv(bfe_file_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

solar_growth_clean <- all_plants_raw %>%
  filter(SubCategory == "subcat_2") %>% 
  mutate(operation_date = ymd(BeginningOfOperation)) %>%
  filter(operation_date >= as.Date("2018-01-01") & operation_date <= as.Date("2024-12-31")) %>%
  select(PostCode, TotalPower)

print(paste("Temporal filtering complete. New PV installations in study period:", nrow(solar_growth_clean)))

# -------------------------------------------------------------------
# 3. SPATIAL RESOLUTION (SWISSTOPO)
# -------------------------------------------------------------------
swisstopo_file_path <- here("data", "raw", "AMTOVZ_CSV_LV95.csv")
print("Ingesting Swisstopo geospatial lookup table...")

ortschaften_raw <- read_delim(swisstopo_file_path, delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

ortschaften_lookup <- ortschaften_raw %>%
  select(PLZ, Ortschaftsname, Gemeindename, `BFS-Nr`, Zusatzziffer, contains("Kanton")) %>% 
  rename(BFS_Nr = `BFS-Nr`, Canton = contains("Kanton")) %>%
  mutate(Is_Main_Commune = (Ortschaftsname == Gemeindename)) %>%
  arrange(PLZ, Zusatzziffer, desc(Is_Main_Commune)) %>%
  distinct(PLZ, .keep_all = TRUE) %>% 
  select(PLZ, Gemeindename, BFS_Nr, Canton)

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
# 4.1 ELCOM TARIFF DATA VIA LINDAS API
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
  elcom_raw <- read_csv(content(response, "text", encoding = "UTF-8"), show_col_types = FALSE)
  elcom_clean <- elcom_raw %>%
    mutate(across(everything(), ~ str_remove_all(., '\\^\\^<.*>'))) %>%
    mutate(across(everything(), ~ str_remove_all(., '"'))) %>%
    mutate(
      BFS_Nr = as.numeric(bfs_nr),
      Operator_Name = as.character(Operator_Name),
      Mean_Price_13_23 = as.numeric(Mean_Price_13_23),
      Peak_Price_2023 = as.numeric(Peak_Price_2023),
      Delta_23_13 = as.numeric(Delta_23_13)
    ) %>%
    mutate(
      Peak_Price_2023 = ifelse(Peak_Price_2023 == 0, NA, Peak_Price_2023),
      Delta_23_13 = ifelse(Peak_Price_2023 == 0 | Delta_23_13 == 0, NA, Delta_23_13)
    ) %>%
    select(BFS_Nr, Operator_Name, Mean_Price_13_23, Peak_Price_2023, Delta_23_13)
} else {
  stop(paste("CRITICAL ERROR: Failed to fetch data from LINDAS API. HTTP Status:", status_code(response)))
}

# -------------------------------------------------------------------
# 4.2 FEDERAL REFERENDUM DATA
# -------------------------------------------------------------------
clean_vote_file <- function(file_name, vote_col_name) {
  raw_data <- read_excel(here("data", "raw", file_name))
  clean_data <- raw_data %>%
    rename(Area_Code = 1, Area_Name = 2, Vote_ID = 3, Vote_Date = 4, Yes_Percent = 5) %>%
    filter(str_detect(Area_Name, "^\\.\\.\\.\\.\\.\\.")) %>%
    mutate(
      BFS_Nr = as.numeric(Area_Code),
      Yes_Percent = as.numeric(str_replace(as.character(Yes_Percent), ",", "."))
    ) %>%
    select(BFS_Nr, Yes_Percent) %>%
    rename(!!vote_col_name := Yes_Percent)
  return(clean_data)
}

vote_2017 <- clean_vote_file("2017Energy.Act_Outcome_YESSHARE.xlsx", "Yes_2017")
vote_2021 <- clean_vote_file("2021CO2.Act_Outcome_YESSHARE.xlsx", "Yes_2021")
vote_2024 <- clean_vote_file("2024Climate.Protection.Act_Outcome_YESSHARE.xlsx", "Yes_2024")

green_index_clean <- vote_2017 %>%
  full_join(vote_2021, by = "BFS_Nr") %>%
  full_join(vote_2024, by = "BFS_Nr") %>%
  rowwise() %>%
  mutate(Green_Index = mean(c_across(starts_with("Yes_")), na.rm = TRUE)) %>%
  ungroup() %>% 
  select(BFS_Nr, Yes_2017, Yes_2021, Yes_2024, Green_Index)

# -------------------------------------------------------------------
# 4.3 NATIONAL COUNCIL ELECTIONS
# -------------------------------------------------------------------
json_file_path <- here("data", "raw", "NRW_2023_Dataset.json")
elections_raw <- fromJSON(json_file_path)
elections_df <- elections_raw$level_gemeinden

left_green_clean <- elections_df %>%
  mutate(BFS_Nr = as.numeric(gemeinde_nummer), Votes = as.numeric(stimmen_liste), Partei_ID = as.numeric(partei_id)) %>%
  group_by(BFS_Nr) %>%
  summarise(
    Total_Votes = sum(Votes, na.rm = TRUE),
    Left_Green_Votes = sum(Votes[Partei_ID %in% c(3, 13, 31)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Left_Green_Share_2023 = (Left_Green_Votes / Total_Votes) * 100,
    Left_Green_Share_2023 = na_if(Left_Green_Share_2023, 0)
  ) %>%
  select(BFS_Nr, Left_Green_Share_2023)

# -------------------------------------------------------------------
# 4.4 SOLAR IRRADIATION
# -------------------------------------------------------------------
irradiation_file <- here("data", "raw", "solar_radiation_per_municipality.xlsx")
irradiation_raw <- read_excel(irradiation_file)

irradiation_clean <- irradiation_raw %>%
  mutate(BFS_Nr = as.numeric(bfs_nummer), Irradiation_kWh_m2 = as.numeric(radiation_kWh_m2)) %>%
  select(BFS_Nr, Irradiation_kWh_m2)

# -------------------------------------------------------------------
# 4.5 PEER EFFECTS (BASELINE PV DENSITY < 2018)
# -------------------------------------------------------------------
baseline_plants <- all_plants_raw %>%
  mutate(Commissioning_Date = as.Date(BeginningOfOperation), PostCode = as.numeric(PostCode)) %>%
  filter(Commissioning_Date < as.Date("2018-01-01")) %>%
  left_join(ortschaften_lookup, by = c("PostCode" = "PLZ")) %>%
  filter(!is.na(BFS_Nr))

baseline_capacity <- baseline_plants %>%
  group_by(BFS_Nr) %>%
  summarise(Baseline_Total_Watts_2017 = sum(TotalPower, na.rm = TRUE) * 1000, .groups = "drop")

peer_effects_clean <- baseline_capacity %>%
  left_join(population_clean, by = "BFS_Nr") %>%
  mutate(Baseline_PV_Density_2017 = Baseline_Total_Watts_2017 / Population) %>%
  select(BFS_Nr, Baseline_PV_Density_2017)

# -------------------------------------------------------------------
# 4.6 FEDERAL TAX DATA
# -------------------------------------------------------------------
wealth_file_path <- here("data", "raw", "27598_DE.csv")
wealth_raw <- read_delim(wealth_file_path, delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

wealth_clean <- wealth_raw %>%
  filter(!str_detect(VARIABLE, "Mio")) %>%
  select(BFS_Nr = GEO_ID, Gemeindename_ESTV = GEO_NAME, Taxable_Income = VALUE) %>%
  mutate(BFS_Nr = as.numeric(BFS_Nr), Taxable_Income = as.numeric(Taxable_Income)) %>%
  filter(!is.na(BFS_Nr))

# -------------------------------------------------------------------
# 4.7 POPULATION DENSITY
# -------------------------------------------------------------------
density_file_path <- here("data", "raw", "population_density_2018_2023(in).csv")
density_raw <- read_csv(density_file_path, show_col_types = FALSE)

density_clean <- density_raw %>%
  mutate(BFS_Nr = as.numeric(bfs_nummer)) %>%
  group_by(BFS_Nr) %>%
  summarise(Population_Density = mean(population_density, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(BFS_Nr))

# -------------------------------------------------------------------
# 4.8 BUILDING STRUCTURE DATA
# -------------------------------------------------------------------
sfh_file_path <- here("data", "raw", "CH1.GWS,DF_GWS_REG1,1.0.0+all.csv")
sfh_raw <- read_delim(sfh_file_path, delim = ",", col_select = c(GEMEINDENAME, TIME_PERIOD, `Building category`, OBS_VALUE), show_col_types = FALSE)

sfh_clean <- sfh_raw %>%
  filter(TIME_PERIOD == 2021) %>%
  mutate(BFS_Nr = as.numeric(GEMEINDENAME)) %>%
  filter(!is.na(BFS_Nr)) %>%
  group_by(BFS_Nr) %>%
  summarise(
    Total_Residential = sum(OBS_VALUE, na.rm = TRUE),
    SFH_Count = sum(OBS_VALUE[str_detect(`Building category`, "Single-family house")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Share_SFH = (SFH_Count / Total_Residential) * 100, Share_SFH = coalesce(Share_SFH, 0)) %>%
  select(BFS_Nr, Share_SFH)

print("Data preparation complete. All dataframes reside in the global environment.")