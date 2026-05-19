# ===================================================================
# SCRIPT 02: RELATIONAL DATA MERGE & SERIALIZATION
# ===================================================================
# Methodological Note: Integrates the disparate spatial, demographic, 
# economic, and political indices into a unified cross-sectional 
# analytical dataframe via deterministic spatial joins (BFS_Nr).

print("Executing relational data merge to construct Master Panel...")

# -------------------------------------------------------------------
# 1. SPATIAL INTEGRATION OF DEPENDENT VARIABLE
# -------------------------------------------------------------------
solar_mapped <- solar_growth_clean %>%
  mutate(PostCode = as.numeric(PostCode)) %>% 
  # Map postal codes to administrative municipal identifiers
  left_join(ortschaften_lookup, by = c("PostCode" = "PLZ")) %>%
  filter(!is.na(BFS_Nr))

# Municipal Aggregation
solar_agg_commune <- solar_mapped %>%
  group_by(BFS_Nr, Gemeindename, Canton) %>%
  summarise(
    # Aggregate absolute capacity additions for the 2018-2024 window
    New_Solar_kW = sum(TotalPower, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 2. CONSTRUCTING THE ANALYTICAL MASTER PANEL
# -------------------------------------------------------------------
final_dataset <- solar_agg_commune %>%
  # Execute sequential left-joins to bind all structural predictors and controls
  left_join(population_clean, by = "BFS_Nr") %>%
  left_join(elcom_clean, by = "BFS_Nr") %>%          
  left_join(left_green_clean, by = "BFS_Nr") %>%    
  left_join(irradiation_clean, by = "BFS_Nr") %>%   
  left_join(peer_effects_clean, by = "BFS_Nr") %>%  
  left_join(wealth_clean, by = "BFS_Nr") %>%        
  left_join(density_clean, by = "BFS_Nr") %>%        
  left_join(sfh_clean, by = "BFS_Nr") %>%
  
  mutate(
    # FIX: Handle structural zeros for raw historical watts first
    Baseline_Total_Watts_2017 = coalesce(Baseline_Total_Watts_2017, 0),
        Baseline_PV_Density_2017 = Baseline_Total_Watts_2017 / Population,
    
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
# 3. DATA SERIALIZATION
# -------------------------------------------------------------------
# Methodological Note: The finalized dataframe is serialized as an RDS object 
# to perfectly preserve factor levels, date formats, and numeric classes for 
# subsequent econometric modeling.

save_path <- here("data", "processed", "solar_growth_2018_2024_final.rds")
saveRDS(final_dataset, save_path)

print(paste("Data processing pipeline complete. Analytical dataset serialized to:", save_path))

# -------------------------------------------------------------------
# OPTIONAL: CLEAN UP ENVIRONMENT
# -------------------------------------------------------------------
rm(list = setdiff(ls(), c("final_dataset")))
print("Environment cleaned. Ready for exploratory data analysis.")