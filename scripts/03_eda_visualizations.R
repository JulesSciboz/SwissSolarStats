# ===================================================================
# SCRIPT 03: EXPLORATORY DATA ANALYSIS & DESCRIPTIVE STATISTICS
# ===================================================================

print("Initializing EDA and loading Master Panel...")

# 1. Load the serialized analytical dataset
data_path <- here("data", "processed", "solar_growth_2018_2024_final.rds")

if (!file.exists(data_path)) {
  stop("CRITICAL ERROR: Master panel not found. Run 02_data_assembly.R first.")
}

final_dataset <- readRDS(data_path)

# -------------------------------------------------------------------
# 2. GENERATE DESCRIPTIVE STATISTICS (TABLE 1)
# -------------------------------------------------------------------
print("Generating Descriptive Statistics Matrix...")

# Isolate Analytical Variables
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
  as.data.frame() # Stargazer requires base R data.frames, not tibbles

# Output to Console (Verification)
stargazer(
  desc_data, 
  type = "text", 
  title = "Table 1: Descriptive Statistics of Municipal Variables",
  digits = 2
)

# Output to Final Document (HTML/Word)
stargazer(
  desc_data, 
  type = "html", 
  out = here("data", "processed", "Table1_Descriptive_Statistics.doc"),
  title = "Table 1: Descriptive Statistics of Municipal Variables",
  digits = 2
)

# -------------------------------------------------------------------
# 3. DISTRIBUTIONAL AUDIT (HISTOGRAMS)
# -------------------------------------------------------------------
# Methodological Note: Validates variable distributions to justify the 
# log-transformations applied to highly skewed structural covariates 
# (Wealth and Population Density) required by OLS assumptions.

print("Executing Distributional Audits...")

# Transform Data for Faceted Auditing
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

# Render Distribution Grid
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
# 4. BIVARIATE CORRELATION ANALYSIS (SCATTERPLOTS)
# -------------------------------------------------------------------
# Methodological Note: Visualizes unadjusted, bivariate linear trends 
# between individual predictors and the dependent variable to assess 
# structural directionality before applying covariate controls.

print("Executing Bivariate Visualizations...")

# Transform Data for Faceted Plotting
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

# Render Structural Correlation Plot
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

print("EDA and Descriptive Statistics completed successfully.")