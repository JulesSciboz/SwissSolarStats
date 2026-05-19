# ===================================================================
# SCRIPT 04: MULTIVARIATE REGRESSION & ROBUSTNESS CHECKS
# ===================================================================

print("Initializing regression environment and loading Master Panel...")

# 1. Load the serialized analytical dataset
data_path <- here("data", "processed", "solar_growth_2018_2024_final.rds")

if (!file.exists(data_path)) {
  stop("CRITICAL ERROR: Master panel not found. Run 02_data_assembly.R first.")
}

final_dataset <- readRDS(data_path)

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

# -------------------------------------------------------------------
# 2. METHODOLOGICAL JUSTIFICATION (NAIVE VS FIXED EFFECTS)
# -------------------------------------------------------------------
print("Estimating Naive and Fixed Effects Baseline Models...")

# Model 1: Naive OLS (No Canton Fixed Effects)
model_naive <- lm(
  New_Watts_per_Capita ~ Peak_Price_2023 + 
    log(Baseline_PV_Density_2017 + 1) + Left_Green_Share_2023 + 
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH, 
  data = final_dataset
)

# Model 2: Optimized Model (Cantonal Fixed Effects)
model_final <- lm(
  New_Watts_per_Capita ~ Peak_Price_2023 + 
    log(Baseline_PV_Density_2017 + 1) + Left_Green_Share_2023 + 
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH + as.factor(Canton), 
  data = final_dataset
)

se_naive <- summary(model_naive)$coefficients[, 2]
se_fe <- summary(model_final)$coefficients[, 2]

# Output Table 3: Model Evolution
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

# Output Table 4: Final Regression Analysis
stargazer(
  model_final,
  type = "html",
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
# 3. COEFFICIENT FOREST PLOT VISUALIZATION
# -------------------------------------------------------------------
print("Rendering Coefficient Forest Plot...")

model_results <- tidy(model_final, conf.int = TRUE) %>%
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
    Variable_Group = ifelse(str_detect(term, "\\[H"), "Primary Hypotheses", "Control Variables"),
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

coef_plot <- ggplot(model_results, aes(x = estimate, y = term, color = Significant)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = round(estimate, 1)), vjust = -1.5, size = 3.5, fontface = "bold", color = "black") +
  facet_wrap(~Variable_Group, scales = "free", ncol = 1) +
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
# 4. ROBUSTNESS CHECKS & SENSITIVITY ANALYSIS
# -------------------------------------------------------------------
print("Executing Robustness Checks (Long-Term Price & Trimmed Panel)...")

# Robustness Model 1: Long-Term Price Proxy
model_robust_mean <- lm(
  New_Watts_per_Capita ~ Mean_Price_13_23 + 
    log(Baseline_PV_Density_2017 + 1) + Left_Green_Share_2023 +
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH + as.factor(Canton), 
  data = final_dataset
)

# Robustness Model 2: High-Leverage Outlier Exclusion (Trimmed Panel)
threshold_99 <- quantile(final_dataset$New_Watts_per_Capita, 0.99, na.rm = TRUE)
data_trimmed <- final_dataset %>% filter(New_Watts_per_Capita <= threshold_99)

model_robust_trimmed <- lm(
  New_Watts_per_Capita ~ Peak_Price_2023 + 
    log(Baseline_PV_Density_2017 + 1) + Left_Green_Share_2023 + 
    Irradiation_kWh_m2 + 
    log(Taxable_Income) + log(Population_Density) + Share_SFH + as.factor(Canton), 
  data = data_trimmed
)

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

# Output Table A1: Robustness Checks
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

print("Regression analysis and robustness checks completed successfully.")