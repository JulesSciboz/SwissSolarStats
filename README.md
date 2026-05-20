# Swiss Solar Growth Analysis (2018–2024)

This project analyzes the determinants of photovoltaic (PV) adoption across Swiss municipalities during the implementation phase of the Energy Strategy 2050.

By merging administrative energy data with socio-economic indicators, physical solar irradiation, and cantonal fixed effects, we isolate the growth of solar capacity between 2018 and 2024. The ultimate regression model explicitly tests the impact of federal electricity price shocks (H1), local social momentum/peer effects (H2), green political ideology (H3), and structural constraints like the "Urban Renter Paradox" via single-family home shares.

## 🚀 How to Reproduce This Study

To ensure full reproducibility across different operating systems (Windows, Mac, Linux) while respecting GitHub's file size limits, this project uses the here package for relative file paths and excludes the massive raw data files from the repository.

Replicate this analysis by following these steps:

## Step 1: Clone and Setup

- Clone this repository to your local machine.

- Open the R project file (SwissSolarStats.Rproj) in RStudio.

## Step 2: Download the Raw Data

Because GitHub has a 100MB file size limit, the heavy Swiss energy and geographic datasets cannot be hosted directly in this repository.

Note: The lightweight municipal population dataset (px-x-0102020000_201.json) is already bundled in the data/raw/ folder for your convenience.

Download the following files and place them exactly as named into the data/raw/ folder:

### 1. Federal Office of Energy (BFE) - Solar Installations

    Source: Elektrizitätsproduktionsanlagen

    Action: Download the CSV file.

    Save as: ElectricityProductionPlant.csv

### 2. Swisstopo - Official Directory of Towns and Cities

    Source: Amtliches Ortschaftenverzeichnis

    Action: Download the CSV (LV95 format).

    Save as: AMTOVZ_CSV_LV95.csv

### 3. BFS - National Council Elections (H3: Left-Green Strength)

    Source: Link to BFS Elections Database

    Action: Download the 2023 election results (NDJSON format).

    Save as: NRW_2023_Dataset.json

### 4. Physical Solar Irradiation (Geographical Control)

    Source: MeteoSwiss / Federal Office of Energy

    Action: Download the municipal solar potential dataset.

    Save as: solar_radiation_per_municipality.xlsx

### 5. BFS - Building Structure (Urban Renter Paradox Control)

    Source: Federal Statistical Office (GWS)

    Action: Download the building category dataset for 2021.

    Save as: CH1.GWS,DF_GWS_REG1,1.0.0+all.csv

### 6. BFS - Municipal Wealth & Taxable Income

    Source: Federal Statistical Office

    Action: Download the municipal tax dataset.

    Save as: 27598_DE.csv

### 7. BFS - Population Density

    Source: Federal Statistical Office

    Action: Download the municipal density dataset.

    Save as: population_density_2018_2023(in).csv

### Live API Integration (ElCom Tariff Data)

This pipeline utilizes the official Swiss Federal Linked Data Service (LINDAS) to automatically query and aggregate historical electricity prices (2013-2023) directly from the Federal Electricity Commission (ElCom) via SPARQL. No manual download is required for electricity prices. The script calculates the 10-year average, the 2023 peak price, and the absolute price shock (Delta) per municipality dynamically.

## Step 3: Run the Analysis

Open master.R in the root directory and source the script. It will sequentially execute the modular files in the scripts/ folder:

    00_config.R: Installs missing dependencies and builds directory trees.

    01_data_preparation.R: Ingests, filters, and standardizes raw structural datasets.

    02_data_assembly.R: Executes spatial joins via BFS_Nr to build the unified solar_growth_2018_2024_final.rds master panel.

    03_eda_visualizations.R: Renders summary statistics (Table 1) and exploratory structural scatterplots/histograms to the plots/ folder.

    04_regression_analysis.R: Estimates the progressive bivariate to multivariate OLS models (with Cantonal fixed effects) and generates formatted .doc tables (Tables 3, 4, 5) and the coefficient forest plot.

### The directory structure must look like this:

SwissSolarStats/
├── data/
│   ├── processed/   (Final datasets and tables will save here)
│   └── raw/         (You will put the downloaded raw data here)
├── plots/           (Generated EDA graphs will save here)
├── scripts/
│   ├── 00_config.R
│   ├── 01_data_preparation.R
│   ├── 02_data_assembly.R
│   ├── 03_eda_visualizations.R
│   └── 04_regression_analysis.R
├── master.R         (The central execution script)
└── README.md
