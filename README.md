# 🇨🇭 Swiss Solar Growth Analysis (2018–2024)

This project analyzes the determinants of photovoltaic (PV) adoption across Swiss municipalities during the implementation phase of the **Energy Strategy 2050**.

By merging administrative energy data with socio-economic indicators, we isolate the **growth** of solar capacity between **2018 and 2024**, identifying which communes are successfully driving the energy transition.

## 🚀 How to Reproduce This Study

To ensure full reproducibility across different operating systems (Windows, Mac, Linux) while respecting GitHub's file size limits, this project uses the `here` package for relative file paths and excludes the massive raw data files from the repository. 

Anyone can replicate this analysis by following these steps:

### Step 1: Clone and Setup
1. Clone this repository to your local machine.
2. Open the R project file (`.Rproj`) in RStudio, or simply open the main R script. 
3. Run **STEP 0 and STEP 1** of the script. This will automatically install any missing packages and generate the required folder structure on your machine:
   ```
   text
   SwissSolarStats/
   ├── data/
   │   ├── processed/   (Final datasets will save here)
   │   └── raw/         (You will put the downloaded data here)
   ├── plots/           (Generated graphs will save here)
   ├── scripts/
   └── README.md
   ```
   
### Step 2: Download the Raw Data

Because GitHub has a 100MB file size limit, the massive Swiss energy and geographic datasets cannot be hosted directly in this repository.
*Note: The lightweight municipal population dataset (`px-x-0102020000_201.json`) is already bundled in the `data/raw/` folder for your convenience!*

All datasets used in this study are publicly available via the Swiss Open Government Data portal. Download the following three files and place them exactly as named into the newly created data/raw/ folder:

    1. Federal Office of Energy (BFE) - Solar Installations

        Source: [Elektrizitätsproduktionsanlagen](https://opendata.swiss/de/dataset/elektrizitatsproduktionsanlagen)

        Action: Download the CSV file.

        Save as: ElectricityProductionPlant.csv

    2. Swisstopo - Official Directory of Towns and Cities

        Source: [Amtliches Ortschaftenverzeichnis](https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz_2056.csv.zip)

        Action: Download the CSV (LV95 format).

        Save as: AMTOVZ_CSV_LV95.csv
        
        
#### Live API Integration (ElCom Data)

This script utilizes the official Swiss Federal Linked Data Service (LINDAS) to automatically query and aggregate historical electricity prices (2013-2023) directly from the Federal Electricity Commission (ElCom) via SPARQL. **No manual download is required for electricity prices.** The script calculates the 10-year average, the 2023 peak price, and the absolute price shock (Delta) per municipality.
   
   
### Step 3: Run the Analysis

Open SwissSolarStats.Rproj in RStudio and run the script 01_load_and_clean.R.
