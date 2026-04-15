# 🇨🇭 Swiss Solar Growth Analysis (2018–2024)

This project analyzes the determinants of photovoltaic (PV) adoption across Swiss municipalities during the implementation phase of the Energy Strategy 2050.

By merging administrative energy data with socio-economic indicators, physical solar irradiation, and cantonal policy frameworks, we isolate the growth of solar capacity between 2018 and 2024. The ultimate regression model explicitly tests the impact of federal electricity price shocks (H1), green political ideology (H2/H3), structural constraints like the "Urban Renter Paradox", local peer effects, and explicit cantonal policies (H4).

## 🚀 How to Reproduce This Study

To ensure full reproducibility across different operating systems (Windows, Mac, Linux) while respecting GitHub's file size limits, this project uses the here package for relative file paths and excludes the massive raw data files from the repository.

Anyone can replicate this analysis by following these steps:

### Step 1: Clone and Setup

    Clone this repository to your local machine.

    Open the R project file (.Rproj) in RStudio, or simply open the main R script.

    Run STEP 0 and STEP 1 of the script. This will automatically install any missing packages and generate the required folder structure on your machine:
    Plaintext

    SwissSolarStats/
    ├── data/
    │   ├── processed/   (Final datasets will save here)
    │   └── raw/         (You will put the downloaded data here)
    ├── plots/           (Generated graphs will save here)
    ├── scripts/
    └── README.md

### Step 2: Download the Raw Data

Because GitHub has a 100MB file size limit, the massive Swiss energy and geographic datasets cannot be hosted directly in this repository.
Note: The lightweight municipal population dataset (px-x-0102020000_201.json) is already bundled in the data/raw/ folder for your convenience!

All datasets used in this study are publicly available via the Swiss Open Government Data portal, the Federal Statistical Office (BFS), and open academic repositories.

Download the following files and place them exactly as named into the data/raw/ folder:

1. Federal Office of Energy (BFE) - Solar Installations

        Source: [Elektrizitätsproduktionsanlagen](https://opendata.swiss/de/dataset/elektrizitatsproduktionsanlagen)

        Action: Download the CSV file.

        Save as: ElectricityProductionPlant.csv

2. Swisstopo - Official Directory of Towns and Cities

        Source: [Amtliches Ortschaftenverzeichnis](https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz_2056.csv.zip)

        Action: Download the CSV (LV95 format).

        Save as: AMTOVZ_CSV_LV95.csv
        
3. BFS - Referendum Data (H2: Green Index)

    Source: [Link to BFS Votations Database](https://www.pxweb.bfs.admin.ch/pxweb/fr/px-x-1703030000_101/px-x-1703030000_101/px-x-1703030000_101.px)

    Action: Download the municipal results (Excel format) for the three major climate and energy votes.

    Save as: * 2017Energy.Act_Outcome_YESSHARE.xlsx

        2021CO2.Act_Outcome_YESSHARE.xlsx

        2024Climate.Protection.Act_Outcome_YESSHARE.xlsx

    Note: The script automatically averages the "Yes" shares of these three referendums to create a robust Green_Index.

4. BFS - National Council Elections (H3: Left-Green Strength)

      Source: [Link to BFS Elections Database](https://www.bfs.admin.ch/bfs/fr/home/statistiques/catalogue.assetdetail.28945919.html)

      Action: Download the 2023 election results (NDJSON format).
  
      Save as: NRW_2023_Dataset.json

      Note: The script parses the JSON to extract the combined vote share of the SP and GPS parties per municipality.
    
5. Physical Solar Irradiation (Control Variable)

        Source: MeteoSwiss / Federal Office of Energy

        Action: Download the municipal solar potential dataset.

        Save as: solar_radiation_per_municipality.xlsx

6. Stadelmann Structural & Policy Data (H4)

        Source: [OSF Data Repository for Stadelmann et al.](https://osf.io/grvt9/files/2gmj8)

        Action: Download the replication .RData file.

        Save as: DataMunicipalitiesOSF.RData (Ensure it is placed in data/raw/Data and R-File of the regression analysis/ or adjust the file path in Step 4.6 of the script).

        Note: This provides the critical controls for the "Urban Renter Paradox" (Share of Single-Family Homes), Municipal Wealth, and Cantonal Policies (Subsidies, Tax Deductions, and Regulatory Friction).

### Live API Integration (ElCom Data)

This script utilizes the official Swiss Federal Linked Data Service (LINDAS) to automatically query and aggregate historical electricity prices (2013-2023) directly from the Federal Electricity Commission (ElCom) via SPARQL. No manual download is required for electricity prices. The script calculates the 10-year average, the 2023 peak price, and the absolute price shock (Delta) per municipality.

### Step 3: Run the Analysis

Open SwissSolarStats.Rproj in RStudio and run the main script. The script will output the final descriptive statistics and the multivariate OLS regression tables (controlling for structural constraints and peer effects) directly into the data/processed/ folder as perfectly formatted Word documents.
