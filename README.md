
# Macroprudential Heatmap Tool

**Macroprudential Heatmap Tool** is a data processing and visualization pipeline developed to support macro-financial surveillance at Peru’s financial regulatory authority (SBS). It aggregates key financial indicators across institutions and dimensions, providing early warning signals of systemic risk.

## 🔍 Objective

The tool aims to offer a dynamic and intuitive overview of financial vulnerabilities through standardized indicators, enabling better communication of risk across teams and decision-makers.

## 📁 Project Structure

```
SBS---Heatmap-Tool/
│
├── ETL/                         # R scripts for ETL process
│   ├── 01 Descarga de informacion.R
│   ├── 02 Manejo de informacion y generacion de indicadores.R
│   ├── 03 Estandarizacion y agrupacion.R
│   ├── 03 Estandarizacion v2 sin IG y RI.R
│   └── 04 Carga de informacion a SQL.R
│
├── Power BI/                   # Final Power BI report
│   └── Macroprudential Heatmap.pbix
│
└── README.md                   # Project overview
```

## ⚙️ Workflow

1. **Data Download** (`01 Descarga de informacion.R`)  
   Pulls time series and financial indicators from multiple sources.

2. **Indicator Processing** (`02 Manejo de informacion...`)  
   Cleans, calculates, and organizes indicators by institution and financial dimension.

3. **Standardization & Aggregation** (`03 Estandarizacion...`)  
   Applies z-score standardization, truncation, and aggregation to structure data for visualization.

4. **SQL Upload** (`04 Carga de informacion a SQL.R`)  
   Loads the final dataset into SQL tables for seamless dashboard integration.

5. **Visualization** (`Macroprudential Heatmap.pbix`)  
   Power BI dashboard that allows users to interact with heatmaps reflecting risks across credit, liquidity, solvency, and other dimensions.

## 📊 Use Cases

- Detect institutions or sectors under increasing financial pressure
- Monitor systemic risk trends over time
- Facilitate risk discussions in macroprudential committees and policy meetings

## 🛠️ Built With

- **R** for data transformation and SQL loading  
- **Power BI** for visualization

## 🌐 Live Dashboard

You can explore the live interactive version of the Macroprudential Heatmap via the following link:

🔗 **[View the interactive dashboard](https://app.powerbi.com/view?r=eyJrIjoiNDVjY2YwYWItZThiZi00YWZiLWE2NmYtN2Q2NjM1MDI4MTlkIiwidCI6IjgzYjAyYzkyLTVmMjYtNDhlZC05ZTViLTZjMmZjYTQ2YThlNiIsImMiOjN9&embedImagePlaceholder=true)**


## 📄 Acknowledgments

This project was developed as part of the macroprudential policy efforts at **SBS Perú**, following global best practices inspired by institutions like the IMF and BIS.
