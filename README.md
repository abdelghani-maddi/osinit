# 🌍 Open Science Initiatives Tracker - OSINIT

**Open Science Initiatives Tracker** is a web application designed to visualize, explore, and analyze global open science initiatives.  
This dashboard integrates interactive maps, charts, and statistical analyses to help researchers, policymakers, and communities understand the distribution, categories, governance models, and collaborative networks behind open science projects worldwide.

---

## 🚀 Features

- 📊 **Global Overview**  
  An interactive summary of open science initiatives by category, including:
  - Total count of initiatives.
  - Category distribution (e.g. Journals, Tools, Policies, Community, Data, Repositories).
  - A clickable map showing geographic distribution.
  - Access to detailed initiative lists.

- 🌐 **Interactive Maps**  
  Using Leaflet:
  - View global initiatives by country.
  - Bubble size represents initiative density.
  - Pie charts visualize community governance models ("Yes" vs. "No").

- 🔍 **Multiple Correspondence Analysis (MCA) & Clustering**  
  Explore structural patterns:
  - Visualize initiatives on MCA plots colored by `NonProfit` or `Category`.
  - Hierarchical clustering (`Ward.D2`) to group initiatives by similarity.
  - Access real-time cluster assignments in a shared Google Sheet.

- 📄 **About & FAQ Sections**  
  Embedded documentation and frequently asked questions.

---

## 🧑‍💻 Tech Stack

- `R` and `Shiny`  
- `shinydashboard` for UI components  
- `leaflet` and `leaflet.minicharts` for interactive maps  
- `ggplot2` and `plotly` for graphs  
- `googlesheets4` for live data sync with Google Sheets  
- `ade4`, `factoextra`, and `explor` for MCA and clustering  
- `sf`, `rnaturalearth`, and `cartogram` for spatial data.

---

## 🔗 Data Source

The application connects in real time to a shared **Google Sheet**:

```
https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit#gid=1136935931
```

This dataset lists and categorizes global open science initiatives, including attributes such as:
- Name
- Country
- Latitude/Longitude
- Governance model
- Category
- Nonprofit status.

---

## ⚙️ Requirements

- R (>= 4.2.0 recommended)
- Required R packages:
  ```r
  shiny
  shinydashboard
  tidyverse
  googlesheets4
  ade4
  factoextra
  plotly
  RColorBrewer
  rnaturalearth
  rnaturalearthdata
  leaflet
  leaflet.minicharts
  sf
  cartogram
  ggforce
  htmltools
  explor
  rsconnect
  ```

- A valid Google Sheets API JSON key (`cle.json`) is required for `googlesheets4` authentication.

---

## 💡 Deployment

- To run locally:

```r
shiny::runApp()
```

- To deploy online (via RStudio Connect / shinyapps.io):

```r
rsconnect::deployApp(appDir = "your/app/directory", appName = "openit")
```

Make sure your Google Sheets credentials are correctly set and your environment variables (`SHINY_APP_NAME`, `SHINY_APP_TOKEN`, `SHINY_APP_SECRET`) are loaded.

---

## 💬 About This Project

This application is part of the **OPENIT Project**, an initiative by the **GEMASS research lab** to map and understand the landscape of open science infrastructures and practices worldwide.  
The dashboard serves as both a research tool and an educational platform, aiming to foster transparency and collaboration in the open science ecosystem.

---

## 📣 Contribute

New initiatives can be proposed via the public form:  
👉 [Add Initiatives & Enrich Data](https://forms.gle/ZSnK9XkaVMBnKfPS6)

---

## 📄 License


CCBY - 4.0
