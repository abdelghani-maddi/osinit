#######################
# Clear the workspace
#######################
rm(list = ls())  # Removes all variables from the workspace

#######################
# Load necessary libraries
#######################
library(shiny)  # For creating web applications
library(shinydashboard)  # For dashboard UI components
library(tidyverse)  # Collection of packages for data manipulation and visualization
library(googlesheets4)  # For reading from and writing to Google Sheets
library(ade4)  # For multivariate analysis (including Correspondence Analysis)
library(factoextra)  # For visualizing multivariate analysis results
library(plotly)  # For interactive plotting
library(RColorBrewer)  # For color palettes
library(rnaturalearth)  # For access to natural Earth data (maps)
library(rnaturalearthdata)  # For additional natural Earth data
library(leaflet.minicharts)  # For mini charts in leaflet maps
library(sf)  # For working with spatial data
library(cartogram)  # For cartogram maps
library(ggforce)  # For advanced plotting with ggplot2
library(leaflet)  # For interactive maps
library(htmltools)  # For HTML tools in Shiny
library(explor)
library(htmlwidgets)


#######################
# Set up Shiny app credentials
#######################
# Authenticate with Shiny server using account credentials stored in environment variables
library(rsconnect)

name <- Sys.getenv("SHINY_APP_NAME")
token <- Sys.getenv("SHINY_APP_TOKEN")
secret <- Sys.getenv("SHINY_APP_SECRET")

rsconnect::setAccountInfo(name = name, token = token, secret = secret)

# Authenticate with Google Sheets using the JSON key file (you need to create one:))
gs4_auth(path = file.path(getwd(), "cle.json"))

#######################
# Load Data from Google Sheets
#######################
# Define a function to download data from a Google Sheets URL
download_data <- function() {
  sheet_url <- "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=1136935931#gid=1136935931" # "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=0#gid=0"
  read_sheet(sheet_url) %>%
    as.data.frame()
}

# Load the data into a variable
data <- download_data()

# Set row names for the data frame to be the organization names
row.names(data) <- data$OrgName2

#######################
# Group Initiatives by Country
#######################
# Group data by country and summarize by counting the number of initiatives
initiatives_par_pays <- data %>%
  group_by(Country, adm0_a3, Latitude, Longitude) %>%
  summarise(
    nb = n(),  # Count number of initiatives per country
    liste_initiatives = paste(OrgName2, collapse = "<br>"),  # Concatenate organization names
    .groups = "drop"  # Drop the grouping structure
  )

# Create a color palette based on the number of initiatives
pal <- colorNumeric("plasma", domain = initiatives_par_pays$nb)

#######################
# Prepare Data for Community Governance Visualization
#######################
# 1️⃣ Group initiatives by country and governance type, and count occurrences
data_pie <- data %>%
  count(adm0_a3, CommunityGovernance, name = "nb_cg") %>%
  group_by(adm0_a3, CommunityGovernance) %>%  
  summarise(nb_cg = sum(nb_cg), .groups = "drop")  # Aggregate counts for governance types

# 2️⃣ Select coordinates for countries
data_coord <- data %>%
  select(adm0_a3, Country, Longitude, Latitude) %>%
  unique()  # Remove duplicate coordinates

# 3️⃣ Join the governance data with country coordinates
data_pie <- data_pie %>%
  right_join(data_coord, by = "adm0_a3") %>%  # Properly join the data by country code
  replace_na(list(nb_cg = 0, CommunityGovernance = "No")) %>%  # Handle missing values
  filter(!is.na(Longitude)) %>%  # Remove countries without coordinates
  select(Country, Longitude, Latitude, CommunityGovernance, nb_cg) %>% 
  pivot_wider(names_from = CommunityGovernance, values_from = nb_cg, values_fill = list(nb_cg = 0)) %>%  # Reshape data for pie chart
  st_drop_geometry()  # Remove geometry column if present

# 4️⃣ Check and remove unnecessary columns
data_pie <- data_pie %>% select(-any_of("0"))  # Remove any column named "0" (if it exists)

#######################
# Perform Multiple Correspondence Analysis (MCA)
#######################
# Function to perform MCA (Correspondence Analysis)
perform_mca <- function(data) {
  d <- data %>%
    select(OrgName2, Nonprofit, Category, CommunityGovernance) %>%
    mutate(across(everything(), as.factor))  # Convert columns to factors for MCA
 
  row.names(d) <- d$OrgName2

  d <- d %>% select(-OrgName2)
  
  acm <- dudi.acm(d, scannf = FALSE, nf = Inf)  # Perform MCA (ACM in R)
  list(acm = acm, d = d)  # Return results
}

# Function to perform hierarchical clustering on MCA results
perform_clustering <- function(cah) {
  md_phi2 <- dist.dudi(acm)  # Calculate distance matrix for clustering
  arbre_phi2 <- hclust(md_phi2, method = "ward.D2")  # Perform hierarchical clustering using Ward's method
}

# Run MCA on the data
mca_results <- perform_mca(data)
acm <- mca_results$acm

# Perform clustering on the MCA results
arbre_phi2 <- perform_clustering(acm)

#######################
# Assign Clusters Based on Hierarchical Clustering
#######################
data$cluster <- cutree(arbre_phi2, 4)  # Cut the dendrogram into 5 clusters


#######################
# Define Custom CSS for Popups in Leaflet Maps
#######################
popup_css <- "
.custom-popup .leaflet-popup-content {
  max-height: 150px;
  overflow-y: auto;
}
"

#######################
monitoring_data <- function() {
  read_sheet("https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo", sheet = "Monitors")
}

#######################
df_focus <- function() {
  read_sheet("https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo", sheet = "Focus")
}

# ================================================
# User Interface - Open Science Initiatives Dashboard
# ================================================

ui <- dashboardPage(
  
  # Header Section: Title and width setup
  dashboardHeader(
    title = tags$strong("Open Science Initiatives"),  # Dashboard Title
    titleWidth = 450                                  # Custom title width
  ),
  
  # Sidebar Section: Navigation menu and logos
  dashboardSidebar(
    sidebarMenu(
      # Navigation menu items
      menuItem("Global Overview", tabName = "overview", icon = icon("globe")),
      menuItem("MCA & Clustering", tabName = "mca", icon = icon("chart-bar")),
      menuItem("Monitoring Principles", tabName = "principles", icon = icon("balance-scale")),
      menuItem("Monitoring Landscape", tabName = "monitoring", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("FAQs", tabName = "FAQs", icon = icon("lightbulb")),
    
      
      # Clickable logo linking to external website (OSMI)
      tags$li(class = "dropdown", 
              tags$a(href = "https://open-science-monitoring.org/", 
                     target = "_blank", 
                     tags$img(
                       src = "logo-osmi.png", 
                       height = "40px", 
                       style = "margin: 0px 0 0px 0px; display: block;"
                     )
              )
      ),
      
      # Clickable logo linking to external website (GEMASS)
      tags$li(class = "dropdown", 
              tags$a(href = "https://www.gemass.fr/contract/openit/", 
                     target = "_blank", 
                     tags$img(
                       src = "logo.png", 
                       height = "40px", 
                       style = "margin: 0px 0 0px 0px; display: block;"
                     )
              )
      ),
      
      
      # Fixed-position logo at the bottom for ANR link
      tags$div(
        tags$a(href = "https://anr.fr/Projet-ANR-24-RESO-0001", target = "_blank",
               tags$img(src = "logo_ANR.jpg", height = "40px", style = "margin: 10px;")
        ),
        style = "position: absolute; bottom: 20px; left: 10px;"
      )
    )
  ),
  
  # Body Section: Main visual content and layout
  dashboardBody(
    
    tags$head(
      # Custom Sidebar Hover Effects for each tab
      tags$style(HTML("
        .sidebar-menu li:nth-child(1) a:hover {
          background-color: #ba3470 !important;  /* Pink for Global Overview */
          color: white !important;
        }
        .sidebar-menu li:nth-child(2) a:hover {
          background-color: #17a2b8 !important;  /* Cyan for MCA & Clustering */
          color: white !important;
        }
        .sidebar-menu li:nth-child(3) a:hover {
          background-color: #ffc107 !important;  /* Yellow for About */
          color: black !important;
        }
        .sidebar-menu li:nth-child(4) a:hover {
          background-color: #8aa728 !important;  /* Green for FAQs */
          color: white !important;
        }
        .sidebar-menu li:nth-child(5) a:hover {
          background-color: #a72859 !important;  /* Pink for Logo */
          color: white !important;
        }
        .sidebar-menu li:nth-child(6) a:hover {
          background-color: #288aa7 !important;  /* Blue for Logo */
          color: white !important;
        }
        .sidebar-menu li:nth-child(7) a:hover {
          background-color: #8aa728 !important;  /* Pink for Global Overview */
          color: white !important;
        }
        .sidebar-menu li:nth-child(8) a:hover {
          background-color: #288aa7 !important;  /* Blue for Logo */
          color: white !important;
        }
      ")),
    
      tags$style(HTML("
  .badge {
    font-size: 12px;
    padding: 5px 8px;
    border-radius: 8px;
    color: white;
  }
  .badge-primary { background-color: #007bff; }
  .badge-success { background-color: #28a745; }
  .badge-warning { background-color: #ffc107; color: black; }
  .badge-info    { background-color: #17a2b8; }
  .badge-danger  { background-color: #dc3545; }
  .badge-secondary { background-color: #6c757d; }
  table.dataTable td { vertical-align: middle; }
  ")),
      
      tags$style(HTML("
  .btn-visit {
    background-color: #007bff;
    color: white;
    padding: 5px 12px;
    border-radius: 6px;
    text-decoration: none;
    font-size: 12px;
    transition: background-color 0.3s ease;
  }

  .btn-visit:hover {
    background-color: #0056b3;
    text-decoration: none;
  }

  .wrap-text {
    white-space: normal !important;
    word-wrap: break-word;
  }

  table.dataTable td {
    vertical-align: middle;
    font-size: 14px;
  }
"))
      
    ),
    
    tags$style(HTML("
  .badge {
    font-size: 13px;
    padding: 6px 10px;
    border-radius: 12px;
    color: white;
    margin-right: 5px;
  }
  ")),
    tabItems(
      
      # --------------------------------------------
      # Global Overview Tab
      # --------------------------------------------
      tabItem(tabName = "overview",
              fluidRow(
                # Total Initiatives Box
                box(
                  title = "Global Open Science Initiatives",  
                  status = "primary",  
                  solidHeader = TRUE,  
                  width = 4,
                  height = "521px",
                  style = "border-radius: 30px; box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);",
                  tags$div(
                    style = "text-align: center; padding-top: 50px;",
                    tags$i(class = "fa fa-lightbulb-o", style = "font-size: 100px; color: #007bff; margin-bottom: 10px;"),
                    tags$div(
                      style = "font-size: 100px; font-weight: bold; color: #007bff;",
                      textOutput("distinct_initiatives")
                    ),
                    tags$div(
                      style = "font-size: 25px; font-weight: bold; color: #333;",
                      "Total Number of Initiatives"
                    ),
                    tags$p(style = "font-size: 15px; margin-top: 10px; color: #555;"),
                    # Links to data and contribution forms
                    tags$a(
                      href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit#gid=1136935931",
                      target = "_blank",
                      "🔗 Access and Download Data",
                      style = "color: #ffffff; text-decoration: none; display: inline-block; margin-top: 15px; padding: 10px; 
                               background-color: #007bff; border-radius: 10px; font-size: 16px;"
                    ),
                    tags$a(
                      href = "https://forms.gle/ZSnK9XkaVMBnKfPS6",
                      target = "_blank",
                      "🔗 Add Initiatives and Enrich Data",
                      style = "color: #0c0c0d; text-decoration: none; display: inline-block; margin-top: 15px; padding: 10px; 
                               background-color: #6aff00; border-radius: 10px; font-size: 16px;"
                    )
                  )
                ),
                
                # Initiatives by Category Chart Box
                box(
                  title = "Initiatives by Category", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 8,
                  plotlyOutput("category_plot"),
                  tags$p(
                    "This bar chart provides an overview of the number of initiatives grouped by their focus area. Hover over each bar to view detailed counts, or click on a bar to explore the specific initiatives within the selected category."
                  )
                )
              ),
              
              # Selected Initiatives List Box
              fluidRow(
                box(
                  title = "Selected Initiatives List",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("clicked_initiatives"),
                  tags$p(
                    "Click on a bar above to view the initiatives belonging to the selected category.",
                    style = "color: #555;"
                  )
                )
              ),
              
              # Maps for Distribution and Governance
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Global Distribution of Initiatives",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    leafletOutput("world_map"),
                    tags$style(HTML(popup_css)),
                    tags$p(
                      "This map visualizes the geographical distribution of open science initiatives around the world. Click on the bubbles to view the number of initiatives per country and a list of the specific projects."
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Community Governance Map",
                    status = "warning",
                    solidHeader = TRUE,
                    width = NULL,
                    leafletOutput("comm_gov"),
                    tags$p(
                      "This map displays a comparison of community governance practices in open science initiatives. The pie charts represent the proportion of initiatives with and without community-driven governance structures."
                    )
                  )
                )
              )
      ),
      
      # --------------------------------------------
      # Monitoring principles Tab
      # --------------------------------------------
      
      tabItem(tabName = "principles",
              tags$head(
                tags$style(HTML("
      .principle-section {
        padding: 50px 40px;
        background-color: #f9fafb;
        background-image: url('soft-gradient.jpg');
        background-size: cover;
        background-position: center;
      }

      .principle-title-main {
        font-size: 38px;
        font-weight: 800;
        text-align: center;
        color: #1f2937;
        margin-bottom: 10px;
      }

      .principle-subtitle {
        font-size: 17px;
        text-align: center;
        color: #6b7280;
        margin-bottom: 40px;
      }

      details.accordion-card {
        background-color: #ffffffdd;
        border-radius: 18px;
        box-shadow: 0 20px 30px rgba(0,0,0,0.06);
        margin-bottom: 30px;
        overflow: hidden;
        border-left: 6px solid #3b82f6;
        transition: all 0.3s ease;
      }

      details[open].accordion-card {
        box-shadow: 0 28px 40px rgba(0,0,0,0.08);
      }

      summary.accordion-header {
        padding: 22px 30px;
        font-size: 20px;
        font-weight: 700;
        background: linear-gradient(to right, #3b82f6, #60a5fa);
        color: white;
        display: flex;
        align-items: center;
        gap: 14px;
        cursor: pointer;
        transition: background 0.3s ease;
      }

      summary.accordion-header:hover {
        background: linear-gradient(to right, #2563eb, #60a5fa);
      }

      .accordion-content {
        padding: 25px 35px;
        background-color: #ffffff;
        animation: fadeIn 0.4s ease-in-out;
      }

      .accordion-content ul {
        padding-left: 20px;
      }

      .accordion-content li {
        margin-bottom: 12px;
        font-size: 16px;
        line-height: 1.7;
        color: #374151;
      }

      .badge-circle {
        width: 42px;
        height: 42px;
        background-color: rgba(255,255,255,0.2);
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 18px;
      }

      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }

      .osmi-logo-small {
        height: 36px;
        float: right;
        margin-top: -10px;
        opacity: 0.85;
      }

      .source-box {
        text-align: center;
        margin-top: 40px;
      }

      .source-box a {
        display: inline-block;
        margin-top: 10px;
        padding: 8px 20px;
        background-color: #004c97;
        color: white;
        border-radius: 25px;
        text-decoration: none;
        font-weight: bold;
        font-size: 15px;
        transition: background-color 0.3s ease;
      }

      .source-box a:hover {
        background-color: #00386f;
      }
    "))
              ),
              
              fluidRow(
                column(12,
                       div(class = "principle-section",
                           
                           # Header
                           div(
                             style = "background-color: rgba(255, 255, 255, 0.9); padding: 20px 30px; border-radius: 12px; display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap;",
                             div(
                               style = "flex: 1; min-width: 250px;",
                               div(class = "principle-title-main", "Principles of Open Science Monitoring"),
                               div(class = "principle-subtitle", "Explore the foundational dimensions of ethical, inclusive and sustainable monitoring practices.")
                             ),
                             tags$img(src = "logo-osmi.png", class = "osmi-logo-small")
                           ),
                           
                           br(),
                           
                           # Accordion Block 1
                           tags$details(class = "accordion-card",
                                        style = "border-left: 6px solid #3b82f6;",
                                        tags$summary(class = "accordion-header",
                                                     style = "background: linear-gradient(to right, #3b82f6, #60a5fa);",
                                                     span(class = "badge-circle", icon("lightbulb")),
                                                     "Part 1: Relevance and Significance"
                                        ),
                                        div(class = "accordion-content", HTML("
            <ul>
              <li><strong>Applicable and clear in scope:</strong> Indicators must be explicitly defined, relevant and scoped appropriately.</li>
              <li><strong>Meaningful for planning and policy:</strong> Indicators should support policy-making across contexts.</li>
              <li><strong>Co-created:</strong> Developed inclusively with researchers and communities.</li>
              <li><strong>Inclusive:</strong> Reflect diversity of contexts, languages, gender, knowledge systems.</li>
              <li><strong>Modular:</strong> Allow flexible composition of indicators for local/global alignment.</li>
              <li><strong>Reliable:</strong> Explicit about scientific consensus and development stage.</li>
              <li><strong>Consistent:</strong> Enable long-term, cross-institutional comparability.</li>
            </ul>
          "))
                           ),
                           
                           # Accordion Block 2
                           tags$details(class = "accordion-card",
                                        style = "border-left: 6px solid #d4af37;",
                                        tags$summary(class = "accordion-header",
                                                     style = "background: linear-gradient(to right, #facc15, #fde68a);",
                                                     span(class = "badge-circle", icon("eye")),
                                                     "Part 2: Transparency and Reproducibility"
                                        ),
                                        div(class = "accordion-content", HTML("
            <ul>
              <li><strong>Openness:</strong> Use open infrastructures, tools, and licenses.</li>
              <li><strong>Quality of sources:</strong> Data must be timely, complete, and accurate.</li>
              <li><strong>Documentation:</strong> Clearly describe data sources, provenance, and methods.</li>
              <li><strong>Reproducibility and reusability:</strong> Version indicators and enable reuse.</li>
              <li><strong>Metadata:</strong> Include rich, standardized metadata with PIDs.</li>
              <li><strong>Community principles:</strong> Align with FAIR, CARE, TRUST frameworks.</li>
              <li><strong>Contextual communication:</strong> Prevent misinterpretation; explain clearly.</li>
              <li><strong>Conflict of interest:</strong> Declare transparently when relevant.</li>
            </ul>
          "))
                           ),
                           
                           # Accordion Block 3
                           tags$details(class = "accordion-card",
                                        style = "border-left: 6px solid #8b5cf6;",
                                        tags$summary(class = "accordion-header",
                                                     style = "background: linear-gradient(to right, #8b5cf6, #c4b5fd);",
                                                     span(class = "badge-circle", icon("balance-scale")),
                                                     "Part 3: Self-Assessment and Responsible Use"
                                        ),
                                        div(class = "accordion-content", HTML("
            <ul>
              <li><strong>Self-evaluation:</strong> Regularly assess and disclose alignment with these principles.</li>
              <li><strong>Revision:</strong> Continuously improve and adapt indicators over time.</li>
              <li><strong>Environmental responsibility:</strong> Reduce monitoring system impact.</li>
              <li><strong>Long-term sustainability:</strong> Plan for funding, access and infrastructure.</li>
              <li><strong>Constructive comparison:</strong> Avoid rankings, encourage fair benchmarking.</li>
            </ul>
          "))
                           ),
                           
                           # Footer
                           div(class = "source-box",
                               tags$em("Source: Open Science Monitoring Initiative, 2025"),
                               tags$br(),
                               tags$a(href = "https://doi.org/10.5281/zenodo.15807481", target = "_blank", "Access full document")
                           )
                       )
                )
              )
      )
      
      
      
      
      ,
      
      
      # --------------------------------------------
      # MCA & Clustering Tab
      # --------------------------------------------
      tabItem(tabName = "mca",
              fluidRow(
                # MCA NonProfit + Interpretation
                column(width = 7.5,
                       box(
                         title = "Multiple Correspondence Analysis (MCA): NonProfit",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         uiOutput("mca_plot_ui"),
                         tags$p(
                           "This plot shows the results of a Multiple Correspondence Analysis (MCA), focusing on the first two dimensions. It visualizes the relationships between different characteristics of open science initiatives."
                         )
                       )
                ),
                column(width = 4.5,
                       box(
                         title = "How to Interpret the MCA Plot (NonProfit)",
                         status = "info",
                         solidHeader = TRUE,
                         width = 12,
                         tags$div(
                           style = "font-size: 15px; line-height: 1.6;",
                           tags$p("This plot displays the results of a Multiple Correspondence Analysis (MCA) performed on categorical variables describing open science initiatives. Each point in the plot represents a single initiative."),
                           tags$ul(
                             tags$li(tags$b("What is plotted:"), "Initiatives are projected into a reduced two-dimensional space derived from categorical variables such as their nonprofit status, governance model, and category."),
                             tags$li(tags$b("Color coding:"), "Points are colored by profit status: red for for-profit initiatives and blue for nonprofit ones. This makes it easy to visually assess how these types of organizations distribute across the MCA space."),
                             tags$li(tags$b("Axes (Dimensions):"), "The horizontal and vertical axes correspond to the first two dimensions of the MCA, which summarize the most significant patterns in the data."),
                             tags$li(tags$b("Spatial interpretation:"), "Initiatives that are closer together on the plot tend to share similar characteristics. For example, a cluster of nonprofit initiatives might share governance structures or operational models."),
                             tags$li(tags$b("Purpose:"), "This plot helps to uncover latent profiles in the ecosystem of open science initiatives, showing how nonprofit and for-profit entities differentiate across multiple categorical dimensions.")
                           ),
                         )
                       )
                )
              ),
              
              fluidRow(
                # MCA Category + Interpretation
                column(width = 7.5,
                       box(
                           title = "Multiple Correspondence Analysis (MCA): Category",
                           status = "primary",
                           solidHeader = TRUE,
                           width = 12,
                           uiOutput("mca_plot_ui2"),
                           tags$p(
                           "This plot presents the results of a Multiple Correspondence Analysis (MCA), highlighting the first two dimensions and grouping initiatives by category."
                           )
                         )
                ),
                column(width = 4.5,
                       box(
                         title = "How to Interpret the MCA Plot (Category)",
                         status = "info",
                         solidHeader = TRUE,
                         width = 12,
                         tags$div(
                           style = "font-size: 15px; line-height: 1.6;",
                           tags$p("This plot shows the same MCA projection, but colored by the main category of each initiative (e.g., publishing platform, repository, infrastructure)."),
                           tags$ul(
                             tags$li(tags$b("What is plotted:"), "Each point is an open science initiative, projected based on their categorical characteristics."),
                             tags$li(tags$b("Color coding:"), "Initiatives are colored according to their assigned category, allowing for visual comparison between types of services or platforms."),
                             tags$li(tags$b("Interpretation:"), "Initiatives from the same category may appear grouped together if they share common traits. Conversely, scattered colors within a region may indicate overlapping features between categories."),
                             tags$li(tags$b("Axes:"), "As in the previous plot, the axes represent the two main MCA dimensions, providing a simplified structure of multivariate associations."),
                             tags$li(tags$b("Goal:"), "This visualization supports the identification of functional or organizational similarities between initiatives, beyond their declared category.")
                           ),
                           tags$p("This view highlights the internal diversity or homogeneity within categories, and helps detect hybrid initiatives that span multiple functions.")
                         )
                       )
                )
              ),
              
              fluidRow(
                # Dendrogram + Interpretation
                column(width = 7.5,
                       box(
                         title = "Hierarchical Clustering Dendrogram",
                         status = "danger",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("dendrogram"),
                         tags$p(
                           "This dendrogram shows the hierarchical clustering of initiatives based on their characteristics. Clusters reveal how initiatives group by similarity."
                         )
                       )
                ),
                column(width = 4.5,
                       box(
                         title = "How to Interpret the Clustering Dendrogram",
                         status = "info",
                         solidHeader = TRUE,
                         width = 12,
                         tags$div(
                           style = "font-size: 15px; line-height: 1.6;",
                           tags$p("The dendrogram visualizes the result of a hierarchical clustering performed on the coordinates obtained from the MCA. It helps group similar initiatives based on the MCA's multi-dimensional representation."),
                           tags$ul(
                             tags$li(tags$b("What is clustered:"), "The clustering uses the MCA coordinates of each initiative, capturing the variation across all selected categorical variables."),
                             tags$li(tags$b("Height of branches:"), "The vertical position where two branches merge represents the dissimilarity between the groups. The lower the merge, the more similar the initiatives."),
                             tags$li(tags$b("Cutting the tree:"), "By cutting the dendrogram at a specific height, we can define distinct clusters (groups) of initiatives."),
                             tags$li(tags$b("Interpretation of clusters:"), "Each cluster brings together initiatives with similar profiles—often combinations of being nonprofit, community-based, and operating within a given category.")
                           ),
                           tags$p("The clustering was performed using Euclidean distances on MCA coordinates, followed by a hierarchical method (Ward's linkage). This method helps define meaningful typologies of open science initiatives.")
                         )
                       )
                )
              ),
              
              # External Link to Cluster Table
              fluidRow(
                box(
                  title = "View Clusters",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  style = "cursor: pointer; text-align: center; font-size: 18px; border-radius: 15px; border: 1px solid #007bff; padding: 20px; transition: all 0.3s ease-in-out; color: white;",
                  tags$a(
                    href = "https://docs.google.com/spreadsheets/d/1WWY-AFsFY70xf7JgRAZFwjl7tcHcdCplb8QT5bb3-k8/edit?gid=998126494#gid=998126494",
                    target = "_blank",
                    "📊 Access Clusters on Google Sheets",
                    style = "color: #ffffff; text-decoration: none; display: inline-block; padding: 15px 30px; background-color: #007bff; border-radius: 10px; font-weight: bold; font-size: 16px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); transition: background-color 0.3s ease;"
                  ),
                  tags$p(
                    "Click the link above to explore the clustering results in more detail, including which initiatives belong to each cluster.",
                    style = "margin-top: 15px; font-size: 16px; line-height: 1.6; color: #0a0303; text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.2);"
                  )
                )
              )
      ),
      
      # --------------------------------------------
      # Monitoring Tab
      # --------------------------------------------
      tabItem(tabName = "monitoring",
              fluidRow(
                box(
                  title = "Open Science Monitoring Initiatives",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("monitoring_table"),
                  tags$p(
                    "This interactive table lists various national, international, institutional, and specialized initiatives monitoring open science. Click on a column header to sort or use the search bar to filter."
                  ),
                  tags$a(
                    href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=1576141174#gid=1576141174",
                    target = "_blank",
                    "📥 View or edit full Google Sheet here",
                    style = "display:inline-block; margin-top:10px; font-size:16px;"
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = tagList(icon("globe"), "Focus by country/region: Open Science Initiatives"),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  DT::dataTableOutput("focus_table"),
                  tags$p(
                    "This table presents major open science initiatives from Nordic countries, including national dashboards, infrastructures, funding programmes, and collaborative platforms. It offers an entry point to explore regional efforts and synergies in open science.",
                    style = "margin-top:10px; font-size:15px;"
                  ),
                  tags$a(
                    href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=2146313781#gid=2146313781",
                    target = "_blank",
                    "📥 View or edit full Google Sheet here",
                    style = "display:inline-block; margin-top:10px; font-size:16px;"
                  )
                )
              )
              
      ),
      
      
      
      # --------------------------------------------
      # About Tab
      # --------------------------------------------
      tabItem(tabName = "about",
              fluidRow(
                div(
                  style = "padding: 20px; text-align: center; font-size: 18px;",
                  tags$p(tags$strong("About this project")),
                  tags$iframe(
                    src = "https://docs.google.com/document/d/163di4K3TfQqM-zqEc7IrxB7SCnhiZPn0pK-fbQrIftQ/preview",
                    width = "100%",
                    height = "800px",
                    style = "border: none;"
                  )
                )
              )
      ),
      
      # --------------------------------------------
      # FAQs Tab
      # --------------------------------------------
      tabItem(tabName = "FAQs",
              fluidRow(
                div(
                  style = "padding: 20px; text-align: center; font-size: 18px;",
                  tags$p(tags$strong("Frequently Asked Questions - FAQs")),
                  tags$iframe(
                    src = "https://docs.google.com/document/d/1F0CrXoXABLvmHDQO3ChrjtR_u9g7Zz5K4tzziCTsqEQ/preview",
                    width = "100%",
                    height = "800px",
                    style = "border: none;"
                  )
                )
              )
      )
    )
  )
)

# --------------------------------------------
# Favicon and Browser Tab Title
# --------------------------------------------
ui <- tagList(
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "https://upload.wikimedia.org/wikipedia/commons/f/f0/Cadenas-ouvert-vert.svg"),
    tags$title("OS Initiatives Dashboard")
  ),
  ui
)


# =============================================
# Server Logic - Open Science Initiatives Dashboard
# =============================================

server <- function(input, output, session) {
  
  # Render the GEMASS logo image in the sidebar
  output$logo_image <- renderImage({
    list(
      src = "www/logo.png",          # Path to the logo image
      alt = "Logo OS Initiatives",   # Alt text for accessibility
      height = "70px",               # Fixed height for display
      width = "auto"                 # Auto width to preserve ratio
    )
  }, deleteFile = FALSE)             # Prevent deletion after rendering
  
  # Reactive polling: auto-refresh the data every 10 minutes
  data_reactive <- reactivePoll(
    600000, session,
    checkFunc = function() { Sys.time() },         # Dummy check (forces periodic refresh)
    valueFunc = function() { download_data() }     # Function to download and return new data
  )
  
  # Output: display the number of distinct initiatives in the UI
  output$distinct_initiatives <- renderText({
    data <- data_reactive()  # Get the latest data snapshot
    n_distinct(data$OrgName2)  # Count distinct initiatives
  })
  
  # Reactive placeholder for selection handling
  selected_category <- reactiveVal(NULL)  # Stores user-selected category from the chart
  
  # Generate a category count table for plotting
  plotted_categories <- reactive({
    data <- data_reactive()
    data %>%
      count(Category, name = "count") %>%
      arrange(desc(count)) %>%
      mutate(
        Category_label = str_wrap(as.character(Category), width = 9),
        Category_label = factor(Category_label, levels = unique(Category_label))
      )
  })
  
  # Render the category distribution plot with Plotly
  output$category_plot <- renderPlotly({
    cat_data <- plotted_categories()
    
    p <- ggplot(cat_data, aes(x = Category_label, y = count, fill = Category)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = count), vjust = 3, size = 4.5, fontface = "bold", color = "black") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 9)),
        axis.title.y = element_text(margin = margin(r = 10))
      ) +
      scale_fill_brewer(palette = "Set3") +
      labs(
        x = "Category",
        y = "Number of Initiatives",
        title = "Number of Initiatives by Category"
      )
    
    ggplotly(p, tooltip = "none", source = "select_bar")
  })
  
  # Detect and handle click events on the Plotly bar chart
  observeEvent(event_data("plotly_click", source = "select_bar"), {
    click_data <- event_data("plotly_click", source = "select_bar")
    
    if (!is.null(click_data)) {
      index <- round(click_data$x)  # Plotly index starts at 0
      categories <- plotted_categories()
      
      if (index >= 1 && index <= nrow(categories)) {
        cat <- categories$Category[index]
        selected_category(cat)
        cat("Category clicked (via index):", cat, "\n")
      } else {
        selected_category(NULL)
      }
    }
  })
  
  # Render the list of initiatives corresponding to the selected category
  output$clicked_initiatives <- renderUI({
    data <- data_reactive()
    
    if (is.null(selected_category())) {
      return(tags$p(
        "Click on a bar in the chart to view the corresponding initiatives.",
        style = "color: #999;"
      ))
    }
    
    filtered_data <- data %>% filter(Category == selected_category())
    cat("Selected category after update:", selected_category(), "\n")
    cat("Number of rows after filtering:", nrow(filtered_data), "\n")
    
    DT::dataTableOutput("initiatives_table")
  })
  
  # Render the filtered initiatives table
  output$initiatives_table <- DT::renderDataTable({
    filtered_data <- data_reactive() %>% filter(Category == selected_category())
    DT::datatable(
      filtered_data %>% select(OrgName, Category, Country, CommunityGovernance, Nonprofit, OpenSource),
      options = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # Render the world map with circle markers representing initiative counts
  output$world_map <- renderLeaflet({
    data <- data_reactive()
    
    map <- leaflet(initiatives_par_pays) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~sqrt(nb) * 2,
        color = ~colorNumeric("plasma", nb)(nb),
        fillOpacity = 0.8,
        popup = ~paste(
          "<b>Country :</b>", Country, "<br>",
          "<b>Count :</b>", nb, "<br>",
          "<b>Initiatives list :</b><br>", liste_initiatives
        ),
        popupOptions = popupOptions(className = "custom-popup")
      ) %>%
      addLegend(
        "bottomright",
        pal = colorNumeric("plasma", initiatives_par_pays$nb),
        values = initiatives_par_pays$nb,
        title = "Initiatives Count"
      ) %>%
      setView(
        lng = mean(initiatives_par_pays$Longitude, na.rm = TRUE),
        lat = mean(initiatives_par_pays$Latitude, na.rm = TRUE),
        zoom = 2
      )
  })
  
  # Render the governance map with pie charts
  output$comm_gov <- renderLeaflet({
    data <- data_reactive()
    
    map <- leaflet() %>%
      addTiles() %>%
      addMinicharts(
        lng = data_pie$Longitude, lat = data_pie$Latitude,
        type = "pie",
        chartdata = data_pie[, c("Yes", "No")],
        colorPalette = c("blue", "red"),
        width = 30, height = 30,
        opacity = 0.8
      ) %>%
      setView(
        lng = mean(initiatives_par_pays$Longitude, na.rm = TRUE),
        lat = mean(initiatives_par_pays$Latitude, na.rm = TRUE),
        zoom = 2
      )
  })
  
  # Reactive computation of MCA (Multiple Correspondence Analysis) results
  mca_results <- reactive({
    perform_mca(data_reactive())  # Calls your custom MCA computation function
  })
  
  # Render the MCA plot for NonProfit dimension
  output$mca_plot_ui <- renderUI({
    acm_object <- mca_results()$acm
    d <- mca_results()$d
    
    rownames(acm_object$li) <- rownames(d)  # Sync labels with source data
    res <- prepare_results(acm_object)
    
    MCA_ind_plot(res,
                 xax = 1, yax = 2,
                 ind_sup = FALSE,
                 lab_var = "Lab",
                 ind_lab_min_contrib = 0,
                 col_var = "Nonprofit",
                 labels_size = 7,
                 point_opacity = 0.5,
                 opacity_var = NULL,
                 point_size = 64,
                 ellipses = FALSE,
                 transitions = TRUE,
                 labels_positions = "auto",
                 xlim = c(-2.27, 2.39),
                 ylim = c(-1.46, 1.5))
  })
  
  # Render the MCA plot for Category dimension
  output$mca_plot_ui2 <- renderUI({
    acm_object <- mca_results()$acm
    d <- mca_results()$d
    
    rownames(acm_object$li) <- rownames(d)
    res <- explor::prepare_results(acm_object)
    
    MCA_ind_plot(res,
                 xax = 1, yax = 2,
                 ind_sup = FALSE,
                 lab_var = "Lab",
                 ind_lab_min_contrib = 0,
                 col_var = "Category",
                 labels_size = 7,
                 point_opacity = 0.5,
                 opacity_var = NULL,
                 point_size = 64,
                 ellipses = FALSE,
                 transitions = TRUE,
                 labels_positions = "auto",
                 xlim = c(-2.27, 2.39),
                 ylim = c(-1.46, 1.5))
  })
  
  # Render the dendrogram for hierarchical clustering
  output$dendrogram <- renderPlotly({
    data <- data_reactive()
    
    p1 <- factoextra::fviz_dend(
      arbre_phi2,
      show_labels = TRUE,
      k = 4,
      rect = TRUE
    ) +
      ggplot2::ggtitle("Dendrogram (cut into 4 clusters)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p1)
  })
  
  # Observe new data, compute clusters and sync to Google Sheets
  observeEvent(data_reactive(), {
    data <- data_reactive()
    
    clustered_data <- data %>%
      mutate(cluster = cutree(arbre_phi2, k = 4))
    
    sheet_id <- "1WWY-AFsFY70xf7JgRAZFwjl7tcHcdCplb8QT5bb3-k8"
    write_sheet(clustered_data, ss = sheet_id, sheet = "Clusters")
    
    cat("✅ Clusters updated in Google Sheet.\n")
  })
  
  #############################
  output$monitoring_table <- DT::renderDataTable({
    df <- monitoring_data()
    
    # Lien cliquable avec icône
    df$Link <- ifelse(
      is.na(df$Link) | df$Link == "",
      "",
      paste0("<a href='", df$Link, "' target='_blank'><i class='fa fa-external-link-alt'></i> Visit</a>")
    )
    
    # Badges de type
    df$Type <- case_when(
      df$Type == "National" ~ "<span class='badge badge-primary'>National</span>",
      df$Type == "International" ~ "<span class='badge badge-success'>International</span>",
      df$Type == "Funder" ~ "<span class='badge badge-warning'>Funder</span>",
      df$Type == "Institutional" ~ "<span class='badge badge-info'>Institution</span>",
      df$Type == "Publisher" ~ "<span class='badge badge-danger'>Publisher</span>",
      df$Type == "Specialised" ~ "<span class='badge badge-secondary'>Specialised</span>",
      TRUE ~ as.character(df$Type)
    )
    
    DT::datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '25%', targets = 0),  # Initiative
          list(width = '10%', targets = 1),  # Type
          list(width = '10%', targets = 2),  # Link
          list(width = '55%', targets = 3)   # Description
        )
      ),
      #class = 'cell-border stripe hover nowrap' 
      class = 'display nowrap compact stripe'
    )
  })
  
  #################################################
  output$focus_table <- DT::renderDataTable({
    df <- df_focus()
    
    # Drapeaux sous forme d’image
    df$`Country/Region` <- ifelse(
      is.na(df$Flag) | df$Flag == "",
      df$`Country/Region`,
      paste0("<img src='", df$Flag, "' width='24' style='margin-right:6px;'>", df$`Country/Region`)
    )
    
    # Lien stylisé
    df$Link <- ifelse(
      is.na(df$Link) | df$Link == "",
      "",
      paste0("<a class='btn-visit' target='_blank' href='", df$Link, "'>🔗 Visit</a>")
    )
    
    df <- df %>% select(Initiative, `Country/Region`, Group, Type, Description, Link)
    
    df$Type <- dplyr::case_when(
      df$Type == "National policy" ~ "<span class='badge badge-primary'>National policy</span>",
      df$Type == "National dashboard" ~ "<span class='badge badge-info'>National dashboard</span>",
      df$Type == "Funding / Coordination" ~ "<span class='badge badge-success'>Funding</span>",
      df$Type == "Funding programme" ~ "<span class='badge badge-success'>Funding</span>",
      df$Type == "Infrastructure" ~ "<span class='badge badge-dark'>Infrastructure</span>",
      df$Type == "Information system" ~ "<span class='badge badge-warning'>Info system</span>",
      df$Type == "National programme" ~ "<span class='badge badge-primary'>Programme</span>",
      df$Type == "International initiative" ~ "<span class='badge badge-secondary'>International</span>",
      df$Type == "Publication platform" ~ "<span class='badge badge-purple'>Platform</span>",
      df$Type == "Monitoring" ~ "<span class='badge badge-danger'>Monitoring</span>",
      TRUE ~ as.character(df$Type)
    )
    
    
    DT::datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      extensions = 'Responsive',
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        responsive = TRUE,
        columnDefs = list(
          list(className = 'dt-left', targets = "_all"),
          list(width = '10%', targets = 0),
          list(width = '10%', targets = 1),
          list(width = '10%', targets = 2),
          list(width = '15%', targets = 3),
          list(width = '40%', targets = 4, className = 'wrap-text'),
          list(width = '10%', targets = 5)
        )
      ),
      class = 'display nowrap compact stripe'
    )
  }) 
}


# Launch the application
shinyApp(ui = ui, server = server)


# To deploy the app online 
# rsconnect::deployApp(appDir = "C:/Users/amaddi/Documents/Projets financés/OPENIT/openit/osinit_app/osinit", appName = "openit")

