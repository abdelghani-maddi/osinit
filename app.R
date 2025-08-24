#######################
# Clear the workspace
#######################
rm(list = ls())  # Removes all variables from the workspace

#######################
# Load necessary libraries -----
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
library(ggdendro)
library(dendextend)
library(shinyWidgets)
library(bslib)
library(colorspace)

#######################
# Set up Shiny app credentials -----
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
row.names(data) <- data$Abbreviated_OrgName

#######################
# Group Initiatives by Country
#######################
# Group data by country and summarize by counting the number of initiatives
initiatives_par_pays <- data %>%
  group_by(Country, ISO3, Latitude, Longitude) %>%
  summarise(
    nb = n(),  # Count number of initiatives per country
    liste_initiatives = paste(Abbreviated_OrgName, collapse = "<br>"),  # Concatenate organization names
    .groups = "drop"  # Drop the grouping structure
  )

# Create a color palette based on the number of initiatives
pal <- colorNumeric("plasma", domain = initiatives_par_pays$nb)

#######################
# Prepare Data for Community Governance Visualization
#######################
# 1. Concatène les initiatives par pays et par type Yes/No
list_initiatives_cg <- data %>%
  group_by(ISO3, Country, Longitude, Latitude, CommunityGovernance) %>%
  summarise(
    list_initiatives = paste(unique(Abbreviated_OrgName), collapse = "<br>"),
    nb_cg = n(),
    .groups = "drop"
  )

# 2. Met en forme large pour avoir colonnes Yes et No pour listes et nombres
data_cg_wide <- list_initiatives_cg %>%
  pivot_wider(
    id_cols = c(ISO3, Country, Longitude, Latitude),
    names_from = CommunityGovernance,
    values_from = c(nb_cg, list_initiatives),
    values_fill = list(nb_cg = 0, list_initiatives = "No initiative")
  )

# 3. Nettoyage noms colonnes (optionnel)
names(data_cg_wide) <- gsub("nb_cg_", "", names(data_cg_wide))
names(data_cg_wide) <- gsub("list_initiatives_", "list_", names(data_cg_wide))


#######################
# Perform Multiple Correspondence Analysis (MCA)
#######################
# Function to perform MCA (Correspondence Analysis)
perform_mca <- function(data) {
  d <- data %>%
    select(Abbreviated_OrgName, Nonprofit, Category, CommunityGovernance) %>%
    mutate(across(everything(), as.factor))  # Convert columns to factors for MCA
  
  d$Abbreviated_OrgName <- substr(d$Abbreviated_OrgName, 1, 35)
  
  row.names(d) <- d$Abbreviated_OrgName

  d <- d %>% select(-Abbreviated_OrgName)
  
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
data$cluster <- cutree(arbre_phi2, 10)  # Cut the dendrogram into 7 clusters


#######################
# Define Custom CSS for Popups in Leaflet Maps
#######################
popup_css <- "
.custom-popup .leaflet-popup-content {
  max-height: 250px;
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
# User Interface - Open Science Initiatives Dashboard ----
# ================================================

ui <- tagList(
  
  #===========================
  # HEAD TAGS: Metadata, favicon, and sidebar hover styles
  #===========================
  tags$head(
    # Set favicon and page title
    # tags$link(rel = "icon", type = "image/png", 
    #           href = "https://upload.wikimedia.org/wikipedia/commons/f/f0/Cadenas-ouvert-vert.svg"),
    # tags$title("OS Initiatives Tracker"),
    
      # # Set favicon and page title
      tags$link(rel = "icon", type = "image/png", href = "hub6.png"),
      tags$title("COSMI Hub"),

    
    # Custom hover color styles for each sidebar menu item
    tags$style(HTML("
      .sidebar-menu li:nth-child(1) a:hover { background-color: #ba3470 !important; color: white !important; }
      .sidebar-menu li:nth-child(2) a:hover { background-color: #17a2b8 !important; color: white !important; }
      .sidebar-menu li:nth-child(3) a:hover { background-color: #ffc107 !important; color: black !important; }
      .sidebar-menu li:nth-child(4) a:hover { background-color: #8aa728 !important; color: white !important; }
      .sidebar-menu li:nth-child(5) a:hover { background-color: #a72859 !important; color: white !important; }
      .sidebar-menu li:nth-child(6) a:hover { background-color: #288aa7 !important; color: white !important; }
      .sidebar-menu li:nth-child(7) a:hover { background-color: #8aa728 !important; color: white !important; }
      .sidebar-menu li:nth-child(8) a:hover { background-color: #288aa7 !important; color: white !important; }
    ")),
    
    tags$meta(property = "og:image", content = "https://amcm.shinyapps.io/openit/overview.png"),
    tags$meta(property = "og:url", content = "https://amcm.shinyapps.io/openit/"),
    tags$meta(property = "og:type", content = "website")
    
  ),
  
  #===========================
  # DASHBOARD LAYOUT
  #===========================
  dashboardPage(    
    
    #===========================
    # HEADER with App Title + Overview Button
    #===========================
    dashboardHeader(
      title = tags$strong("COSMI Hub"),
      titleWidth = 230,
      
      # Navigation button: Overview
      tags$li(
        class = "dropdown",
        actionButton(
          inputId = "go_overview",
          label = tagList(icon("home"), "Overview"),
          class = "btn btn-outline-light",
          style = "margin-top: 8px; margin-right: 12px; padding: 4px 8px; font-size: 13px;"
        )
      ),
      
      tags$li(
        class = "dropdown",
        style = "display: flex; gap: 15px; align-items: center; margin-left: 10px;",
        tags$a(
          id = "share_linkedin",
          href = "https://www.linkedin.com/sharing/share-offsite/?url=https://amcm.shinyapps.io/openit/preview.html",
          target = "_blank",
          style = "background-color: white; color: #0077b5; font-size: 20px; padding: 7px; border-radius: 50% 0 0 50%;",
          icon("linkedin")
        ),
        tags$a(
          id = "share_bluesky",
          href = "https://bsky.app/intent/compose?text=https://amcm.shinyapps.io/openit/preview.html",
          target = "_blank",
          style = "background-color: white; color: #1da1f2; font-size: 20px; padding: 7px; border-radius: 0 50% 50% 0;",
          icon("bluesky")  
        )
      )
      
    ),
    
    #===========================
    # SIDEBAR MENU
    #===========================
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",  # Tab navigation identifier
        
        # 1. Global Overview
        menuItem(" Global overview", tabName = "overview", icon = icon("globe")),
        
        # 2. Explore Section
        menuItem("Explore Initiatives", icon = icon("map"),
                 menuSubItem("By Country / Region", tabName = "by_country"),
                 menuSubItem("By Category", tabName = "by_category")
                 
        ),
        
        # 3. Analytical Section
        menuItem("Analyze Structure", icon = icon("project-diagram"),
                 menuSubItem("Correspondence Analysis", tabName = "mca"),
                 menuSubItem("Typology – Clustering", tabName = "hcpc")
        ),
        
        # 4. Monitoring Section
        menuItem("Monitoring Framework", icon = icon("balance-scale"),
                 menuSubItem("OSMI Principles", tabName = "principles"),
                 menuSubItem("Monitoring Landscape", tabName = "monitoring")
        ),
        
        # 5. Contribution & Data
        menuItem("Contribute & Data", icon = icon("users"),
                 menuSubItem("Suggest Initiative", tabName = "add_initiative"),
                 menuSubItem("Report an error", tabName = "report_error"),
                 menuSubItem("Download Dataset", tabName = "download_data"),
                 menuSubItem("Source Code (GitHub)", 
                             href = "https://github.com/abdelghani-maddi/osinit", 
                             newtab = TRUE)
        ),
        
        # 6. Resources
        menuItem("Resources", icon = icon("book"),
                 menuSubItem("About the Project", tabName = "about"),
                 menuSubItem("FAQs", tabName = "FAQs")
        ),
        
        # # 7. Logos (external links)
        # tags$li(class = "dropdown", 
        #         tags$a(href = "https://www.gemass.fr/contract/openit/", target = "_blank",
        #                tags$img(src = "logo.png", height = "47px", style = "margin: 0px; display: block;")
        #         )
        # ),
        # tags$li(class = "dropdown", 
        #         tags$a(href = "https://open-science-monitoring.org/", target = "_blank",
        #                tags$img(src = "osmi.png", height = "44px", style = "margin: 0px; display: block;")
        #         )
        # ),
        
        # 8. Logos fixed to bottom of sidebar (ANR + CC-BY)
        tags$div(
          style = "position: absolute; bottom: 20px; left: 10px;",
          # tags$a(href = "https://anr.fr/Projet-ANR-24-RESO-0001", target = "_blank",
          #        tags$img(src = "logo_ANR.jpg", height = "33px", style = "margin: 5px; display: block;")
          # ),
          tags$a(href = "https://creativecommons.org/licenses/by/4.0/deed.fr", target = "_blank",
                 tags$img(src = "CC_BY.png", height = "30px", style = "margin: 5px; display: block;")
          )
        )
      )
    ),
    
    #===========================
    # BODY STYLES & CUSTOM CSS
    #===========================
    dashboardBody(
      id = "tabs",
      
      tags$head(
        # Favicon + Title (again for redundancy)
        # tags$link(rel = "icon", type = "image/png",
        #           href = "https://upload.wikimedia.org/wikipedia/commons/f/f0/Cadenas-ouvert-vert.svg"),
        # tags$title("OS Initiatives Tracker"),
        
        tags$link(rel = "icon", type = "image/png", href = "hub6.png"),
        tags$title("COSMI Hub"),
        
        # ---- Custom Section Styles ----
        tags$style(HTML("
          .interactive-section-box {
            position: relative;
            border-radius: 20px;
            color: white;
            text-align: center;
            padding: 30px 20px;
            margin-bottom: 20px;
            overflow: hidden;
            height: 240px;
            box-shadow: 0 6px 20px rgba(0, 0, 0, 0.1);
            transition: all 0.3s ease-in-out;
          }

          .section-main .section-icon {
            font-size: 50px;
            margin-bottom: 15px;
          }

          .section-main .section-title {
            font-size: 20px;
            font-weight: bold;
          }

          .section-main .section-subtitle {
            font-size: 14px;
            opacity: 0.85;
          }

          .section-hover-menu {
            position: absolute;
            top: 0; left: 0; right: 0; bottom: 0;
            background-color: rgba(255,255,255,0.95);
            color: #333;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            opacity: 0;
            transition: opacity 0.3s ease-in-out;
            border-radius: 20px;
            padding: 20px;
          }

          .interactive-section-box:hover .section-hover-menu {
            opacity: 1;
          }

          .explore-box     { background: linear-gradient(135deg, #1e88e5, #90caf9); }  /* Blue */
          .analyze-box     { background: linear-gradient(135deg, #f57c00, #ffe0b2); }  /* Orange */
          .monitor-box     { background: linear-gradient(135deg, #43a047, #c8e6c9); }  /* Green */
          .contribute-box  { background: linear-gradient(135deg, #d81b60, #f8bbd0); }  /* Pink */
          .dataset-box     { background: linear-gradient(135deg, #fbc02d, #fff9c4); }  /* Yellow */
          .about-box       { background: linear-gradient(135deg, #5e35b1, #d1c4e9); }  /* Purple */

          .btn-explore {
            background-color: #007bff;
            color: white;
            border: none;
            margin: 10px;
            width: 80%;
            padding: 10px;
            border-radius: 8px;
            font-size: 14px;
            transition: background-color 0.3s ease;
          }

          .btn-explore:hover {
            background-color: #0056b3;
          }
        ")),
        
        # ---- Badge Styles for Tables ----
        tags$style(HTML("
          .badge {
            display: inline-block;
            font-size: 13px;
            padding: 6px 10px;
            border-radius: 12px;
            color: white;
            margin-right: 5px;
          }

          .badge-primary   { background-color: #007bff; }
          .badge-success   { background-color: #28a745; }
          .badge-warning   { background-color: #ffc107; color: black; }
          .badge-info      { background-color: #17a2b8; }
          .badge-danger    { background-color: #dc3545; }
          .badge-secondary { background-color: #6c757d; }
        ")),
        
        # ---- Table Cell Wrapping ----
        tags$style(HTML("
          .wrap-text {
            white-space: normal !important;
            word-wrap: break-word;
          }

          table.dataTable td {
            vertical-align: top;
            font-size: 14px;
            white-space: normal !important;
            word-break: break-word;
            line-height: 1.4;
          }
        ")),
        
        # ---- MCA Plot Container Overflow Fix ----
        tags$style(HTML("
          .mca-box-container {
            overflow-x: auto;
          }
        ")),
        
        # ---- Header Height Adjustment ----
        tags$style(HTML("
          .main-header .navbar {
            min-height: 50px !important;
            height: 50px !important;
          }
        "))
        
        
        
        ),


# ================================================
# Dashboard TabItems ----
# ================================================
tabItems(
  
  # --------------------------------------------------
  # Global Overview Tab -----
  # --------------------------------------------------
  tabItem(tabName = "overview",
          
          # -----------------------------
          # OVERVIEW INTRO BOX – Enhanced
          # -----------------------------
          fluidRow(
            box(
              title = div(
                icon("globe-europe", style = "margin-right: 10px;"),  # Icon before title
                "Welcome to the Community Open Science Mapping Initiatives (COSMI) Hub"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              style = "border-radius: 30px; box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1); background: linear-gradient(to right, #f4f9ff, #ffffff);",
              
              # Intro text block
              div(
                style = "font-size:17px; line-height:1.7; padding:20px;",
                HTML("
            <p><strong>This dashboard provides a dynamic and community-driven overview of Open Science initiatives around the world.</strong></p>
            <p>It is designed to support the discovery, classification, and comparative analysis of initiatives promoting openness in scholarly communication.</p>
            <p>You can navigate using the <strong>sidebar menu</strong> or the <strong>interactive cards</strong> just below.</p>
          ")
              ),
              
              # Centered total initiatives count + action buttons
              tags$div(
                style = "text-align: center; padding-bottom: 40px;",

                tags$i(class = "fa fa-chart-line", style = "font-size: 100px; color: #007bff; margin-bottom: 10px;"),
# fa fa-chart-line  # fa fa-users 
                tags$div(
                  style = "font-size: 100px; font-weight: bold; color: #007bff;",
                  textOutput("distinct_initiatives")
                ),

                tags$div(
                  style = "font-size: 25px; font-weight: bold; color: #333;",
                  "Total number of initiatives included"
                ),

                # Two call-to-action buttons
                tags$div(
                  style = "margin-top: 25px;",

                  tags$a(
                    href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit#gid=1136935931",
                    target = "_blank",
                    icon("database", class = "me-2"),
                    "View & Download Dataset",
                    style = "color: #ffffff; text-decoration: none; display: inline-block; margin: 10px; padding: 12px 20px;
                       background-color: #007bff; border-radius: 12px; font-size: 16px; font-weight: 500;"
                  ),

                  tags$a(
                    href = "https://forms.gle/ZSnK9XkaVMBnKfPS6",
                    target = "_blank",
                    icon("plus-square", class = "me-2"),
                    "Suggest New Initiative",
                    style = "color: #000000; text-decoration: none; display: inline-block; margin: 10px; padding: 12px 20px;
                       background-color: #aaff66; border-radius: 12px; font-size: 16px; font-weight: 500;"
                  ),
                  
                  tags$a(
                    href = "https://docs.google.com/forms/d/1iDJp9iMNSfG2ur47i6122QphXxssL34-my_m2vDusoI/preview",
                    target = "_blank",
                    icon("exclamation-triangle", class = "me-2"),
                    "Report an Error",
                    style = "color: #ffffff; text-decoration: none; display: inline-block; margin: 10px; padding: 12px 20px;
                      background-color: #ff6961; border-radius: 12px; font-size: 16px; font-weight: 500;"
                  )
                )
              )
            )
          ),
          
          # -----------------------------
          # VISUAL NAVIGATION CARDS (1)
          # -----------------------------
          fluidRow(
            
            # EXPLORE BOX
            column(
              width = 4,
              div(
                class = "interactive-section-box explore-box section-main",
                tags$i(class = "fas fa-map-marked-alt section-icon"),
                div(class = "section-title", "Explore Initiatives"),
                div(class = "section-subtitle", "By country, map, category"),
                div(class = "section-hover-menu",
                    actionButton("goto_by_country", "By Country / Region", class = "btn-explore"),
                    actionButton("goto_by_category", "By Category", class = "btn-explore")
                )
              )
            ),
            
            # ANALYZE BOX
            column(
              width = 4,
              div(
                class = "interactive-section-box analyze-box section-main",
                tags$i(class = "fas fa-project-diagram section-icon"),
                div(class = "section-title", "Analyze Structure"),
                div(class = "section-subtitle", "MCA and clustering"),
                div(class = "section-hover-menu",
                    actionButton("goto_mca", "Correspondence Analysis", class = "btn-explore"),
                    actionButton("goto_hcpc", "Typology – Clustering", class = "btn-explore")
                )
              )
            ),
            
            # MONITOR BOX
            column(
              width = 4,
              div(
                class = "interactive-section-box monitor-box section-main",
                tags$i(class = "fas fa-balance-scale section-icon"),
                div(class = "section-title", "Monitoring Framework"),
                div(class = "section-subtitle", "OSMI principles & landscape"),
                div(class = "section-hover-menu",
                    actionButton("goto_principles", "OSMI Principles", class = "btn-explore"),
                    actionButton("goto_monitoring", "Monitoring Landscape", class = "btn-explore")
                )
              )
            )
          ),
          
          # -----------------------------
          # VISUAL NAVIGATION CARDS (2)
          # -----------------------------
          fluidRow(
            
            # CONTRIBUTE BOX
            column(
              width = 4,
              div(
                class = "interactive-section-box contribute-box section-main",
                tags$i(class = "fas fa-plus-circle section-icon"),
                div(class = "section-title", "Contribute"),
                div(class = "section-subtitle", "Add initiatives / Report an error"),
                div(class = "section-hover-menu",
                    actionButton("goto_add_initiative", "Suggest Initiative", class = "btn-explore"),
                    actionButton("goto_report_error", "Report an Error", class = "btn-explore")
                    
                )
              )
            ),
            

            
            # DATASET BOX
            column(
              width = 4,
              div(
                class = "interactive-section-box dataset-box section-main",
                tags$i(class = "fas fa-database section-icon"),
                div(class = "section-title", "Dataset Access"),
                div(class = "section-subtitle", "View or download the full dataset"),
                div(class = "section-hover-menu",
                    
                    # Bouton pour accéder au dataset 
                    actionButton("goto_download_data", "Access Dataset", class = "btn-explore"),
                    
                    # Lien direct vers le code source sur GitHub
                    tags$a(
                      href = "https://github.com/abdelghani-maddi/osinit/blob/main/app.R",  
                      target = "_blank",
                      class = "btn-explore",
                      "Source code (Github)"
                    )
                )
              )
            ),
            
            
            # ABOUT BOX
            column(
              width = 4,
              div(
                class = "interactive-section-box about-box section-main",
                tags$i(class = "fas fa-info-circle section-icon"),
                div(class = "section-title", "Resources"),
                div(class = "section-subtitle", "Project description and FAQs"),
                div(class = "section-hover-menu",
                    actionButton("goto_about", "About the Project", class = "btn-explore"),
                    actionButton("goto_faqs", "FAQs", class = "btn-explore")
                )
              )
            )
          ),
          
          # Bande horizontale claire avec logos cliquables
          fluidRow(
            tags$div(
              style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
              
              # Logos avec liens 
              tags$a(
                href = "https://anr.fr/Projet-ANR-24-RESO-0001",
                target = "_blank",
                tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
              ),
              tags$a(
                href = "https://www.gemass.fr/contract/openit/",
                target = "_blank",
                tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
              )
              # ,
              # tags$a(
              #   href = "https://open-science-monitoring.org/",
              #   target = "_blank",
              #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
              # )
            )
          )
  
  ),
  
  # --------------------------------------------------
  # Suggest Initiative Tab -----
  # --------------------------------------------------
  tabItem(tabName = "add_initiative",
          fluidRow(
            box(
              title = "Suggest a New Initiative",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              
              # Intro text and embedded Google Form
              div(
                style = "padding: 10px; font-size: 16px;",
                tags$p("Use the embedded form below to suggest a new Open Science initiative for inclusion in our dashboard."),
                tags$iframe(
                  src = "https://docs.google.com/forms/d/e/1FAIpQLSfCNlKAj8BV-6d9Ls5SiG1tQVEBU28NTYqztt3jszbJQX5gyA/viewform?embedded=true",
                  width = "100%",
                  height = "1000px",
                  frameborder = "0",
                  style = "border: none;"
                )
              )
            )
          ),
          
          # Bande horizontale claire avec logos cliquables
          fluidRow(
            tags$div(
              style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
              
              # Logos avec liens 
              tags$a(
                href = "https://anr.fr/Projet-ANR-24-RESO-0001",
                target = "_blank",
                tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
              ),
              tags$a(
                href = "https://www.gemass.fr/contract/openit/",
                target = "_blank",
                tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
              )
              # ,
              # tags$a(
              #   href = "https://open-science-monitoring.org/",
              #   target = "_blank",
              #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
              # )
            )
          )
  ),
  
  # --------------------------------------------------
  # Report error Tab -----
  # --------------------------------------------------
  
  tabItem(tabName = "report_error",
          fluidRow(
            box(
              title = "Report an Error or Suggest a Correction",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              
              # Intro text
              div(
                style = "padding: 10px; font-size: 16px;",
                tags$p("If you find an error or outdated information in the dashboard, please use the form below to report it or suggest a correction."),
                tags$p("Your feedback helps us keep the data accurate and up-to-date. Thank you!")
              ),
              
              # Embedded Google Form
              tags$iframe(
                src = "https://docs.google.com/forms/d/1iDJp9iMNSfG2ur47i6122QphXxssL34-my_m2vDusoI/preview",
                width = "100%",
                height = "800px",
                frameborder = "0",
                style = "border: none;"
              )
            )
          ),
          
          # Bande horizontale claire avec logos cliquables
          fluidRow(
            tags$div(
              style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
              
              # Logos avec liens 
              tags$a(
                href = "https://anr.fr/Projet-ANR-24-RESO-0001",
                target = "_blank",
                tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
              ),
              tags$a(
                href = "https://www.gemass.fr/contract/openit/",
                target = "_blank",
                tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
              )
              # ,
              # tags$a(
              #   href = "https://open-science-monitoring.org/",
              #   target = "_blank",
              #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
              # )
            )
          )
  ),
  
  
  # --------------------------------------------------
  # Download Dataset Tab -----
  # --------------------------------------------------
  tabItem(tabName = "download_data",
          fluidRow(
            box(
              title = "Access the Dataset",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              
              # Embedded Google Sheet + external access button
              div(
                style = "padding: 10px; font-size: 16px;",
                
                # Top-right button to open the dataset in Google Sheets
                tags$div(
                  style = "margin-bottom: 15px; text-align: right;",
                  tags$a(
                    href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=1136935931",
                    target = "_blank",
                    class = "btn btn-primary",
                    icon("external-link-alt"),
                    "Open in Google Sheets"
                  )
                ),
                
                # Embedded live preview of the dataset
                tags$p("Below is a live preview of the dataset. Use the button above to open the full version for download or copy."),
                tags$iframe(
                  src = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/preview",
                  width = "100%",
                  height = "800px",
                  frameborder = "0",
                  style = "border: none;"
                )
              )
            )
          ),
          
          # Bande horizontale claire avec logos cliquables
          fluidRow(
            tags$div(
              style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
              
              # Logos avec liens 
              tags$a(
                href = "https://anr.fr/Projet-ANR-24-RESO-0001",
                target = "_blank",
                tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
              ),
              tags$a(
                href = "https://www.gemass.fr/contract/openit/",
                target = "_blank",
                tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
              )
              # ,
              # tags$a(
              #   href = "https://open-science-monitoring.org/",
              #   target = "_blank",
              #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
              # )
            )
          )
  ),


# --------------------------------------------------
# Initiatives by Category Tab -----
# --------------------------------------------------
tabItem(tabName = "by_category",
        
        # Bar Chart: Distribution by category
        fluidRow(
          box(
            title = "Initiatives by Category", 
            status = "info", 
            solidHeader = TRUE, 
            width = 12,

            # Interactive Plotly chart
            plotlyOutput("category_plot",  height = "1000px"),
            
            # Description under the plot
            tags$p(
              "This bar chart provides an overview of the number of initiatives grouped by their focus area. Categories for the focus area are based on the first level of the open science Taxonomy by da Silveira et al. (2023). You can click on a bar to explore the specific initiatives within the selected category."
            )
          )
        ),
        
        # List of initiatives selected by clicking on a category
        fluidRow(
          box(
            title = "Selected Initiatives List",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            # Rendered list of clicked initiatives
            uiOutput("clicked_initiatives"),
            
            tags$p(
              "Click on a bar above to view the initiatives belonging to the selected category.",
              style = "color: #555;"
            )
          )
        ),
        
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
),


# --------------------------------------------------
# Global Distribution of Initiatives Tab -----
# --------------------------------------------------
tabItem(tabName = "by_country",
        
        # World Map of initiatives
        fluidRow(
          column(
            width = 12,
            box(
              title = "Global Distribution of Initiatives",
              status = "success",
              solidHeader = TRUE,
              width = NULL,

              # Leaflet interactive map
              leafletOutput("world_map"),
              
              # Apply custom popup CSS 
              tags$style(HTML(popup_css)),
              
              # Descriptive paragraph below the map
              tags$p(
                "This map visualizes the geographical distribution of open science initiatives around the world. 
          Click on the bubbles to view the number of initiatives per country and a list of the specific projects."
              )
            )
          )
        ),
        
        # Community governance map
        fluidRow(
          column(
            width = 12,
            box(
              title = "Community Governance Map",
              status = "warning",
              solidHeader = TRUE,
              width = NULL,
              
              # Second map focused on governance types
              leafletOutput("comm_gov"),
              
              tags$p(
                "This map displays a comparison of community governance practices in open science initiatives. 
          The pie charts represent the proportion of initiatives with and without community-driven governance structures."
              )
            )
          )
        ),
        
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
),


# --------------------------------------------------
# Monitoring Principles Tab -----
# --------------------------------------------------
tabItem(tabName = "principles",
        
        # ---- CSS STYLING FOR CARDS ----
        tags$head(
          tags$style(HTML("
      .section-card {
        border-radius: 16px;
        box-shadow: 0 6px 20px rgba(0, 0, 0, 0.08);
        padding: 20px;
        height: 100%;
        transition: all 0.3s ease-in-out;
        cursor: pointer;
        color: #fff;
      }
      .section-card:hover {
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.12);
        transform: translateY(-5px);
      }
      .section-icon { font-size: 40px; margin-bottom: 10px; }
      .section-title { font-size: 20px; font-weight: bold; margin-bottom: 5px; }
      .section-subtitle { font-size: 14px; opacity: 0.85; }

      .explore-card    { background-color: #007bff; }   /* Blue */
      .analyze-card    { background-color: #6f42c1; }   /* Purple */
      .monitor-card    { background-color: #17a2b8; }   /* Teal */
      .contribute-card { background-color: #28a745; }   /* Green */
      .dataset-card    { background-color: #20c997; }   /* Mint */
      .about-card      { background-color: #fd7e14; }   /* Orange */
    "))
        ),
        
        # ---- CSS STYLING FOR ACCORDIONS ----
        tags$head(
          tags$style(HTML("
      .principle-section {
        padding: 50px 40px;
        background-color: #f9fafb;
        background-image: linear-gradient(to bottom right, #f0f4f8, #d9e2ec);
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
      .accordion-content ul { padding-left: 20px; }
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
        
        # ---- CONTENT SECTION ----
        fluidRow(
          column(12,
                 
                 # Main section container
                 div(class = "principle-section",
                
                   div(
                     style = "background-color: rgba(255, 255, 255, 0.9); padding: 20px 30px; border-radius: 12px; 
          display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap;",
                     div(
                       style = "flex: 1; min-width: 250px;",
                       div(class = "principle-title-main", "Principles of Open Science Monitoring"),
                       div(class = "principle-subtitle", 
                           "Explore the foundational dimensions of ethical, inclusive and sustainable monitoring practices.")
                     ),
                     tags$a(
                       href = "https://open-science-monitoring.org/",
                       target = "_blank",  
                       tags$img(src = "logo-osmi.png", class = "osmi-logo-small")
                     )
                   ),
                   
                     br(),
                     
                     # ----- Accordion 1: Relevance and Significance -----
                     tags$details(class = "accordion-card", style = "border-left: 6px solid #3b82f6;",
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
                     
                     # ----- Accordion 2: Transparency and Reproducibility -----
                     tags$details(class = "accordion-card", style = "border-left: 6px solid #d4af37;",
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
                     
                     # ----- Accordion 3: Self-Assessment and Responsible Use -----
                     tags$details(class = "accordion-card", style = "border-left: 6px solid #8b5cf6;",
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
                     
                     # ---- Footer with Source Link ----
                     div(class = "source-box",
                         tags$em("Source: Open Science Monitoring Initiative, 2025"),
                         tags$br(),
                         tags$a(
                           href = "https://doi.org/10.5281/zenodo.15807481",
                           target = "_blank",
                           "Access full document"
                         )
                     )
                 )
          )
        ),
        
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
),


# --------------------------------------------------
# MCA Tab: Multiple Correspondence Analysis -----
# --------------------------------------------------
tabItem(tabName = "mca",
        fluidRow(
          # ---- MCA Plot: By Category ----
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
          
          # ---- MCA Interpretation: By Category ----
          column(width = 4.5,
                 box(
                   title = "How to Interpret the MCA Plot (Category)",
                   status = "info",
                   solidHeader = TRUE,
                   width = 12,
                   tags$div(
                     style = "font-size: 15px; line-height: 1.6;",
                     tags$p("This plot shows the same MCA projection, but colored by the main category of each initiative."),
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
          # ---- MCA Plot: NonProfit Status ----
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
          
          # ---- MCA Interpretation: NonProfit ----
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
                     )
                   )
                 )
          )
        ),
        
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
),    

# --------------------------------------------------
# HCPC Tab: Hierarchical Clustering -----
# --------------------------------------------------
tabItem(tabName = "hcpc",
        fluidRow(
          # ---- Dendrogram Plot ----
          column(width = 7.5,
                 box(
                   title = "Hierarchical Clustering Dendrogram",
                   status = "danger",
                   solidHeader = TRUE,
                   width = 12,
                   plotlyOutput("dendrogram", height = "7000px"),
                   tags$p(
                     "This dendrogram shows the hierarchical clustering of initiatives based on their characteristics. Clusters reveal how initiatives group by similarity."
                   )
                 )
          ),
          
          # ---- Dendrogram Interpretation ----
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
        
        # ---- External Link to Cluster Table ----
        fluidRow(
          box(
            title = "View Clusters",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            style = "cursor: pointer; text-align: center; font-size: 18px; border-radius: 15px; border: 1px solid #007bff;
               padding: 20px; transition: all 0.3s ease-in-out; color: white;",
            tags$a(
              href = "https://docs.google.com/spreadsheets/d/1WWY-AFsFY70xf7JgRAZFwjl7tcHcdCplb8QT5bb3-k8/edit?gid=998126494#gid=998126494",
              target = "_blank",
              "📥 Access Clusters on Google Sheets",
              style = "color: #ffffff; text-decoration: none; display: inline-block; padding: 15px 30px;
                 background-color: #007bff; border-radius: 10px; font-weight: bold; font-size: 16px;
                 box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); transition: background-color 0.3s ease;"
            ),
            tags$p(
              "Click the link above to explore the clustering results in more detail, including which initiatives belong to each cluster.",
              style = "margin-top: 15px; font-size: 16px; line-height: 1.6; color: #0a0303;
                 text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.2);"
            )
          )
        ),
        
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
),

# --------------------------------------------------
# Monitoring Tab: Monitoring Landscape Table -----
# --------------------------------------------------
tabItem(tabName = "monitoring",
        fluidRow(
          # ---- Table 1: Global Monitoring Initiatives ----
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
          # ---- Table 2: Regional Focus Table ----
          box(
            title = tagList(icon("globe"), "Focus by country/region: Open Science Initiatives"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            DT::dataTableOutput("focus_table"),
            tags$p(
              "This table presents major open science initiatives, including national dashboards, infrastructures, funding programmes, and collaborative platforms. It offers an entry point to explore regional efforts and synergies in open science.",
              style = "margin-top:10px; font-size:15px;"
            ),
            tags$a(
              href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=2146313781#gid=2146313781",
              target = "_blank",
              "📥 View or edit full Google Sheet here",
              style = "display:inline-block; margin-top:10px; font-size:16px;"
            )
          )
        ),
        
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
),

# --------------------------------------------------
# About Tab: Project Description (Embedded Google Doc)
# --------------------------------------------------
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
        ),
        
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
),

# --------------------------------------------------
# FAQs Tab: Frequently Asked Questions (Embedded Google Doc)
# --------------------------------------------------
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
         ),
        # Bande horizontale claire avec logos cliquables
        fluidRow(
          tags$div(
            style = "width: 100%; background-color: #ffffff; padding: 15px 0; text-align: center; border-top: 1px solid #ddd;",
            
            # Logos avec liens 
            tags$a(
              href = "https://anr.fr/Projet-ANR-24-RESO-0001",
              target = "_blank",
              tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", style = "height:65px; margin: 0 15px; vertical-align: middle;")
            ),
            tags$a(
              href = "https://www.gemass.fr/contract/openit/",
              target = "_blank",
              tags$img(src = "logo-transparent.png", alt = "OPENIT", style = "height:70px; margin: 0 15px; vertical-align: middle;")
            )
            # ,
            # tags$a(
            #   href = "https://open-science-monitoring.org/",
            #   target = "_blank",
            #   tags$img(src = "logo-osmi old.png", alt = "OSMI", style = "height:50px; margin: 0 15px; vertical-align: middle;")
            # )
          )
        )
        )
      )
    )
  )
)

# =============================================
# Server Logic - Open Science Initiatives Dashboard
# =============================================

server <- function(input, output, session) {
  
  # 1. Lecture du paramètre ?tab=... au chargement
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$tab)) {
      updateTabItems(session, "tabs", selected = query$tab)
    }
  })

  # 2. Mise à jour de l'URL quand l’utilisateur change d’onglet
  observeEvent(input$tabs, {
    updateQueryString(
      paste0("?tab=", input$tabs),
      mode = "replace",  # "replace" pour ne pas ajouter à l'historique
      session = session
    )
  })
  
  
  
  
  observeEvent(input$goto_by_category, {
    updateTabItems(session, "tabs", "by_category")
  })
  observeEvent(input$goto_by_country, {
    updateTabItems(session, "tabs", "by_country")
  })
  observeEvent(input$goto_mca, {
    updateTabItems(session, "tabs", "mca")
  })
  observeEvent(input$goto_hcpc, {
    updateTabItems(session, "tabs", "hcpc")
  })
  observeEvent(input$goto_principles, {
    updateTabItems(session, "tabs", "principles")
  })
  observeEvent(input$goto_monitoring, {
    updateTabItems(session, "tabs", "monitoring")
  })
  observeEvent(input$goto_add_initiative, {
    updateTabItems(session, "tabs", "add_initiative")
  })
  observeEvent(input$goto_report_error, {
    updateTabItems(session, "tabs", "report_error")
  })
  observeEvent(input$goto_download_data, {
    updateTabItems(session, "tabs", "download_data")
  })
  observeEvent(input$goto_about, {
    updateTabItems(session, "tabs", "about")
  })
  observeEvent(input$goto_faqs, {
    updateTabItems(session, "tabs", "FAQs")
  })
  observeEvent(input$go_overview, {
    updateTabItems(session, inputId = "tabs", selected = "overview")
  })
  
  
  # Render the OPENIT logo image in the sidebar
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
    n_distinct(data$Abbreviated_OrgName)  # Count distinct initiatives
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
      geom_bar(stat = "identity") +
      geom_text(aes(label = count), vjust = 3, size = 4.5, fontface = "bold", color = "black") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 9)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.title = element_blank()
      ) +
      scale_fill_brewer(palette = "Set3") +
      labs(
        x= "",  #x = "Category",
        y = "Number of Initiatives",
        title = "Number of Initiatives by Category"
      )
    
    ggplotly(p, tooltip = "none", source = "select_bar") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,         # à gauche 
          y = -0.35,         # bas de la zone graphique
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 12),
          itemwidth = 50
        ),
        margin = list(b = 150)  # marge assez grande en bas pour que la légende ne soit pas coupée
      )
    
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
      filtered_data %>% 
        select(OrgName, Category, Country, 
               CommunityGovernance, Nonprofit, OpenSource),
      # extensions = c('Buttons', 'Scroller'),
      options = list(pageLength = 25, 
                     autoWidth = FALSE ,        
                     scrollX = TRUE #,
                     # dom = 'Bfrtip',
                     # buttons = c('copy', 'csv', 'excel')
                     ),
      rownames = FALSE
    )
  })
  
  # Render the world map with circle markers representing initiative counts
   output$world_map <- renderLeaflet({
    data <- data_reactive()
    
    map <- leaflet(initiatives_par_pays, options = leafletOptions(minZoom = 2)) %>%
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
      ) %>%
      setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)  # Empêche le scroll infini
    
    map
  })
  
  
   output$comm_gov <- renderLeaflet({
     data <- data_reactive()
     
     df <- data_cg_wide
     
     leaflet(df, options = leafletOptions(minZoom = 2)) %>%
       addTiles() %>%
       addMinicharts(
         lng = df$Longitude, lat = df$Latitude,
         type = "pie",
         chartdata = df[, c("Yes", "No")],
         colorPalette = c("blue", "red"),
         width = 30, height = 30,
         opacity = 0.8
       ) %>%
       addCircleMarkers(
         lng = df$Longitude, lat = df$Latitude,
         radius = 10,
         color = "transparent", fillOpacity = 0, opacity = 0,
         popup = ~paste0(
           "<div style='font-family: Arial, sans-serif; font-size: 14px;'>",
           
           # Titre : pays
           "<h4 style='margin: 0 0 8px 0;'>", Country, "</h4>",
           
           # Tableau avec deux parties : en haut le résumé, en bas les listes
           "<table style='border-collapse: collapse; width: 250px;'>",
           
           # Ligne entête
           "<thead>",
           "<tr style='background-color: #4a90e2; color: white;'>",
           "<th style='border: 1px solid #ddd; padding: 8px; text-align: center;'>Yes</th>",
           "<th style='border: 1px solid #ddd; padding: 8px; text-align: center;'>No</th>",
           "</tr>",
           "</thead>",
           
           # Ligne résumé avec les nombres
           "<tbody>",
           "<tr style='background-color: #f1f1f1;'>",
           "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", Yes, "</td>",
           "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", No, "</td>",
           "</tr>",
           
           # Ligne avec les listes
           "<tr>",
           "<td style='border: 1px solid #ddd; padding: 8px; vertical-align: top;'>", list_Yes, "</td>",
           "<td style='border: 1px solid #ddd; padding: 8px; vertical-align: top;'>", list_No, "</td>",
           "</tr>",
           
           "</tbody>",
           "</table>",
           "</div>"
         ),
         popupOptions = popupOptions(className = "custom-popup")
       ) %>%
       setView(
         lng = mean(df$Longitude, na.rm = TRUE),
         lat = mean(df$Latitude, na.rm = TRUE),
         zoom = 2
       ) %>%
       setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
   })
  
  # Reactive computation of MCA (Multiple Correspondence Analysis) results
  mca_results <- reactive({
    perform_mca(data_reactive())  
  })
  
 
  # output$mca_plot_ui <- renderUI({
  #   acm_object <- mca_results()$acm
  #   d <- mca_results()$d
  #   
  #   rownames(acm_object$li) <- rownames(d)
  #   res <- prepare_results(acm_object)
  #   
  #   div(
  #     style = "height: 800px;",  
  #     MCA_ind_plot(res,
  #                  width = "90%", height = "800px",
  #                  xax = 1, yax = 2,
  #                  ind_sup = FALSE,
  #                  lab_var = "Lab",
  #                  ind_lab_min_contrib = 0,
  #                  col_var = "Nonprofit",
  #                  labels_size = 9,
  #                  point_opacity = 0.5,
  #                  opacity_var = NULL,
  #                  point_size = 64,
  #                  ellipses = FALSE,
  #                  transitions = TRUE,
  #                  labels_positions = "auto",
  #                  xlim = c(-2.27, 2.39),
  #                  ylim = c(-1.46, 1.5))
  #   )
  # })
  
  output$mca_plot_ui <- renderUI({
    acm_object <- mca_results()$acm
    d <- mca_results()$d
    rownames(acm_object$li) <- rownames(d)
    res <- prepare_results(acm_object)
    
    div(
      style = "height: 800px;",  
      MCA_ind_plot(res,
                   width = "90%", height = "800px",
                   xax = 1, yax = 2,
                   ind_sup = FALSE,
                   lab_var = "Lab",
                   ind_lab_min_contrib = 0.05, # moins de labels
                   col_var = "Nonprofit",
                   labels_size = 7,            # plus petit
                   point_opacity = 0.5,
                   point_size = 32,            # plus petit
                   ellipses = FALSE,
                   transitions = FALSE,        # désactiver animations
                   labels_positions = "auto",
                   xlim = c(-2.27, 2.39),
                   ylim = c(-1.46, 1.5))
    )
  })
  
  
  
  # Render the MCA plot for Category dimension
  output$mca_plot_ui2 <- renderUI({
    acm_object <- mca_results()$acm
    d <- mca_results()$d
    
    rownames(acm_object$li) <- rownames(d)
    res <- explor::prepare_results(acm_object)
    
    div(
      style = "height: 800px;",  
      MCA_ind_plot(res,
                   width = "90%", height = "800px",
                   xax = 1, yax = 2,
                   ind_sup = FALSE,
                   lab_var = "Lab",
                   ind_lab_min_contrib = 0.05,
                   col_var = "Category",
                   labels_size = 7,
                   point_opacity = 0.5,
                   opacity_var = NULL,
                   point_size = 32,
                   ellipses = FALSE,
                   transitions = FALSE,
                   labels_positions = "auto",
                   xlim = c(-2.27, 2.39),
                   ylim = c(-1.46, 1.5))
    )
  })
  
  
  # Render the dendrogram for hierarchical clustering
  output$dendrogram <- renderPlotly({
    
    # Nombre de clusters
    k <- 10
    
    # Créer et colorier le dendrogramme
    dend <- as.dendrogram(arbre_phi2)
    dend_colored <- color_branches(dend, k = k)
    
    # Extraire les données du dendrogramme coloré
    dend_data <- dendro_data(dend_colored, type = "rectangle")
    segs <- dend_data$segments
    labels <- dend_data$labels
    
    # On récupère les couleurs via une astuce :)
    # color_branches ajoute un attribut "edgePar" avec une couleur sur chaque nœud
    # Il faut "traverser" le dendrogramme pour en extraire l'ordre
    extract_segment_colors <- function(dend) {
      segs <- list()
      get_segments <- function(node) {
        if (is.leaf(node)) return()
        attr <- attributes(node)
        if (!is.null(attr$edgePar$col)) {
          col <- attr$edgePar$col
        } else {
          col <- "gray"
        }
        x <- attr$height
        for (i in 1:2) {
          child <- node[[i]]
          y <- attributes(child)$height
          segs[[length(segs) + 1]] <<- list(
            x = x, xend = y,
            y = attr$members - sum(sapply(node[1:i], function(n) attributes(n)$members)),
            yend = attr$members - sum(sapply(node[1:(i-1)], function(n) attributes(n)$members)),
            col = col
          )
          get_segments(child)
        }
      }
      get_segments(dend)
      do.call(rbind, lapply(segs, as.data.frame))
    }
    
    # Appel de la fonction pour obtenir les couleurs
    segs_color_df <- extract_segment_colors(dend_colored)
    
    # Fusion avec les segments horizontaux
    segs$col <- segs_color_df$col
    
    # Passage en horizontal
    max_x <- max(segs$yend, na.rm = TRUE)
    segs_horiz <- data.frame(
      x = max_x - segs$y,
      xend = max_x - segs$yend,
      y = segs$x,
      yend = segs$xend,
      col = segs$col
    )
    
    # Ajouter les clusters aux labels
    clusters <- cutree(arbre_phi2, k = k)
    labels$cluster <- as.factor(clusters[match(labels$label, names(clusters))])
    labels$x_horiz <- max_x - labels$y
    labels$y_horiz <- labels$x
    label_offset <- max(segs_horiz$xend, na.rm = TRUE) + 0.2
    
    # Affichage plotly
    plot_ly(type = "scatter", mode = "lines") %>%
      add_segments(
        data = segs_horiz,
        x = ~x, xend = ~xend,
        y = ~y, yend = ~yend,
        line = list(color = ~col, width = 3),
        showlegend = FALSE
      ) %>%
      
      add_text(
        data = labels,
        x = label_offset,
        y = ~y_horiz,
        text = ~label,
        # textfont = list(size = 13),
        textfont = list(size = 15, family = "Arial Black"),
        textposition = "right",
        color = ~cluster,
        colors = "Set1",
        showlegend = FALSE
      ) %>%
    
      
      layout(
        title = "Interactive dendrogram",
        xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE,
                     range = c(0, label_offset + 9)),
        yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
        margin = list(l = 20, r = 35, t = 30, b = 20)
      )
    
  })
  
  # Observe new data, compute clusters and sync to Google Sheets
  observeEvent(data_reactive(), {
    data <- data_reactive()
    
    clustered_data <- data %>%
      mutate(cluster = cutree(arbre_phi2, k = 10))
    
    sheet_id <- "1WWY-AFsFY70xf7JgRAZFwjl7tcHcdCplb8QT5bb3-k8"
    write_sheet(clustered_data, ss = sheet_id, sheet = "Clusters")
    
    cat("✅ Clusters updated in Google Sheet.\n")
  })
  
  # --------------------------------------------------
  # Generate a pastel color palette for badges
  # --------------------------------------------------
  generate_color_palette <- function(n) {
    # Generate 'n' distinct hues evenly spaced around the color wheel
    hues <- seq(15, 375, length = n + 1)
    # Return HCL-based pastel colors
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  # --------------------------------------------------
  # Create colored HTML badges for categorical types
  # --------------------------------------------------
  create_colored_badge_column <- function(type_vector) {
    unique_types <- unique(type_vector)                      # Identify unique categories
    colors <- generate_color_palette(length(unique_types))   # Generate one color per category
    names(colors) <- unique_types                            # Assign names to the palette
    # Create an HTML <span> badge for each element in the type vector
    badges <- paste0(
      "<span class='badge' style='background-color:", 
      colors[type_vector], 
      ";'>", 
      type_vector, 
      "</span>"
    )
    return(badges)
  }
  
  # --------------------------------------------------
  # Monitoring Table: National/Global Monitoring Initiatives
  # --------------------------------------------------
  output$monitoring_table <- DT::renderDataTable({
    df <- monitoring_data()
    
    # Add stylized HTML link buttons to the 'Link' column
    df$Link <- ifelse(
      is.na(df$Link) | df$Link == "",
      "",
      paste0("<a class='btn-visit' target='_blank' href='", df$Link, "'>🔗 Visit</a>")
    )
    
    # Replace 'Type' column with colored badges
    df$Type <- create_colored_badge_column(df$Type)
    
    # Render datatable
    DT::datatable(
      df %>% select(Initiative, Type, Link, Description),
      escape = FALSE,                      # Allow rendering of HTML in cells
      rownames = FALSE,                    # Remove row indices
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,                  # Show 10 rows per page
        scrollX = TRUE,                   # Enable horizontal scrolling
        dom = 'Bfrtip',                   # Show buttons (copy, CSV, Excel)
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-left', targets = "_all"),
          list(width = '25%', targets = 0),  # Initiative
          list(width = '15%', targets = 1),  # Type
          list(width = '10%', targets = 2),  # Link
          list(width = '50%', targets = 3, className = 'wrap-text')  # Description
        )
      ),
      class = 'display nowrap compact stripe'
    )
  })
  
  # --------------------------------------------------
  # Focus Table: Initiatives by Country/Region
  # --------------------------------------------------
  output$focus_table <- DT::renderDataTable({
    df <- df_focus()
    
    # Add country flag icons in the 'Country/Region' column
    df$`Country/Region` <- ifelse(
      is.na(df$Flag) | df$Flag == "",
      df$`Country/Region`,
      paste0("<img src='", df$Flag, "' width='24' style='margin-right:6px;'>", df$`Country/Region`)
    )
    
    # Stylized HTML link buttons
    df$Link <- ifelse(
      is.na(df$Link) | df$Link == "",
      "",
      paste0("<a class='btn-visit' target='_blank' href='", df$Link, "'>🔗 Visit</a>")
    )
    
    # Colored badge for type
    df$Type <- create_colored_badge_column(df$Type)
    
    # Render datatable
    DT::datatable(
      df %>% select(Initiative, `Country/Region`, Group, Type, Description, Link),
      escape = FALSE,
      rownames = FALSE,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-left', targets = "_all"),
          list(width = '15%', targets = 0),  # Initiative
          list(width = '15%', targets = 1),  # Country/Region
          list(width = '10%', targets = 2),  # Group
          list(width = '15%', targets = 3),  # Type
          list(width = '35%', targets = 4, className = 'wrap-text'),  # Description
          list(width = '10%', targets = 5)   # Link
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

