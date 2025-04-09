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
    #filter(!(adm0_a3 == "NA") & !is.na(Country)) %>%
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
#    select(OrgName2, Nonprofit, audience, Focus, Category, CommunityGovernance) %>%
    
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
# Update Google Sheets with the Cluster Data
#######################
# Define the URL for the Google Sheet to update
sheet_url <- "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo"

# Write the data (including cluster information) back to the Google Sheet
write_sheet(data, ss = sheet_url, sheet = "Clusters")

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
# User Interface :)
#######################

ui <- dashboardPage(
  
  # Dashboard header with title and customized width
  dashboardHeader(
    title = "OS initiatives",  # Dashboard title
    titleWidth = 450           # Custom width for the title
  ),
  
  # Sidebar (navigation menu)
  dashboardSidebar(
    sidebarMenu(
      # Menu item for the global overview view
      menuItem("Global Overview", tabName = "overview", icon = icon("globe")),
      
      # Menu item for MCA & Clustering analysis
      menuItem("MCA & Clustering", tabName = "mca", icon = icon("chart-bar")),
      
      
      # Menu item for "About" Page
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      
      
      # Add a clickable logo that redirects to an external link
      tags$li(class = "dropdown", 
              tags$a(href = "https://www.gemass.fr/contract/openit/", 
                     target = "_blank", 
                     imageOutput('logo_image')))  # Display logo (to be defined in the server)
    )
  ),
  
  # Body of the dashboard (different pages and visualizations)
  dashboardBody(
    tabItems(
      
      # Global overview (overview)
      tabItem(tabName = "overview",
              fluidRow(
                # Box to display the total number of initiatives
                box(
                  title = "Global Open Science Initiatives",  
                  status = "primary",  
                  solidHeader = TRUE,  
                  width = 4,  # Box width
                  height = "521px",  
                  style = "border-radius: 30px; box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);",  # Custom style
                  tags$div(
                    style = "text-align: center; padding-top: 50px;",  # Styling for internal div
                    tags$i(class = "fa fa-lightbulb-o", style = "font-size: 100px; color: #007bff; margin-bottom: 10px;"),  # Icon
                    tags$div(
                      style = "font-size: 100px; font-weight: bold; color: #007bff;", 
                      textOutput("distinct_initiatives")  # Dynamically display the number of distinct initiatives
                    ),
                    tags$div(
                      style = "font-size: 25px; font-weight: bold; color: #333;",
                      "Total Number of Initiatives"  # Label below the number of initiatives
                    ),
                    tags$p(
                      style = "font-size: 15px; margin-top: 10px; color: #555;"
                    ),
                    # Clickable link to access and enrich the data
                    tags$a(
                      href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=1136935931#gid=1136935931",
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
                
                # Box to display initiatives by category
                box(
                  title = "Initiatives by Category", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 8,
                  plotlyOutput("category_plot"),  # Display Plotly interactive chart
                  tags$p(
                    "This bar chart provides an overview of the number of initiatives grouped by their focus area. The categories include Journals and Publication Models, Open Science and Policies, Tools and Research Services, Community and Education, Preprints and Repositories, and Data and Infrastructure. Hover over each bar to view detailed counts, or click on a bar to explore the specific initiatives within the selected category."
                  )
                )
              ),
              
              # Row to display the list of selected initiatives
              fluidRow(
                box(
                  title = "Selected Initiatives List",  
                  status = "info",  
                  solidHeader = TRUE,  
                  width = 12,  
                  uiOutput("clicked_initiatives"),  # Dynamically display the list of selected initiatives
                  tags$p(
                    "Click on a bar above to view the initiatives belonging to the selected category.",
                    style = "color: #555;"
                  )
                )
              ),
              
              # Row to display the global distribution map of initiatives and the community governance map
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Global Distribution of Initiatives",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    leafletOutput("world_map"),  # Interactive map with Leaflet
                    tags$style(HTML(popup_css)),  # CSS style for map popup
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
                    leafletOutput("comm_gov"),  # Another map to show governance data
                    tags$p(
                      "This map displays a comparison of community governance practices in open science initiatives. The pie charts represent the proportion of initiatives with and without community-driven governance structures."
                    )
                  )
                )
              )
      ),
      
      # MCA & Clustering analysis page
      tabItem(tabName = "mca",
              fluidRow(
                box(
                      title = "Multiple Correspondence Analysis (MCA): NonProfit",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      uiOutput("mca_plot_ui"),  # Utilisation d'uiOutput pour afficher le plot explor
                      
                      tags$p(
                        "This plot shows the results of a Multiple Correspondence Analysis (MCA), focusing on the first two dimensions. It visualizes the relationships between different characteristics of open science initiatives. Each point represents an initiative, and the colors indicate whether the initiative is a nonprofit or not, highlighting potential differences in their positioning along these two axes."
                      )
                    ),
                
                box(
                  title = "Multiple Correspondence Analysis (MCA): Category",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("mca_plot_ui2"),  # Utilisation d'uiOutput pour afficher le plot explor
                  
                  tags$p(
                    "This plot presents the results of a Multiple Correspondence Analysis (MCA), highlighting the first two dimensions. Each point represents an open science initiative, and the colors differentiate the initiatives based on their category (e.g., open-source, community-driven). This visualization helps to explore how the category influences the distribution of initiatives in the multidimensional space."
                  )
                ),
                
                # Column to display the hierarchical clustering dendrogram
                box(
                  title = "Hierarchical Clustering Dendrogram",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("dendrogram"),  # Dendrogram plot output
                  tags$p(
                    "This dendrogram shows the hierarchical clustering of initiatives based on their characteristics. The clusters provide insight into how similar initiatives group together based on shared attributes."
                  )
                )
              ),
              fluidRow(
                # Box with a link to explore the clusters further
                box(
                  title = "View Clusters",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  style = "cursor: pointer; text-align: center; font-size: 18px; 
                           border-radius: 10px; box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);",
                  tags$a(
                    href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=998126494#gid=998126494",
                    target = "_blank",
                    "📊 Access Clusters on Google Sheets",
                    style = "color: #ffffff; text-decoration: none; display: block; padding: 15px; background-color: #007bff; border-radius: 10px;"
                  ),
                  tags$p(
                    "Click the link above to explore the clustering results in more detail, including which initiatives belong to each cluster."
                  )
                )
              )
              
      ),
      # "About" tab
      # Ajout de l'onglet About
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About this Project", status = "primary", solidHeader = TRUE,
                  width = 12,
                  div(style = "white-space: pre-wrap;",
                      paste(readLines("About.txt"), collapse = "\n"))
                )
                
              )
      )
    )
    )
  )

# # Ajout de l'icône dans la section head
# ui <- tagList(
#   tags$head(
#     tags$title("OS Initiatives Dashboard")
#   ),
#   tags$a("🌐 OS Initiatives", href = "https://www.gemass.fr/contract/openit/", 
#          style = "font-size: 20px; text-decoration: none; margin-left: 10px;"),
#   ui
# )

# # Ajout de l'icône dans la section head
ui <- tagList(
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "https://upload.wikimedia.org/wikipedia/commons/f/f0/Cadenas-ouvert-vert.svg"),
    tags$title("OS Initiatives Dashboard")
  ),
  ui
)



#######################
# Server part
#######################


server <- function(input, output, session) {

  # Rendering the logo image in the UI
  output$logo_image <- renderImage({
    list(src = "www/logo.png",  # Path to the logo image
         alt = "Logo OS Initiatives",  # Alt text for the image
         height = "70px",  # Set the height of the logo
         width = "auto")  # Set width to auto to maintain aspect ratio
  }, deleteFile = FALSE)  # Ensure the image file is not deleted after rendering
  
  # Reactive poll to update the data every 10 minutes (600000 ms)
  data_reactive <- reactivePoll(600000, session,
                                checkFunc = function() { Sys.time() },  # Function to check if the time has changed
                                valueFunc = function() { download_data() }  # Function to download new data when time changes
  )
  
  # Output: Calculate the number of distinct initiatives and display it
  output$distinct_initiatives <- renderText({
    data <- data_reactive()  # Get the most recent data
    n_distinct(data$OrgName2)  # Count the number of distinct initiatives based on the 'OrgName2' column
  })
  
  
  # # Creating an interactive Plotly bar chart to show the number of initiatives by category
  # output$category_plot <- renderPlotly({
  #   data <- data_reactive()  # Get the most recent data
  #   
  #   # Count the number of initiatives per category for the Y-axis
  #   category_count <- data %>%
  #     group_by(Category) %>%
  #     summarise(count = n(), .groups = "drop")  # Group by Category and count the number of occurrences
  #   # Tri des catégories par ordre décroissant
  #   category_count <- category_count %>%
  #     arrange(desc(count)) %>%
  #     mutate(
  #       Category = factor(Category, levels = unique(Category)),  # pour le tri des couleurs
  #       Category_label = str_wrap(as.character(Category), width = 15),
  #       Category_label = factor(Category_label, levels = unique(Category_label))  # pour l'ordre sur l'axe x
  #     )
  #   
  #   # Create a bar plot using ggplot
  #   p <- ggplot(category_count, aes(x = Category_label, y = count, fill = Category)) +
  #     geom_bar(stat = "identity", show.legend = FALSE) +
  #     geom_text(aes(label = count), vjust = 3, size = 4.5, fontface = "bold", color = "black") +
  #     theme_minimal(base_size = 14) +
  #     theme(
  #       axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
  #       plot.title = element_text(face = "bold", hjust = 0.5),
  #       axis.title.x = element_text(margin = margin(t = 10)),
  #       axis.title.y = element_text(margin = margin(r = 10))
  #     ) +
  #     scale_fill_brewer(palette = "Set3") +
  #     labs(x = "Category", y = "Number of Initiatives", title = "Number of Initiatives by Category")  # Labels for axes and title
  #   
  #   ggplotly(p, tooltip = "text", source = "select_bar")  # Make the ggplot chart interactive with Plotly
  # })
  # 
  # #####  
  # # Reactive value to store the category selected by the user from the chart
  # selected_category <- reactiveVal(NULL)  # Initially set the selected category to NULL
  # 
  # # Observe the click event on the Plotly chart to update the selected category
  # observeEvent(event_data("plotly_click", source = "select_bar"), {
  #   click_data <- event_data("plotly_click", source = "select_bar")  # Capture click event data
  #   
  #   if (!is.null(click_data)) {  # Check if the click data is valid
  #     selected_category(click_data$text)  # Update the selected category based on the clicked category
  #     cat("Selected category:", selected_category(), "\n")
  #   }
  # })
  # 
  # # Reactive expression to get unique category labels from the data
  # category_labels <- reactive({
  #   data <- data_reactive()  # Get the most recent data
  #   unique(data$Category)  # Return the unique category labels from the 'Category' column
  # })
  # 
  # # Handle the category selection logic when a category is clicked
  # observeEvent(event_data("plotly_click", source = "select_bar"), {
  #   click_data <- event_data("plotly_click", source = "select_bar")  # Capture click event data
  #   
  #   if (!is.null(click_data)) {  # If click data exists
  #     category_index <- as.numeric(click_data$x)  # Convert the clicked category index to numeric
  #     labels <- category_labels()  # Get the unique category labels from the data
  #     
  #     # Check if the clicked index is valid and update the selected category accordingly
  #     if (category_index > 0 && category_index <= length(labels)) {
  #       selected_category(labels[category_index])  # Set the selected category to the clicked one
  #     } else {
  #       selected_category(NULL)  # Set to NULL if the clicked index is out of bounds
  #     }
  #     
  #     cat("Selected category:", selected_category(), "\n")  # Print the selected category for debugging
  #   }
  # })
  # 
  # # Output: Render the UI elements based on the selected category (display initiatives table or message)
  # output$clicked_initiatives <- renderUI({
  #   data <- data_reactive()  # Get the most recent data
  #   
  #   if (is.null(selected_category())) {
  #     return(tags$p("Click on a bar in the chart to view the corresponding initiatives.", style = "color: #999;"))  # Message when no category is selected
  #   }
  #   
  #   # Filter the data based on the selected category
  #   filtered_data <- data %>% filter(Category == selected_category())
  #   
  #   cat("Selected category after update:", selected_category(), "\n")
  #   cat("Number of rows after filtering:", nrow(filtered_data), "\n")
  #   
  #   DT::dataTableOutput("initiatives_table")  # Output the filtered data table
  # })
  # 
  # # Render the data table with the filtered initiatives data based on the selected category
  # output$initiatives_table <- DT::renderDataTable({
  #   filtered_data <- data_reactive() %>% filter(Category == selected_category())  # Filter the data based on selected category
  #   
  #   DT::datatable(
  #     filtered_data %>% select(OrgName, Category, CommunityGovernance, Nonprofit, OpenSource),  # Select the columns to display
  #     options = list(pageLength = 5, autoWidth = TRUE),  # Set the table options (page length and auto width)
  #     rownames = FALSE  # Disable row names in the table
  #   )
  # })

  
  # Data reactive
  data_reactive <- reactive({
    data
  })
  
  # Reactive value for selection
  selected_category <- reactiveVal(NULL)
  
  # Reactive table of categories with counts and wrapped labels (used for plotting and indexing)
  plotted_categories <- reactive({
    data <- data_reactive()
    data %>%
      count(Category, name = "count") %>%
      arrange(desc(count)) %>%
      mutate(
        Category_label = str_wrap(as.character(Category), width = 15),
        Category_label = factor(Category_label, levels = unique(Category_label))
      )
  })
  
  # Plot
  output$category_plot <- renderPlotly({
    cat_data <- plotted_categories()
    
    p <- ggplot(cat_data, aes(x = Category_label, y = count, fill = Category)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = count), vjust = 3, size = 4.5, fontface = "bold", color = "black") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
      ) +
      scale_fill_brewer(palette = "Set3") +
      labs(x = "Category", y = "Number of Initiatives", title = "Number of Initiatives by Category")
    
    ggplotly(p, tooltip = "none", source = "select_bar")
  })
  
  # Selection from click
  observeEvent(event_data("plotly_click", source = "select_bar"), {
    click_data <- event_data("plotly_click", source = "select_bar")
    
    if (!is.null(click_data)) {
      index <- round(click_data$x) #  # plotly index starts at 0
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
  
  # UI output
  output$clicked_initiatives <- renderUI({
    data <- data_reactive()
    
    if (is.null(selected_category())) {
      return(tags$p("Click on a bar in the chart to view the corresponding initiatives.", style = "color: #999;"))
    }
    
    filtered_data <- data %>% filter(Category == selected_category())
    
    cat("Selected category after update:", selected_category(), "\n")
    cat("Number of rows after filtering:", nrow(filtered_data), "\n")
    
    DT::dataTableOutput("initiatives_table")
  })
  
  # Table output
  output$initiatives_table <- DT::renderDataTable({
    filtered_data <- data_reactive() %>% filter(Category == selected_category())
    
    DT::datatable(
      filtered_data %>% select(OrgName, Category, CommunityGovernance, Nonprofit, OpenSource),
      options = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
    
  #####
  # Render the world map with Leaflet, showing initiatives by country
  output$world_map <- renderLeaflet({
    data <- data_reactive()  # Get the most recent data
    
    # Create a Leaflet map to visualize initiatives by country
    leaflet(initiatives_par_pays) %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,  # Use longitude and latitude for marker positions
        radius = ~sqrt(nb) * 2,  # Size of the markers proportional to the square root of the count
        color = ~colorNumeric("plasma", nb)(nb),  # Color based on the number of initiatives
        fillOpacity = 0.8,  # Set the fill opacity of the circles
        popup = ~paste(
          "<b>Country :</b>", Country, "<br>",  # Popup displaying country and count information
          "<b>Count :</b>", nb, "<br>",
          "<b>Initiatives list :</b><br>", liste_initiatives
        ),
        popupOptions = popupOptions(className = "custom-popup")  # Custom popup options
      ) %>%
      addLegend(
        "bottomright",  # Position of the legend
        pal = colorNumeric("plasma", initiatives_par_pays$nb),  # Color palette for legend
        values = initiatives_par_pays$nb,  # Legend values
        title = "Initiatives count"  # Title of the legend
      ) %>%
      setView(lng = mean(initiatives_par_pays$Longitude, na.rm = TRUE),  # Set initial map view
              lat = mean(initiatives_par_pays$Latitude, na.rm = TRUE),
              zoom = 2)  # Zoom level
  })
  
  # Render the map with pie charts representing community governance in each country
  output$comm_gov <- renderLeaflet({
    data <- data_reactive()  # Get the most recent data
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
      addMinicharts(
        lng = data_pie$Longitude, lat = data_pie$Latitude,  # Position of pie charts
        type = "pie",  # Pie chart type
        chartdata = data_pie[, c("Yes", "No")],  # Data for the Yes/No values in the pie chart
        colorPalette = c("blue", "red"),  # Blue for Yes and Red for No
        width = 30, height = 30,  # Size of the pie charts
        opacity = 0.8  # Opacity of the pie charts
      ) %>%
      addLegend(
        "bottomright",  # Position of the legend
        colors = c("blue", "red"),  # Colors for Yes/No
        labels = c("Yes", "No"),  # Labels for the legend
        title = "Community Governance"  # Title of the legend
      ) %>%
      setView(lng = mean(initiatives_par_pays$Longitude, na.rm = TRUE),
              lat = mean(initiatives_par_pays$Latitude, na.rm = TRUE),
              zoom = 2)  # Set the initial map view and zoom level
  })    
  
  # # Render the MCA plot (Multiple Correspondence Analysis) using ggplotly
  # output$mca_plot <- renderPlotly({
  #   data <- data_reactive()  # Get the most recent data
  #   
  #   # Create a MCA plot using fviz_mca_ind
  #   p1 <- fviz_mca_ind(acm, geom = "point", alpha.ind = .25, habillage = mca_results$d$CommunityGovernance, addEllipses = TRUE, repel = TRUE) +
  #     theme_minimal()  # Use a minimal theme for the plot
  #   ggplotly(p1)  # Convert the ggplot to a Plotly interactive plot
  # })
  

  # Perform MCA once (using your existing data processing)
  mca_results <- reactive({
    perform_mca(data_reactive())  # Utilise ta fonction existante pour calculer l'ACM
  })
  
  # Préparation et affichage du graphique avec les bonnes étiquettes
  output$mca_plot_ui <- renderUI({
    acm_object <- mca_results()$acm   # Récupérer l'objet ACM
    d <- mca_results()$d  # Récupérer les données originales utilisées pour l'ACM
    
    # Ajouter les noms de lignes ou une colonne pour étiqueter les points
    rownames(acm_object$li) <- rownames(d)  # Associer les étiquettes d'origine aux points
    
    # Convertir l'objet ACM pour explor avec les étiquettes
    res <- prepare_results(acm_object)
    
    # Affichage interactif avec les étiquettes correctes
    MCA_ind_plot(res, 
                         xax = 1, yax = 2, ind_sup = FALSE, lab_var = "Lab",
                         ind_lab_min_contrib = 0, col_var = "Nonprofit", 
                         labels_size = 7, point_opacity = 0.5, 
                         opacity_var = NULL, point_size = 64, ellipses = FALSE,
                         transitions = TRUE, labels_positions = "auto", 
                         xlim = c(-2.27, 2.39), ylim = c(-1.46, 1.5))
  })
  
  
  # Préparation et affichage du graphique avec les bonnes étiquettes
  output$mca_plot_ui2 <- renderUI({
    acm_object <- mca_results()$acm   # Récupérer l'objet ACM
    d <- mca_results()$d  # Récupérer les données originales utilisées pour l'ACM
    
    # Ajouter les noms de lignes ou une colonne pour étiqueter les points
    rownames(acm_object$li) <- rownames(d)  # Associer les étiquettes d'origine aux points
    
    # Convertir l'objet ACM pour explor avec les étiquettes
    res <- explor::prepare_results(acm_object)
    
    # Affichage interactif avec les étiquettes correctes
    MCA_ind_plot(res, 
                         xax = 1, yax = 2, ind_sup = FALSE, lab_var = "Lab",
                         ind_lab_min_contrib = 0, col_var = "Category", 
                         labels_size = 7, point_opacity = 0.5, 
                         opacity_var = NULL, point_size = 64, ellipses = FALSE,
                         transitions = TRUE, labels_positions = "auto", 
                         xlim = c(-2.27, 2.39), ylim = c(-1.46, 1.5))
  })
  
  # Render the dendrogram (cluster analysis) plot
  output$dendrogram <- renderPlotly({
    data <- data_reactive()  # Get the most recent data
    
    # Create a dendrogram using factoextra::fviz_dend
    p1 <- factoextra::fviz_dend(
      arbre_phi2,  # Input tree structure for clustering
      show_labels = TRUE,  # Show labels for each cluster
      k = 4,  # Cut the dendrogram into 4 clusters
      rect = TRUE  # Display rectangles around clusters
    ) +
      ggplot2::ggtitle("Dendrogram (cut into 4 clusters)") +  # Title of the plot
      theme_minimal() +  # Use a minimal theme
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate the x-axis labels for better readability
    
    ggplotly(p1)  # Convert the ggplot to a Plotly interactive plot
  })  
}

# To deploy the app locally
shinyApp(ui = ui, server = server)

# To deploy the app online
# rsconnect::deployApp(appDir = "C:/Users/amaddi/Documents/Projets financés/OPENIT/openit/osinit_app/osinit", appName = "openit")

