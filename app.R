#######################
rm(list = ls())
#######################
library(shiny)
library(shinydashboard)
library(tidyverse)
library(googlesheets4)
library(ade4)
library(factoextra)
library(plotly)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet.minicharts)
library(sf)
library(cartogram)
library(ggforce)
library(leaflet)
library(htmltools)


# 📌 Configuration des identifiants (via variables d'environnement)
library(rsconnect)

name <- Sys.getenv("SHINY_APP_NAME")
token <- Sys.getenv("SHINY_APP_TOKEN")
secret <- Sys.getenv("SHINY_APP_SECRET")

rsconnect::setAccountInfo(name = name, token = token, secret = secret)


gs4_auth(path = file.path(getwd(), "cle.json"))


# Charger les données
download_data <- function() {
  sheet_url <- "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?usp=sharing"
  read_sheet(sheet_url) %>%
    filter(!(adm0_a3 == "NA") & !is.na(Country)) %>%
    as.data.frame()
}

data <- download_data()
row.names(data) <- data$OrgName2


# Regrouper les initiatives par pays et concaténer les noms des organisations
initiatives_par_pays <- data %>%
  group_by(Country, adm0_a3, Latitude, Longitude) %>%
  summarise(
    nb = n(),
    liste_initiatives = paste(OrgName2, collapse = "<br>"),
    .groups = "drop"
  )


# Créer la palette de couleurs
pal <- colorNumeric("plasma", domain = initiatives_par_pays$nb)


### Prepa CommunityGovernance

# 1️⃣ Regrouper les initiatives par pays et type de gouvernance
data_pie <- data %>%
  count(adm0_a3, CommunityGovernance, name = "nb_cg") %>%  # Compter initiatives par pays/gouvernance
  group_by(adm0_a3, CommunityGovernance) %>%  
  summarise(nb_cg = sum(nb_cg), .groups = "drop")  # Agréger pour éviter les doublons

# 2️⃣ Coordonnées pays

data_coord <- data %>%
  select(adm0_a3, Country, Longitude, Latitude) %>%
  unique()


# 3️⃣ Joindre avec la carte et préparer les données
data_pie <- data_pie %>%
  right_join(data_coord, by = "adm0_a3") %>%  # Joindre proprement
  replace_na(list(nb_cg = 0, CommunityGovernance = "No")) %>%  # Gérer les valeurs manquantes
  filter(!is.na(Longitude)) %>%  # Supprimer les points sans coordonnées
  select(Country, Longitude, Latitude, CommunityGovernance, nb_cg) %>% 
  pivot_wider(names_from = CommunityGovernance, values_from = nb_cg, values_fill = list(nb_cg = 0)) %>%  # Corrige values_fill
  st_drop_geometry()  # Retirer la colonne "geometry"

# 4️⃣ Vérification des colonnes
data_pie <- data_pie %>% select(-any_of("0"))  # Supprimer une éventuelle colonne inutile



# MCA
perform_mca <- function(data) {
  d <- data %>%
    select(Nonprofit, OpenSource, Category, CommunityGovernance) %>%
    mutate(across(everything(), as.factor))
  
  acm <- dudi.acm(d, scannf = FALSE, nf = Inf)
  list(acm = acm, d = d)
}

# Function to perform clustering on the MCA results
perform_clustering <- function(cah) {
  # We can perform clustering on the full MCA result
  # (Using the same dudi.acm object for simplicity)
  md_phi2 <- dist.dudi(acm)
  arbre_phi2 <- hclust(md_phi2, method = "ward.D2")
}


mca_results <- perform_mca(data)
acm <- mca_results$acm
arbre_phi2 <- perform_clustering(cah)

########################################
########################################
data$cluster <- cutree(arbre_phi2, 5)

# Mettre à jour Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo"
write_sheet(data, ss = sheet_url, sheet = "Clusters")
########################################
########################################

# Définir le style CSS pour les popups
popup_css <- "
.custom-popup .leaflet-popup-content {
  max-height: 150px;
  overflow-y: auto;
}
"

ui <- dashboardPage(

#  dashboardHeader(title = "OS itiatives", titleWidth = 450),
  
  dashboardHeader(
    title = "OS initiatives",
    titleWidth = 450),
    
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Overview", tabName = "overview", icon = icon("globe")),
      menuItem("MCA & Clustering", tabName = "mca", icon = icon("chart-bar")),
      # Ajouter un logo cliquable
      tags$li(class = "dropdown", 
              tags$a(href = "https://www.gemass.fr/contract/openit/", 
                     target = "_blank", 
                     imageOutput('logo_image')))
      )),
  
  dashboardBody(
     tabItems(
      tabItem(tabName = "overview",
              fluidRow(
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
                    tags$p(
                      style = "font-size: 15px; margin-top: 10px; color: #555;"#,
                      #"This figure represents the total number of distinct open science initiatives tracked in the dataset."
                    ),
                    tags$a(
                      href = "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?gid=998126494",
                      target = "_blank",
                      "🔗 Access and Enrich Data",
                      style = "color: #ffffff; text-decoration: none; display: inline-block; margin-top: 15px; padding: 10px; 
                               background-color: #007bff; border-radius: 10px; font-size: 16px;"
                    )
                  )
                ),

                box(
                  title = "Initiatives by Category", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 8,
                  plotlyOutput("category_plot"),
                  tags$p(
                    "This bar chart provides an overview of the number of initiatives grouped by their focus area. The categories include Journals and Publication Models, Open Science and Policies, Tools and Research Services, Community and Education, Preprints and Repositories, and Data and Infrastructure. Hover over each bar to view detailed counts, or click on a bar to explore the specific initiatives within the selected category."
                  )
                )
              ),
              
              fluidRow(
                # Box supplémentaire pour afficher les initiatives cliquées
                box(
                  title = "Selected Initiatives List",  
                  status = "info",  
                  solidHeader = TRUE,  
                  width = 12,  
                  uiOutput("clicked_initiatives"),  # Pour afficher dynamiquement la liste
                  tags$p(
                    "Click on a bar above to view the initiatives belonging to the selected category.",
                    style = "color: #555;"
                  )
                )
              ),
              
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
      
      tabItem(tabName = "mca",
              fluidRow(
                column(
                  title = "Multiple Correspondence Analysis (MCA)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("mca_plot"),
                  tags$p(
                    "This plot shows the results of a Multiple Correspondence Analysis (MCA), which visualizes the relationships between different characteristics of open science initiatives. Each point represents an initiative, and the colors differentiate initiatives based on community governance."
                  )
                ),
              #),
              #fluidRow(
              column(
                  title = "Hierarchical Clustering Dendrogram",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("dendrogram"),
                  tags$p(
                    "This dendrogram shows the hierarchical clustering of initiatives based on their characteristics. The clusters provide insight into how similar initiatives group together based on shared attributes."
                  )
                )
              ),
              fluidRow(
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
      )
    )
  )
)


server <- function(input, output, session) {

  output$logo_image <- renderImage({
    list(src = "www/logo.png",
         alt = "Logo OS Initiatives",
         height = "70px",  # Ajuster la taille du logo
         width = "auto")
  }, deleteFile = FALSE)
  
  data_reactive <- reactivePoll(600000, session,
                                checkFunc = function() { Sys.time() },  # Vérifier si le temps a changé
                                valueFunc = function() { download_data() }  # Charger les nouvelles données
                                )
  
  # Calcul du nombre distinct d'initiatives
  output$distinct_initiatives <- renderText({
    data <- data_reactive()  # Utiliser les données actualisées
    n_distinct(data$OrgName2)  # Nombre d'initiatives
  })
  

  # Création du graphique interactif Plotly avec gestion des clics
  output$category_plot <- renderPlotly({
    data <- data_reactive()
    
    # Compter le nombre d'initiatives par catégorie pour l'axe Y
    category_count <- data %>%
      group_by(Category) %>%
      summarise(count = n(), .groups = "drop")
    
    # Créer le graphique avec ggplot
    p <- ggplot(category_count, aes(x = Category, y = count, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = count), vjust = -0.5, size = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3") +
      labs(x = "Category", y = "Number of Initiatives", title = "Number of Initiatives by Category")
    
    ggplotly(p, source = "select_bar")  # Rendre le graphique interactif
  })

#####  
  # Gestion dynamique de la liste des initiatives lorsqu'une catégorie est cliquée
  selected_category <- reactiveVal(NULL)  # Stocker la catégorie cliquée

  observeEvent(event_data("plotly_click", source = "select_bar"), {
    # Capture l'événement de clic
    click_data <- event_data("plotly_click", source = "select_bar")

    if (!is.null(click_data)) {
      # Récupérer la catégorie cliquée
      selected_category(click_data$x)
    }
  })


    category_labels <- reactive({
    data <- data_reactive()
    unique(data$Category)
  })
  
  observeEvent(event_data("plotly_click", source = "select_bar"), {
    click_data <- event_data("plotly_click", source = "select_bar")
    
    if (!is.null(click_data)) {
      category_index <- as.numeric(click_data$x)
      labels <- category_labels()
      
      if (category_index > 0 && category_index <= length(labels)) {
        selected_category(labels[category_index])
      } else {
        selected_category(NULL)
      }
      
      cat("Catégorie sélectionnée :", selected_category(), "\n")
    }
  })
  
  output$clicked_initiatives <- renderUI({
    data <- data_reactive()
    
    if (is.null(selected_category())) {
      return(tags$p("Click on a bar in the chart to view the corresponding initiatives.", style = "color: #999;"))
    }
    
    filtered_data <- data %>% filter(Category == selected_category())
    
    cat("Catégorie sélectionnée après mise à jour :", selected_category(), "\n")
    cat("Nombre de lignes après filtrage :", nrow(filtered_data), "\n")
    
    DT::dataTableOutput("initiatives_table")
  })
  
  output$initiatives_table <- DT::renderDataTable({
    filtered_data <- data_reactive() %>% filter(Category == selected_category())
    
    DT::datatable(
      filtered_data %>% select(OrgName, Category, CommunityGovernance, Nonprofit, OpenSource),
      options = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  
  
  #####
  output$world_map <- renderLeaflet({
    data <- data_reactive()  # Utiliser les données actualisées

    leaflet(initiatives_par_pays) %>%
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
        title = "Initiatives count"
      ) %>%
      setView(lng = mean(initiatives_par_pays$Longitude, na.rm = TRUE),
              lat = mean(initiatives_par_pays$Latitude, na.rm = TRUE),
              zoom = 2)
  })

   
  output$comm_gov <- renderLeaflet({
    data <- data_reactive()  # Utiliser les données actualisées
    # 5️⃣ Création de la carte avec pie charts
      leaflet() %>%
      addTiles() %>%
      addMinicharts(
        lng = data_pie$Longitude,
        lat = data_pie$Latitude,
        type = "pie",
        chartdata = data_pie[, c("Yes", "No")],  # Colonnes des valeurs
        colorPalette = c("blue", "red"),  # Couleurs Yes/No
        width = 30, height = 30,  # Taille des camemberts
        opacity = 0.8
      ) %>%
      addLegend(
        "bottomright",
        colors = c("blue", "red"),
        labels = c("Yes", "No"),
        title = "Community Governance"
      ) %>%
        setView(lng = mean(initiatives_par_pays$Longitude, na.rm = TRUE),
                lat = mean(initiatives_par_pays$Latitude, na.rm = TRUE),
                zoom = 2)
    
  })    
  
  output$mca_plot <- renderPlotly({
    data <- data_reactive()  # Utiliser les données actualisées
    p1 <- fviz_mca_ind(acm, geom = "point", alpha.ind = .25, habillage = mca_results$d$CommunityGovernance, addEllipses = TRUE) +
      theme_minimal()
    ggplotly(p1)
  })
  
  output$dendrogram <- renderPlotly({
    data <- data_reactive()  # Utiliser les données actualisées
    p1 <- factoextra::fviz_dend(
      arbre_phi2,
      show_labels = TRUE,
      k = 4,
      rect = TRUE
    )  +
      ggplot2::ggtitle("Dendrogram (cut into 4 clusters)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1)  # Rotation des labels de l'axe x
      )
    ggplotly(p1)
  })  
}

shinyApp(ui = ui, server = server)

# rsconnect::deployApp(appDir = "C:/Users/amaddi/Documents/Projets financés/OPENIT/openit/osinit", appName = "openit")

