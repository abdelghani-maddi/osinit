library(tidyverse)
library(questionr)
library(gtsummary)
library(readxl)
library(ade4)

# 📊 Lire les données
sheet_url <- "https://docs.google.com/spreadsheets/d/1F2T_VfKAxvvGdna-nMOrIErM6bZnMXiirzMnsx-7YZo/edit?usp=sharing" 
data <- read_sheet(sheet_url)

data <- data %>%
  filter(!is.na(Country)) %>%
  as.data.frame()

row.names(data) <- data$OrgName2


# Selection des variables pour l'ACM
d <- data %>%
  select(Nonprofit, OpenSource, Category, CommunityGovernance) %>% #, audience) %>%
  mutate(across(everything(), as.factor))


# Réaliser l'ACM
acm <- dudi.acm(d, scannf = F, nf = Inf)


# Graphique interactif

screeplot(acm) # autre façon de représenter graph des axes
res <- explor::prepare_results(acm)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = "Lab",
                     ind_lab_min_contrib = 0, col_var = "CommunityGovernance", labels_size = 7,
                     point_opacity = 0.5, opacity_var = "Cos2", point_size = 103, ellipses = TRUE,
                     transitions = TRUE, labels_positions = "auto", xlim = c(-2.33, 1.94), ylim = c(-2.02,
                                                                                                    2.24))
# Une autre façon intéressante
s.hist(acm$li, clabel =0 , pch = 15, cbr = 3, adj = 0.5)

# représentation qui lie les observations aux barycentes :
# s.class(acm$li, d$Nonprofit, col = c("red", "darkgreen"))
# s.class(acm$li, d$CommunityGovernance, col = c("red", "darkgreen"))


# Variables supplémentaires / illustratives
fviz_mca_ind(acm, geom = "point", habillage = d$Category) # cela marche aussi avec "habillahe" à condition de ne pas changer l'ordre des individus !

# plettes de couleurs : https://www.datanovia.com/en/fr/blog/palette-de-couleurs-rcolorbrewer-de-a-a-z/ 
scatter(acm, col = RColorBrewer::brewer.pal(5, "Set1")) # sortir 5 couleurs de la palette "set1"


# visualisation avec les variables sociodémographiques
fviz_mca_ind(acm, geom = "point", habillage = d$Nonprofit, addEllipses = TRUE)
fviz_mca_ind(acm, geom = "point", habillage = d$OpenSource, addEllipses = TRUE)


# Clusterisation
acm_ad <- d |>
  ade4::dudi.acm(scannf = FALSE)
md_phi2 <- acm_ad |> 
  ade4::dist.dudi()


arbre_phi2 <- md_phi2 |> 
  hclust(method = "ward.D2")


arbre_phi2 |> 
  factoextra::fviz_dend(
    show_labels = T,
    k = 4,
    rect = TRUE
  ) +
  ggplot2::ggtitle("Dendrogramme découpé en 4 classes (distance de Gowver)")




# Créer un résumé des données par continent
continent_counts <- data %>%
  count(Continent) %>%
  filter(Continent != "NA")  # On supprime les valeurs "NA"

# Carte simple de répartition des continents
ggplot(data = continent_counts, aes(x = Continent, y = n, fill = Continent)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Répartition des publications par continent",
       x = "Continent", y = "Nombre de publications") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
