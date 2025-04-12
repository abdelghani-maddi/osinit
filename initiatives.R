
library(tidyverse)
library(questionr)
library(gtsummary)
library(readxl)
library(ade4)

data <- read_excel("D:/OPEN IT/OpenScholComm_Landscaping.xlsx") %>%
  as.data.frame()

row.names(data) <- data$OrgName2

# Normaliser le nom dans la colonne OrgName
data$OrgName <- gsub("\\\\", "\\", data$OrgName)

# Définir les catégories
categories <- list(
  "Preprints and Repositories" = c("arXiv", "bioRxiv", "Peer Community In", "HAL Open Science", 
                                   "Europe PMC", "Zenodo", "Registry of Open Access Repositories", 
                                   "La Referencia", "ResearchEquals", "veriXiv", "Preprint Club", 
                                   "PREreview", "Érudit", "Episciences", "EGUsphere", "Janeway", 
                                   "Open Journal Systems", "Recherche Data Gouv", "DOAB", "DOAJ"),
  
  "Journals and Publication Models" = c("Access Microbiology", "PLOS", "PeerJ", "GigaScience Journal", 
                                        "Copernicus Publications", "eLife", "SciELO", "F1000", 
                                        "Wellcome Open Research", "Springer Nature Research Square", 
                                        "The Unjournal", "Rapid ReviewsInfectious Diseases (RRID)", 
                                        "GIGAbyte Journal", "Review Commons", "Springer Nature In Review", 
                                        "Micropublication", "Ubiquity"),
  
  "Data and Infrastructure" = c("OpenAIRE Explore", "OpenAIRE Graph", "OpenAlex", "Figshare", 
                                "Datacite", "CORE (COnnecting REpositories)", 
                                "BASE (Bielefeld Academic Search Engine)", "OASPA", "OA.Works", 
                                "Creative Commons", "OApen", "Unpaywall", "MetaROR", "Sherpa Services", 
                                "DeSci Labs"),
  
  "Open Science and Policies" = c("cOAlition S", "Barcelona Declaration", "Amelica", "SPARC", 
                                  "Redalyc", "Open Research Europe", "ALPSP", "Open Science MOOC", 
                                  "OpenEdition", "Science Explorer (SciX)"),
  
  "Tools and Research Services" = c("Altmetric", "Crossref", "Zotero", "hypothes.is", "scite.ai", 
                                    "Plaudit", "PubPub", "Signals", "Science Open", "SciPost", 
                                    "Scirate", "Curvenote", "protocols.io", "Sciety", "ScienceCast", 
                                    "Cassyni", "Kotahi"),
  
  "Community and Education" = c("ASAPbio", "Forum for Open Research in MENA", "Knowledge Commons", 
                                "MetaDocencia", "Life Science Editors Foundation Praise for Preprints", 
                                "JMIRx", "Octopus", "DocMaps", "Pubpeer", "Arcadia Science", 
                                "Biophysics Colab", "preLights", "Peeriodicals", 
                                "Open for you! Webinar series", "Research Hub", 
                                "DataPASS Journal Editors Discussion Interface listserv", 
                                "Early Evidence Base")
)

# Ajouter la colonne Category
data$Category <- sapply(data$OrgName, function(org) {
  category <- names(categories)[sapply(categories, function(cat) org %in% cat)]
  if (length(category) > 0) return(category) else return("Uncategorized")
})

# Vérifier les organisations toujours non classées
uncategorized <- data$OrgName[data$Category == "Uncategorized"]
print(uncategorized)



# Liste des catégories avec leurs organisations
audiences <- list(
  "Life Science Researchers" = c("Life science researchers, scholarly publishing community","Life science researchers", "Biophysics researchers", "Immunology researchers", "Life science big data researchers", "Researchers in biology, biomedicine, and psychology", "Life science researchers from marginalized backgrounds", "Earth science researchers"),
  "Scholarly Publishing Community" = c("Scholarly publishing community", "Academic journal editors", "Publishers", "Librarians, public, scholarly publishing community", "Research funders, scholarly publishing community", "Scholarly publishing community, research funders", "Researchers publishing on arXiv and bioRxiv", "Researchers, scholarly publishing community", "Publishers, researchers"),
  "Researchers (All Disciplines)" = c("Researchers, all disciplines", "Researchers, all disciplines, scholarly publishing community", "Researchers, all disciplines, with particular interest in the humanities", "Researchers, all disciplines, European focus", "Researchers, all disciplines, Latin America", "Researchers, all disciplines, scholarly publishing community, Latin America", "Researchers, all disciplines, scholarly publishing community, Latin America"),
  "Regional and Linguistic Focus" = c("Researchers, all disciplines, Latin America", "Spanish language focus", "Spanish/Portuguese language focus, Scholarly publishing community", "Researchers, all disciplines, Latin America", "Researchers, all disciplines, Latin America", "Canadian journals and researchers in social sciences and humanities", "Researchers, librarians, students in humanities and social sciences", "Scholarly publishing community, Latin America", "Researchers and librarians in the Middle East and North Africa"),
  "Specialized Research Communities" = c("Physics, math, and computer science researchers", "Space scientists", "Social science researchers", "Metascience researchers", "Research consumers", "Research organisations, universities, funders, and government", "Academic institutions, researchers", "Researchers funded by the Gates Foundation", "Researchers funded by Wellcome", "arXiv readers and researchers", "Big data researchers", "Researchers, publishers, librarians, students in humanities and social sciences")
)

# Appliquer la fonction pour créer la colonne 'audience'
data$audience <- sapply(data$TargetAudience, classify_audience)

# Vérifier les organisations "Uncategorized" à nouveau
uncategorized <- data$TargetAudience[data$audience == "Uncategorized"]

# Afficher les valeurs non catégorisées mises à jour
print(uncategorized)



# Sauvegarder dans un fichier Excel
if (require(openxlsx)) {
  write.xlsx(data, "classified_data.xlsx")
  print("File 'classified_data.xlsx' has been created.")
}

d <- data %>%
  select(Nonprofit, OpenSource, Category, CommunityGovernance) %>% #, audience) %>%
  mutate(across(everything(), as.factor))


class(d$Category)
describe(d$Category)

class(d$audience)
describe(d$audience)


acm <- dudi.acm(d, scannf = F, nf = Inf)

#explor::explor(acm)


res <- explor::prepare_results(acm)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = "Lab",
                     ind_lab_min_contrib = 49, col_var = "CommunityGovernance", labels_size = 8,
                     point_opacity = 0.5, opacity_var = "Cos2", point_size = 90, ellipses = TRUE,
                     transitions = TRUE, labels_positions = "auto", xlim = c(-2.36, 2.12), ylim = c(-2.09,
                                                                                                    2.39))

screeplot(acm) # autre façon de représenter graph des axes
res <- explor::prepare_results(acm)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = "Lab",
                     ind_lab_min_contrib = 0, col_var = "CommunityGovernance", labels_size = 7,
                     point_opacity = 0.5, opacity_var = "Cos2", point_size = 103, ellipses = TRUE,
                     transitions = TRUE, labels_positions = "auto", xlim = c(-2.33, 1.94), ylim = c(-2.02,
                                                                                                    2.24))
##
# Usage du factoextra 
library(factoextra)
fviz_screeplot(acm, choice = "eigenvalue") # inertie
fviz_screeplot(acm) # inertie / inerie totale (en %)
summary(acm) # autre façn de connaitre l'inertie des 5 premiers axes
##

# cercle des corrélations de l'ACM
s.corcircle(acm$co)
s.corcircle(acm$co, clabel = .7) # ajuster les étiquettes
fviz_mca_var(acm)
fviz_mca_var(acm, repel = TRUE) # éviter la superposition des étiquettes avec "repel"

# graphique plus beau des barycentres avec explor : 
# explor::explor(acm)
res <- explor::prepare_results(acm)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = "Variable",
                     size_var = NULL, size_range = c(10, 300), labels_size = 10, point_size = 56,
                     transitions = TRUE, labels_positions = NULL, labels_prepend_var = FALSE,
                     xlim = c(-1.45, 1.81), ylim = c(-2.03, 1.23))

## outil pour analyser la distribution au sein des axes
boxplot(acm) # axe 1 par défaut
boxplot(acm, xax = 2) # choisir l'axe

# Pour analyser la contribution des modalités à la formation des axes
fviz_contrib(acm, choice = "var", axes = 1) #  personnaliser l'axe
fviz_contrib(acm, choice = "var", axes = 2) #  personnaliser l'axe

# Autre façon pour analyser la contribution des modalités à la formation des axes
par(mfrow = c(2, 2))
for (i in 1:4) 
  barplot(acm$cr[, i], names.arg = row.names(acm$cr), las = 2, main = paste("Axe", i))
par(mfrow = c(1, 1)) # à remettre pour ne pas afficher le reste des graphiques deux à deux

# Représentation des individus avec facrotextra (facultatif)
fviz_mca_ind(acm, geom ="point", alpha.ind = .25)


# devtools::install_github("larmarange/JLutils")
# library(JLutils)
# s.freq(acm$li) # La taille des carrés représente le nombre d'observations, plutôt que simplement des point de même taille
# Une autre façon intéressante
s.hist(acm$li, clabel =0 , pch = 15, cbr = 3, adj = 0.5)
# représentation qui lie les observations aux barycentes :
s.class(acm$li, d$Nonprofit, col = c("red", "darkgreen"))
s.class(acm$li, d$CommunityGovernance, col = c("red", "darkgreen"))
# Autre façon intéressante :
fviz_mca_ind(acm)
fviz_mca_ind(acm, geom = "point")
fviz_mca_ind(acm, geom = "point", habillage = d2$sexe)
fviz_mca_ind(acm, geom = "point", habillage = d2$sexe, addEllipses = TRUE)

# Variables supplémentaires / illustratives
s.class(acm$li, d$Nonprofit)
fviz_mca_ind(acm, geom = "point", habillage = d$Category) # cela marche aussi avec "habillahe" à condition de ne pas changer l'ordre des individus !
# Résumer l'information pour toutes les variables
scatter(acm)

# plettes de couleurs : https://www.datanovia.com/en/fr/blog/palette-de-couleurs-rcolorbrewer-de-a-a-z/ 
scatter(acm, col = RColorBrewer::brewer.pal(5, "Set1")) # sortir 5 couleurs de la palette "set1"
scatter(acm, col = RColorBrewer::brewer.pal(n = 8, name = "Dark2")) # sortir 8 couleurs de la palette "Dark2"


fviz_mca_var(acm, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

plotellipses(acm)

# # ACM alternative 
# d3 <- hdv2003 %>%
#   select(peche.chasse, cinema, cuisine, bricol, sport, lecture.bd)
# acm2 <- dudi.acm(d3, scannf = FALSE, nf = Inf)

# explor::explor(acm2)

# visualisation avec les variables sociodémographiques
fviz_mca_ind(acm, geom = "point", habillage = d$Nonprofit, addEllipses = TRUE)
fviz_mca_ind(acm, geom = "point", habillage = d$OpenSource, addEllipses = TRUE)
# Visualisation globale
scatter(acm, col = RColorBrewer::brewer.pal(5, "Set1"))

# Variables supplémentaires avec FactorMineR
acm3 <- MCA(d) # ACM
# ACM avec les variables supplémentaires 
acm3 <- MCA(d, quali.sup = 1:3) # on a la représentation de la corrélation des variables avec les axes


#
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


d$from <- arbre_phi2 |> 
  cutree(4)

d$from <- paste0("group",d$from)

d$to <- rownames(d)
