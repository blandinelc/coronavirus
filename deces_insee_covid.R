library(dplyr)
library(purrr)
library(readxl)
library(clipr)
library(sf)



# Importer l'ensemble des données téléchargées ----------------------------



xl_data <- list.files("donnees_source/insee/deces_derniere_maj", full.names = T)
#xl_sheets <- c("France sauf 13", "22", "29", "35", "56")

#Sélectionner toutes les feuilles sauf la première
xl_sheets <- (excel_sheets(xl_data))[-1]




#Créer une liste regroupant les feuilles choisies
datainsee <- lapply(xl_sheets, #appliquer à chaque feuille
             function(x) read_excel(
               path = xl_data, 
               sheet = x,
               range = "A5:G65",
               #éviter les problèmes de format des col
               col_types = c("date",
                             "numeric", 
                             "numeric", 
                             "numeric", 
                             "numeric", 
                             "numeric", 
                             "numeric"),
               #noms des colonnes
               col_names = c("date", 
                             "dematerialise2020",
                             "total2020",
                             "dematerialise2019",
                             "total2019",
                             "dematerialise2018",
                             "total2018"))) %>%
  #Reprendre le nom des feuilles pour identifier les départements
  set_names(xl_sheets) %>%
  #Regrouper en faisant apparaître un champ "dep"
  bind_rows(.id = "dep")




#Charger un geojson des départements -------------------------------------

dep_geo <- read_sf(dsn = "donnees_source/geo_departements/basemap_dep_domtom_proches.geojson", layer = "basemap_dep_domtom_proches") %>%
  select(insee_dep, insee_reg, nom_dep, geometry)




#Calculer la variation sur un an -----------------------------------------

taux_variation_dep <- datainsee %>%
  filter(!is.na(total2020)) %>%
  filter(date == max(date)) %>%
  select(dep, date, total2020, total2019) %>%
  mutate(taux_variation = ((total2020 - total2019) / total2019 * 100) %>% round(digits = 1)) %>%
  right_join(dep_geo, by = c("dep" = "insee_dep"))

st_write(taux_variation_dep, dsn = paste0("exports/deces_insee_variation_", Sys.Date(), ".geojson"), 
         layer = paste0("exports/deces_insee_variation_", Sys.Date()), delete_dsn = T)


write_clip(taux_variation_dep$taux_variation)

#Isoler et cumuler les départements bretons ------------------------------


datainsee_bzh <- datainsee %>%
  filter(dep %in% c("22", "29", "35", "56")) %>%
  group_by(date) %>%
  summarize(dematerialise2020 = sum(dematerialise2020),
            total2020 = sum(total2020),
            dematerialise2019 = sum(dematerialise2019),
            total2019 = sum(total2019),
            dematerialise2018 = sum(dematerialise2018),
            total2018 = sum(total2018)) %>%
  mutate(deces_journaliers_2020 = total2020 - lag(total2020),
         deces_journaliers_2019 = total2019 - lag(total2019),
         deces_journaliers_2018 = total2018 - lag(total2018))


write_clip(datainsee_bzh$total2020)

write_clip(datainsee_bzh$deces_journaliers_2020)
write_clip(datainsee_bzh$deces_journaliers_2019)
write_clip(datainsee_bzh$deces_journaliers_2018)

#Format Flourish décès totaux
deces_total_bzh_flourish <- datainsee_bzh %>%
  select(total2020, total2019, total2018)

write_clip(deces_total_bzh_flourish)



#Format Flourish décès transmis par voie dématérialisée
dematerialises_bzh_flourish <- datainsee_bzh %>%
  select(dematerialise2020, dematerialise2019, dematerialise2018)

write_clip(dematerialises_bzh_flourish)





#Méthode pour télécharger directement le zip en ligne --------------------



library(purrr)


url <- "https://www.insee.fr/fr/statistiques/fichier/4470857/2020-04-03_deces_quotidiens_departement.zip"

#L'emplacement où télécharger le fichier (le workfolder)
path_zip <- "."

#L'emplacement où dézipper le fichier (dossier pour list.files)
path_unzip <- "./donnees_source/insee"

#Nom du fichier ZIP téléchargé
destfile <- "archive.zip"



# download zip
curl::curl_download(url, destfile = paste(path_zip, destfile, sep = "/"))

#unzip
unzip(destfile)

# list all files
files <- list.files(path = path_unzip)

# apply map_df() to iterate read_csv over files
data <- map_df(paste(path_unzip, files, sep = "/"),
               read_csv,
               ## additional params to read_csv here
)



#####
## Ancienne version du script sans fonction --------------------------------



datainsee_fr <- read_xlsx("donnees_source/insee/2020-04-03_deces_quotidiens_departement.xlsx",
                          sheet = 2,
                          range = "A5:H35",
                          col_types = c("date", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric"),
                          col_names = c("date", 
                                        "dematerialise2020",
                                        "total2020",
                                        "taux_dematerialisation2020",
                                        "dematerialise2019",
                                        "total2019",
                                        "dematerialise2018",
                                        "total2018"))

datainsee_22 <- read_xlsx("donnees_source/insee/2020-04-03_deces_quotidiens_departement.xlsx",
                          sheet = "22",
                          range = "A5:H35",
                          col_types = c("date", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric"),
                          col_names = c("date", 
                                        "dematerialise2020",
                                        "total2020",
                                        "taux_dematerialisation2020",
                                        "dematerialise2019",
                                        "total2019",
                                        "dematerialise2018",
                                        "total2018"))

datainsee_29 <- read_xlsx("donnees_source/insee/2020-04-03_deces_quotidiens_departement.xlsx",
                          sheet = "29",
                          range = "A5:H35",
                          col_types = c("date", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric"),
                          col_names = c("date", 
                                        "dematerialise2020",
                                        "total2020",
                                        "taux_dematerialisation2020",
                                        "dematerialise2019",
                                        "total2019",
                                        "dematerialise2018",
                                        "total2018"))

datainsee_35 <- read_xlsx("donnees_source/insee/2020-04-03_deces_quotidiens_departement.xlsx",
                          sheet = "35",
                          range = "A5:H35",
                          col_types = c("date", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric"),
                          col_names = c("date", 
                                        "dematerialise2020",
                                        "total2020",
                                        "taux_dematerialisation2020",
                                        "dematerialise2019",
                                        "total2019",
                                        "dematerialise2018",
                                        "total2018"))

datainsee_56 <- read_xlsx("donnees_source/insee/2020-04-03_deces_quotidiens_departement.xlsx",
                          sheet = "56",
                          range = "A5:H35",
                          col_types = c("date", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric", 
                                        "numeric"),
                          col_names = c("date", 
                                        "dematerialise2020",
                                        "total2020",
                                        "taux_dematerialisation2020",
                                        "dematerialise2019",
                                        "total2019",
                                        "dematerialise2018",
                                        "total2018"))


