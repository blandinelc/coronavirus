library(dplyr)
library(readxl)
library(tidyr)
library(tibble)
library(clipr)





## Stats du dernier jour le plus récent -------------------------------------------------




#Récupérer la date de la dernière mise à jour dans le fichier
date_maj <- read_xlsx("donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                  sheet = 2,
                  range = "H6",
                  col_names = "date") %>%
  mutate(date = as.character(date)) %>%
  mutate(date = paste(substring(date, 9, 10), substring(date, 6, 7), sep = "/")) %>%
  as.character()



#Lire la colonne pour le jour le plus récent avec la date_maj comme nom
data <- read_xlsx("donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                  sheet = 2,
                  range = c("N8:N17"),
                  col_names = date_maj) %>%
  mutate_if(is.numeric, round, digits = 1)




#Récupérer les seuils des âges et grouper avec la colonne data
deces_age <- read_xlsx("donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                  sheet = 2,
                  range = "A7:A17") %>%
  bind_cols(data)









## Fonctions pour récupérer les données de chaque jour ---------------------




#Tests pour comprendre

#Récupérer la date de la veille : le jour le plus récent + 7 colonnes
date_test <- read_xlsx("donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                       sheet = 2,
                       range = cell_limits(c(6, 8 + 7), c(6, 8 + 7)),
                       col_names = "date") %>%
  mutate(date = as.character(date)) %>%
  mutate(date = paste(substring(date, 9, 10), substring(date, 6, 7), sep = "/")) %>%
  as.character()






#Récupérer les données de la veille : celles du jour le plus récent + 7 colonnes
data_test <- read_xlsx("donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                  sheet = 2,
                  #Définit la zone avec ligne-col min et ligne-col max
                  range = cell_limits(c(8, 14 + 7), c(17, 14 + 7)),
                  col_names = F)





## Version automatisée avec lapply()



#Chemin du fichier
xl_data <- "donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx"


#Nombre de jours à traiter multiplié par les 7 colonnes (au hasard, simplement pour avoir une limite)
colonnes <- c(0:20) * 7



#Isoler les jours présents dans le fichier
dates <- lapply(colonnes, 
                #fonction lisant le excel avec un décalage de 7 cols à chaque fois
                function(x) (read_xlsx("donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                                       sheet = 2,
                                       range = cell_limits(c(6, 8 + x), c(6, 8 + x)),
                                       col_names = "jour")
                )) %>%
  #transformer en df sans noms et avec une col "date"
  enframe(name = NULL, value = "jour") %>%
  #délister les dates
  unnest(cols = c(jour)) %>%
  #Transformer les jour en caractères et les afficher au format voulu
  mutate_all(as.character) %>%
  mutate(jour = paste(substring(jour, 9, 10), substring(jour, 6, 7), sep = "/")) %>%
  #Transposer la colonne en ligne car les data de chaque jour sont en lignes
  t()



#Récupérer la part tous sexes confondus pour chacun des jour
datas <- lapply(colonnes, function(x) read_excel(
                       path = "donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                       sheet = 2,
                       range = cell_limits(c(8, 14 + x), c(17, 14 + x)),
                       col_names = F) %>%
                  #Arrondir les données
                  mutate_if(is.numeric, round, digits = 1)) %>%
  #Regrouper toutes les colonnes car les lignes correspondent
  bind_cols() %>%
  #Utiliser les dates comme noms de jour
  setNames(dates)



#Récupérer les seuils des âges et grouper avec la colonne data
deces_age <- read_xlsx("donnees_source/ined/Deaths-Age-Sex_Covid-19_France_06-04.xlsx",
                       sheet = 2,
                       range = "A7:A17") %>%
  bind_cols(datas) %>%
  rename(groupe_age = `Age Group`) %>% 
  pivot_longer(cols = c(-groupe_age), names_to = "date", values_to = "part") %>%
  pivot_wider(names_from = groupe_age, values_from = part) %>%
  mutate(date = as.Date(date, format = "%d/%m")) %>%
  #mutate(date = format(date, "%d/%m")) %>%
  arrange(date) %>%
  mutate(date = format(date, "%d/%m"))



write_clip(deces_age)
