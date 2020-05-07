library(dplyr)
#library(tidyr) #pour utiliser replace_na
library(data.table)
library(stringr)
library(sf)
library(ggplot2)



## Passages aux urgences sur 7 jours ----



#Récupérer les données actualisées
data_sursaud <- fread("https://www.data.gouv.fr/fr/datasets/r/eceb9fb4-3ebc-4da3-828d-f5939712600a",
                      encoding = "UTF-8")



passages_7jrs <- data_sursaud %>%
  filter(sursaud_cl_age_corona == 0) %>%
  select(1:5) %>%
  mutate(date_de_passage = as.Date(date_de_passage, format = "%Y-%m-%d"),
         #remplacer les valeurs manquantes par un zéro avec data.table - ou replace_na()
         nbre_pass_corona = data.table::nafill(nbre_pass_corona, fill = 0),
         nbre_pass_tot = data.table::nafill(nbre_pass_tot, fill = 0)) %>%
  #conserver les 7 derniers jours seulement
  filter(date_de_passage > (max(date_de_passage) - 7)) %>%
  group_by(dep) %>%
  summarize(nbre_pass_corona = sum(nbre_pass_corona),
            nbre_pass_tot = sum(nbre_pass_tot)) %>%
  mutate(taux_pass = round(nbre_pass_corona / nbre_pass_tot * 100),
         #gérer les divisions par zéro
         taux_pass = if_else(is.nan(taux_pass) == T, 0, taux_pass),
         statut_passages = case_when(taux_pass <= 6 ~ "vert",
                                     taux_pass <= 10 ~ "orange",
                                     taux_pass > 10 ~ "rouge"))




## Carte des départements indicateur 1 ----

#https://r-spatial.github.io/sf/articles/sf5.html pour créer cartes avec package sf et ggplot
#cheatsheet couleurs dans R https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf



dep_geo <- sf::read_sf(dsn = "donnees_source/geo_departements/basemap_dep_domtom_proches.geojson", layer = "basemap_dep_domtom_proches") %>%
  select(insee_dep, insee_reg, nom_dep, geometry) %>%
  #enlève le trait de séparation entre Outre-mer et Hexagone
  filter(nom_dep != "separation")

carte_passages <- left_join(dep_geo, passages_7jrs, dep_geo, by = c("insee_dep" = "dep"))


#plot(dep_geo["taux_pass"], breaks = c(0,6,10,200))


ggplot() + 
  geom_sf(data = carte_passages, aes(fill = statut_passages)) +
  coord_sf(datum = NA) + #enlève graticules et coordonnées géo
  scale_fill_manual(values = c("seagreen1", "orange", "red3"), 
                    aesthetics = "fill", 
                    breaks = c("vert", "orange", "rouge")) +
  theme_minimal()


ggsave("carte_indicateur_1", path = "exports", device = "svg")




## Saturation des services de réa ----


donnees_hosp <- fread("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",
                       encoding = "UTF-8")


capacites_rea <- fread("donnees_source/lits_rea_dep_2018.csv") %>%
  select(reg, dep, lits) %>%
  mutate(dep = stringr::str_pad(dep, width = 2, pad = "0")) %>%
  #remplacer les numéros de l'Outre-mer
  mutate(dep = case_when(dep == "9A" ~ "971",
                         dep == "9B" ~ "972",
                         dep == "9C" ~ "973",
                         dep == "9D" ~ "974",
                         dep == "9F" ~ "976",
                         dep = T ~ dep)) %>%
  group_by(reg, dep) %>%
  summarize(lits = sum(lits, na.rm = T)) %>%
  distinct()


rea_7jrs <- donnees_hosp %>%
  filter(sexe == 0,
         as.Date(jour) > max(as.Date(jour)) - 7 ) %>%
  select(1, 5) %>%
  #calculer la moyenne sur 7 jours des départements
  group_by(dep) %>%
  summarize(rea_7jrs = round(sum(rea) / 7)) %>%
  distinct() %>%
  #ajouter les capacités dép et rég de réa
  left_join(capacites_rea, by = "dep") %>%
  #calculer les capacités régionales et leur occupation
  group_by(reg) %>%
  mutate(rea_7jrs = sum(rea_7jrs),
         lits = sum(lits),
         taux_occup_reg = round(rea_7jrs / lits * 100),
         statut_rea = case_when(taux_occup_reg <= 60 ~ "vert",
                                taux_occup_reg <= 80 ~ "orange",
                                taux_occup_reg > 80 ~ "rouge"))


#Faire la carte

left_join(dep_geo, rea_7jrs, by = c("insee_dep" = "dep")) %>%
  ggplot() +
  geom_sf(aes(fill = statut_rea)) +
  scale_fill_manual(values = c("seagreen1", "orange", "red3"), 
                    aesthetics = "fill", 
                    breaks = c("vert", "orange", "rouge")) +
  coord_sf(datum = NA) +
  theme_minimal()

ggsave("carte_indicateur_2", path = "exports", device = "svg")
