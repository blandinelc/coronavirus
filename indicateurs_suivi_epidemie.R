library(dplyr) #Traitement de données
library(data.table)
library(readr)
library(tidyr)
library(RCurl) #Export serveur
library(ggplot2) #Packages pour visualisations
library(gghighlight)
library(sf)
library(svglite)



# IMPORT DES DONNÉES ------------------------------------------------------



indicateurs_fra <- read_csv("https://www.data.gouv.fr/fr/datasets/r/d86f11b0-0a62-41c1-bf6e-dc9a408cf7b5")

indicateurs_dep <- read_csv("https://www.data.gouv.fr/fr/datasets/r/4acad602-d8b1-4516-bc71-7d5574d5f33e",
                            locale = locale(encoding = "Latin1"))




# PARAMETRES PAR DEFAUT DES PLOTS -----------------------------------------




#Thème de base
default_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(color = "#727272", face = "italic", size = "4"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(colour = "#444444", size = 7),
    legend.text = element_text(colour = "#444444", size = 7)
  )

#Légende de base
default_legend <- guides(fill = guide_legend(
  #modifier la taille des symboles de légende
  keywidth = 0.8,
  keyheight = 0.8,
  override.aes = list(linetype = 0)
))




# 1/ TAUX D'INCIDENCE (activité épidémique) ----------------------------------




## Taux d'incidence en France

tx_incid_fr <- indicateurs_fra %>%
  select(
    extract_date, 
    tx_incid
    ) %>%
  filter(
    !is.na(tx_incid)
    ) %>%
  mutate(
    tx_incid = round(tx_incid, 1)
    ) %>%
  arrange(
    extract_date
    ) %>%
  rename(
    `Taux d'incidence national` = tx_incid
    )


    ## Graphique évolution fr
    
    ggplot(tx_incid_fr, aes(x = extract_date, y = `Taux d'incidence national`)) +
      geom_path(size = 1, colour = "#BFD5E3") +
      geom_area(alpha = 0.2, fill = "#BFD5E3") +
      theme_light() +
      theme(panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face="bold", 
                                       #color="#993333", 
                                       angle=45),
            axis.line = element_line(colour = "black")) +
      #geom_label() +
      #annotate("text", x = 4, y = 25, label = "Some text") +
      geom_point(size = 0) +
      gghighlight(max(`Taux d'incidence national`), max_highlight = 1, label_key = `Taux d'incidence national`) +
      geom_text(aes(
        label = as.character(max(extract_date), format = "%d %b")),
        size = 4,
        y = 12
      ) +    
    ggsave("")
    
    ftpUpload("")



## Taux d'incidence en Bretagne

tx_incid_bzh <- indicateurs_dep %>%
  filter(
    region ==  "53"
    ) %>%
  select(
    extract_date, 
    Departement = libelle_dep, 
    `Taux d'incidence départemental` = tx_incid
    ) %>%
  filter(
    !is.na(`Taux d'incidence départemental`)
    ) %>%
  arrange(
    extract_date
    ) %>%
  left_join(
    tx_incid_fr, by = "extract_date"
    ) %>%
  mutate_if(
    is.character, 
    enc2utf8
    )



## Export taux d'incidence

write_csv(tx_incid_bzh, 
          "", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote_escape = F)

ftpUpload("")



## Carte fixe du taux d'incidence par département


#Polygones départements
polygones_dep_geojson <- st_read(dsn = "~/DATA/coronavirus/donnees_source/basemap_dep_domtom_proches.geojson", 
                                 layer = "basemap_dep_domtom_proches",
                                 stringsAsFactors = F) %>%
  select(geometry, insee_dep, nom_minuscules)


#Récupérer les données les plus récentes
tx_incid_dep_fixe <- indicateurs_dep %>%
  select(extract_date, code_dep = departement, Departement = libelle_dep, `Taux d'incidence` = tx_incid, couleur = tx_incid_couleur) %>%
  filter(!is.na(`Taux d'incidence`)) %>%
  filter(extract_date == max(extract_date)) %>%
  # mutate(
  #   #catégoriser en fonction du niveau
  #   couleur = case_when(`Taux d'incidence` < 10 ~ "vert",
  #                       `Taux d'incidence` < 50 ~ "orange",
  #                       `Taux d'incidence` >= 50 ~ "rouge")
  #   ) %>%
  right_join(polygones_dep_geojson, by = c("code_dep" = "insee_dep"))


#Créer une carte fixe avec les seuils de couleur
ggplot(tx_incid_dep_fixe) +
  geom_sf(aes(geometry = geometry, fill = couleur), size = 0.5, color = "white") +
  coord_sf(crs = 4326, datum = NA) +
  scale_fill_manual(values = c("vert" = "#3DE4B3",
                               "orange" = "#FFCCA3",
                               "rouge" = "#D1335B"),
                    breaks = c("vert", "orange", "rouge"),
                    labels = c("- de 10", "entre 10 et 50", "+ de 50")) +
  labs(title = "L'activité épidémique par département",
       subtitle = paste("Taux d'incidence au", format(tx_incid_dep_fixe$extract_date, "%d %B")),
       caption = paste0("Données vérifiées au ", format(Sys.Date(), "%d %B")),
       fill = "Cas pour 100.000 hab.") +
  default_theme +
  default_legend +
  ggsave("",
         width = 15, height = 18, units = "cm")


#Export de la carte taux d'indidencesur le serveur
ftpUpload("")




# 2/ TAUX DE POSITIVITÉ ------------------------------------------------------




## Taux de positivité en France

tx_pos_fr <- indicateurs_fra %>%
  select(extract_date, tx_pos) %>%
  filter(!is.na(tx_pos)) %>%
  mutate(tx_pos = round(tx_pos, 2)) %>%
  arrange(extract_date) %>%
  rename(`Taux de positivité national` = tx_pos)



## Taux de positivité en Bretagne

tx_pos_bzh <- indicateurs_dep %>%
  filter(region ==  "53") %>%
  select(extract_date, Departement = libelle_dep, `Taux de positivité departemental` = tx_pos) %>%
  filter(!is.na(`Taux de positivité departemental`)) %>%
  arrange(extract_date) %>%
  mutate(`Taux de positivité departemental` = round(`Taux de positivité departemental`, 2)) %>%
  left_join(tx_pos_fr, by = "extract_date")



## Export taux de positivité

write.csv(tx_pos_bzh, 
          "", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote = F,
          fileEncoding = "UTF-8",
          row.names = F)

ftpUpload("")




## Carte taux de positivité


#Récupérer les données les plus récentes
tx_pos_dep_fixe <- indicateurs_dep %>%
  select(extract_date, code_dep = departement, Departement = libelle_dep, `Taux de positivité` = tx_pos) %>%
  filter(!is.na(`Taux de positivité`)) %>%
  filter(extract_date == max(extract_date)) %>%
  mutate(
    #catégoriser en fonction du niveau
    couleur = case_when(`Taux de positivité` < 5 ~ "vert",
                        `Taux de positivité` < 10 ~ "orange",
                        `Taux de positivité` >= 10 ~ "rouge")
  ) %>%
  right_join(polygones_dep_geojson, by = c("code_dep" = "insee_dep"))



#Construire la carte avec les seuils de couleur
ggplot(tx_pos_dep_fixe) +
  geom_sf(aes(geometry = geometry, fill = couleur), size = 1, color = "white") +
  coord_sf(crs = 4326, datum = NA) +
  scale_fill_manual(values = c("vert" = "#3DE4B3",
                               "orange" = "#FFCCA3",
                               "rouge" = "#D1335B"),
                    breaks = c("vert", "orange", "rouge"),
                    labels = c("- de 5%", "entre 5% et 10%", "+ de 10%")) +
  labs(title = "La part de tests positifs par département",
       subtitle = paste("Taux de positivité au", format(tx_incid_dep_fixe$extract_date, "%d %B")),
       caption = paste0("Données vérifiées au ", format(Sys.Date(), "%d %B")),
       fill = "% de tests positifs") +
  default_theme +
  default_legend +
  ggsave("",
         limitsize = T)

#Export de la carte taux d'indidencesur le serveur
ftpUpload("")



# 3/ FACTEUR DE REPRODUCTION R -----------------------------------------------




## Bilan par région à la date la plus récente


#Données pour carto reg
polygones_reg_csv <- fread("~/DATA/coronavirus/donnees_source/regions_fr_om_flourish.csv", encoding = "UTF-8")


#Carte pour les données les plus récentes
fr_reg_taux_R_derniere_maj <- indicateurs_dep %>%
  select(extract_date, region, libelle_reg, R) %>%
  filter(!is.na(R)) %>%
  distinct() %>%
  #group_by(libelle_reg) %>%
  filter(extract_date == max(extract_date)) %>% 
  right_join(polygones_reg_csv, by = c("region" = "code")) %>%
  mutate(
    #catégoriser en fonction du niveau du R
    couleur_R = case_when(R < 1 ~ "vert",
                          R < 1.5 ~ "orange",
                          R >= 1.5 ~ "rouge"),
    #texte pour la légende
    classement_R = case_when(R < 1 ~ "R au-dessous de 1",
                             R < 1.5 ~ "R entre 1 et 1.5",
                             R >= 1.5 ~ "R au-dessus de 1.5"),
    #format de date en toutes lettres
    extract_date = format(extract_date, "%d %B"),
    #commentaire pour afficher dans la carte
    commentaire = if_else(
      !is.na(R),
      paste0(
        "Facteur de reproduction R de <b>",
        R,
        "</b> au <b>",
        extract_date,
        "</b>, ce qui correspond au classement ",
        couleur_R
      ),
      "Pas d'estimation disponible pour cette date"
    ),
    #créer un champ donnant la date de mise à jour
    date_maj = format(Sys.time(), "%d %B, %Hh")
  ) %>%
  select(geometry, extract_date, libelle_reg, R, couleur_R, classement_R, nom, commentaire, date_maj)

Encoding(fr_reg_taux_R_derniere_maj$commentaire) <- "latin1"



## Export facteur R par région pour carte

write_csv(fr_reg_taux_R_derniere_maj, 
          "", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote_escape = F)

ftpUpload("")



## Récap de l'évolution du R pour chaque région

fr_reg_taux_R_histo <- indicateurs_dep %>%
  select(extract_date, libelle_reg, R) %>%
  filter(!is.na(R)) %>%
  distinct() %>%
  pivot_wider(id_cols = "extract_date", names_from = "libelle_reg", values_from = "R")



## Export historique des facteurs R par région

write.csv(fr_reg_taux_R_histo, 
          "", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote = T,
          fileEncoding = "UTF-8",
          row.names = F)

ftpUpload("")





# 4/ TAUX D'OCCUPATION EN RÉA ------------------------------------------------




## Taux d'occupation SAE national

tx_occup_fr <- indicateurs_fra %>%
  select(extract_date, taux_occupation_sae) %>%
  filter(!is.na(taux_occupation_sae)) %>%
  arrange(extract_date) %>%
  rename(`Taux d'occupation national`= taux_occupation_sae)



## Taux d'occupation SAE par département breton

tx_occup_bzh <- indicateurs_dep %>%
  filter(region ==  "53") %>%
  select(extract_date, Departement = libelle_dep, `Taux d'occupation regional` = taux_occupation_sae) %>%
  filter(!is.na(`Taux d'occupation regional`)) %>%
  arrange(extract_date) %>%
  left_join(tx_occup_fr, by = "extract_date")



## Export taux d'occupation

write_excel_csv(tx_occup_bzh, 
          "", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote_escape = F)

ftpUpload("")



## Carte taux d'occupation SAE


#Récupérer les données les plus récentes
tx_occup_dep_fixe <- indicateurs_dep %>%
  select(
    extract_date,
    code_dep = departement,
    Departement = libelle_dep,
    `Taux d'occupation` = taux_occupation_sae,
    couleur = taux_occupation_sae_couleur
  ) %>%
  filter(!is.na(`Taux d'occupation`)) %>%
  filter(extract_date == max(extract_date)) %>%
  # mutate(
  #   #catégoriser en fonction du niveau
  #   couleur = case_when(`Taux d'occupation` < 40 ~ "vert",
  #                       `Taux d'occupation` < 60 ~ "orange",
  #                       `Taux d'occupation` >= 60 ~ "rouge")
  # ) %>%
  right_join(polygones_dep_geojson, by = c("code_dep" = "insee_dep"))



#Construire la carte avec les seuils de couleur
ggplot(tx_pos_dep_fixe) +
  geom_sf(aes(geometry = geometry, fill = couleur),
          size = 1,
          color = "white") +
  coord_sf(crs = 4326, datum = NA) +
  scale_fill_manual(
    values = c(
      "vert" = "#3DE4B3",
      "orange" = "#FFCCA3",
      "rouge" = "#D1335B"
    ),
    breaks = c("vert", "orange", "rouge"),
    labels = c("- de 5%", "entre 5% et 10%", "+ de 10%")
  ) +
  labs(
    title = "L'occupation des services de réanimation par département",
    subtitle = paste(
      "Taux d'occupation par rapport aux capacités habituelles, au",
      format(tx_incid_dep_fixe$extract_date, "%d %B")
    ),
    caption = paste0("Données vérifiées au ", format(Sys.Date(), "%d %B")),
    fill = "% de lits occupés"
  ) +
  default_theme +
  default_legend +
  ggsave("",
         limitsize = T)

#Export de la carte taux d'indidencesur le serveur
ftpUpload("")





# ENSEMBLE DONNÉES INDICATEURS BZH ET FR ----------------------------------




bzh_fr_indicateurs_dep <- left_join(tx_incid_bzh, tx_pos_bzh, by = c("extract_date", "Departement")) %>%
  left_join(tx_occup_bzh, by = c("extract_date", "Departement")) %>%
  mutate_if(is.character, enc2native)



write_csv(bzh_fr_indicateurs_dep, "")

ftpUpload("")



