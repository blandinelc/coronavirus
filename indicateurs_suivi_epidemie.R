library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(RCurl)
library(ggplot2)
library(gghighlight)



# IMPORT DES DONNÉES ------------------------------------------------------



indicateurs_fra <- fread("https://www.data.gouv.fr/fr/datasets/r/d86f11b0-0a62-41c1-bf6e-dc9a408cf7b5") %>%
  mutate(extract_date = as.Date(extract_date))


write_excel_csv2(indicateurs_fra, "~/DATA/coronavirus/exports/fr_indicateurs_suivi_ministere.csv")

ftpUpload("~/DATA/coronavirus/exports/fr_indicateurs_suivi_ministere.csv", 
          to = "")


indicateurs_dep <- fread("https://www.data.gouv.fr/fr/datasets/r/4acad602-d8b1-4516-bc71-7d5574d5f33e") %>%
  mutate(extract_date = as.Date(extract_date))




# TAUX D'INCIDENCE (activité épidémique) ----------------------------------




## Taux d'incidence en France

tx_incid_fr <- indicateurs_fra %>%
  select(extract_date, tx_incid) %>%
  filter(!is.na(tx_incid)) %>%
  mutate(tx_incid = round(tx_incid, 1)) %>%
  arrange(extract_date) %>%
  rename(`Taux d'incidence national` = tx_incid)


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
      )
    
    ggsave("~/DATA/coronavirus/graphiques/tx_incid_fr.png")
    
    ftpUpload("~/DATA/coronavirus/graphiques/tx_incid_fr.png", 
              to = "")



## Taux d'incidence en Bretagne

tx_incid_bzh <- indicateurs_dep %>%
  filter(region ==  "53") %>%
  select(extract_date, `Département` = libelle_dep, `Taux d'incidence départemental` = tx_incid) %>%
  filter(!is.na(`Taux d'incidence départemental`)) %>%
  arrange(extract_date) %>%
  left_join(tx_incid_fr, by = "extract_date")



## Export taux d'incidence

write_csv(tx_incid_bzh, 
          "~/DATA/coronavirus/exports/indicateurs_suivi/bzh_dep_indicateur_taux_incidence.csv", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote_escape = F)

ftpUpload("~/DATA/coronavirus/exports/indicateurs_suivi/bzh_dep_indicateur_taux_incidence.csv", 
          to = "")




# TAUX DE POSITIVITÉ ------------------------------------------------------




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
  select(extract_date, `Département` = libelle_dep, `Taux de positivité départemental` = tx_pos) %>%
  filter(!is.na(`Taux de positivité départemental`)) %>%
  arrange(extract_date) %>%
  mutate(`Taux de positivité départemental` = round(`Taux de positivité départemental`, 2)) %>%
  left_join(tx_pos_fr, by = "extract_date")



## Export taux de positivité

write_csv(tx_pos_bzh, 
          "~/DATA/coronavirus/exports/indicateurs_suivi/bzh_dep_indicateur_taux_positivite.csv", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote_escape = F)

ftpUpload("~/DATA/coronavirus/exports/indicateurs_suivi/bzh_dep_indicateur_taux_positivite.csv", 
          to = "")




# FACTEUR DE REPRODUCTION R -----------------------------------------------




## Bilan par région à la date la plus récente

#Données pour carto
polygones_reg <- fread("~/DATA/coronavirus/donnees_source/regions_fr_om_flourish.csv", encoding = "UTF-8")

#Carte pour les données les plus récentes

fr_reg_taux_R_derniere_maj <- indicateurs_dep %>%
  select(extract_date, region, libelle_reg, R) %>%
  filter(!is.na(R)) %>%
  distinct() %>%
  #group_by(libelle_reg) %>%
  filter(extract_date == max(extract_date)) %>% 
  right_join(polygones_reg, by = c("region" = "code")) %>%
  mutate(
    #catégoriser en fonction du niveau du R
    couleur_R = case_when(R < 1 ~ "vert",
                          R < 1.5 ~ "orange",
                          R >= 1.5 ~ "rouge"),
    #texte pour la légende
    classement_R = case_when(R < 1 ~ "R inférieur à 1",
                             R < 1.5 ~ "R entre 1 et 1.5",
                             R >= 1.5 ~ "R supérieur à 1.5"),
    #format de date en toutes lettres
    extract_date = format(extract_date, "%d %B"),
    #commentaire pour afficher dans la carte
    commentaire = if_else(
      !is.na(R),
      paste0(
        "À la date du ",
        extract_date,
        ", le facteur de reproduction R était estimé à <b>",
        R,
        "</b> dans cette région, ce qui la classe en ",
        couleur_R
      ),
      "Pas de données significatives disponibles à cette date"
    ),
    #créer un champ donnant la date de mise à jour
    date_maj = format(Sys.time(), "%d %B, %Hh")
  ) %>%
  select(geometry, extract_date, libelle_reg, R, couleur_R, classement_R, nom, commentaire, date_maj)



## Export facteur R par région pour carte

write_csv(fr_reg_taux_R_derniere_maj, 
          "~/DATA/coronavirus/exports/indicateurs_suivi/fr_reg_taux_R_carte_derniere_maj.csv", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote_escape = F)

ftpUpload("~/DATA/coronavirus/exports/indicateurs_suivi/fr_reg_taux_R_carte_derniere_maj.csv", 
          to = "")



## Récap de l'évolution du R pour chaque région
# fr_reg_taux_R_histo <- indicateurs_dep %>%
#   select(extract_date, libelle_reg, R) %>%
#   filter(!is.na(R)) %>%
#   distinct() %>%
#   pivot_wider(id_cols = "extract_date", names_from = "libelle_reg", values_from = "R")




# TAUX D'OCCUPATION EN RÉA ------------------------------------------------




## Taux d'occupation SAE national

tx_occup_fr <- indicateurs_fra %>%
  select(extract_date, taux_occupation_sae) %>%
  filter(!is.na(taux_occupation_sae)) %>%
  arrange(extract_date) %>%
  rename(`Taux d'occupation national`= taux_occupation_sae)



## Taux d'occupation SAE par département breton

tx_occup_bzh <- indicateurs_dep %>%
  filter(region ==  "53") %>%
  select(extract_date, `Département` = libelle_dep, `Taux d'occupation régional` = taux_occupation_sae) %>%
  filter(!is.na(`Taux d'occupation régional`)) %>%
  arrange(extract_date) %>%
  left_join(tx_occup_fr, by = "extract_date")



## Export taux d'occupation

write_csv(tx_occup_bzh, 
          "~/DATA/coronavirus/exports/indicateurs_suivi/bzh_indicateur_tx_occup.csv", 
          #éviter que le champ geometry soit cassé par les guillemets
          quote_escape = F)

ftpUpload("~/DATA/coronavirus/exports/indicateurs_suivi/bzh_indicateur_tx_occup.csv", 
          to = "")




# ENSEMBLE DONNÉES INDICATEURS BZH ET FR ----------------------------------




bzh_fr_indicateurs_dep <- left_join(tx_incid_bzh, tx_pos_bzh, by = c("extract_date", "Département")) %>%
  left_join(tx_occup_bzh, by = c("extract_date", "Département"))



write_excel_csv(bzh_fr_indicateurs_dep, "~/DATA/coronavirus/exports/indicateurs_suivi/bzh_fr_indicateurs_suivi_global.csv")

ftpUpload("~/DATA/coronavirus/exports/indicateurs_suivi/bzh_dep_indicateurs_suivi.csv", 
          to = "")



