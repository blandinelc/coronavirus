library(dplyr)
library(data.table)
library(lubridate)
library(stringr)

## SpF - Données globales hospitalières  -------------------------------------------------------------


data_confirmes <- fread("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",
                        encoding = "UTF-8")




data_confirmes_difference_rea <- data_confirmes %>%
  filter(sexe == "0",
         dep %in% c("22", "29", "35", "44", "56")) %>%
  mutate(
    hosp_sans_rea = hosp - rea,
    dep = case_when(dep == "22" ~ "Côtes-d'Armor",
                    dep == "29" ~ "Finistère",
                    dep == "35" ~ "Ille-et-Vilaine",
                    dep == "44" ~ "Loire-Atlantique",
                    dep == "56" ~ "Morbihan")
  ) %>%
  select(-sexe)




total_bzh <- data_confirmes %>%
  mutate(hosp_sans_rea = hosp - rea) %>%
  filter(sexe == "0",
         dep %in% c("22", "29", "35", "56")) %>%
  group_by(jour) %>%
  summarize(hosp = sum(hosp),
            rea = sum(rea),
            rad = sum(rad),
            dc = sum(dc),
            hosp_sans_rea = sum(hosp_sans_rea)) %>%
  mutate(dep = "Bretagne administrative") %>%
  bind_rows(data_confirmes_difference_rea) %>%
  select(jour, 
         dep, 
         hospitalisations = hosp, 
         reanimations = rea, 
         retours_domicile = rad, 
         deces = dc, 
         hosp_sans_rea) %>%
  arrange(jour) %>%
  mutate(jour = paste(str_pad(day(jour), width = 2, pad = 0), 
                      str_pad(month(jour), width = 2, pad = 0),
                      str_pad(year(jour), width = 2, pad = 0),
                      sep = "/"))



