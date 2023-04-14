#This script assigns all languages in glottolog_df to their nearest AUTOTYP area

#Script was written by Hedvig Skirgård

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  fields, #for calculating distances
  tibble, #for manipulating rownames
  readr #for reading in data files
)

options(tidyverse.quiet = TRUE)

glottolog_df <- read_tsv("./data/glottolog-cldf_wide_df.tsv") %>%
  dplyr::select(Glottocode, Longitude, Latitude, Family_ID, Name)

##Adding in areas of linguistic contact from AUTOTYP

AUTOTYP <- read_csv("https://raw.githubusercontent.com/autotyp/autotyp-data/master/data/csv/Register.csv") %>% 
  dplyr::select(Glottocode, Area, Longitude, Latitude) %>% 
  filter(Glottocode != "balk1252") %>% #There's a set of languages in autotyp that have more than one area, for now they're just hardcoded excluded in these lines
  filter(Glottocode != "east2295") %>%
  filter(Glottocode != "indo1316") %>%
  filter(Glottocode != "kyer1238") %>%
  filter(Glottocode != "mart1256") %>%
  filter(Glottocode != "minn1241") %>%
  filter(Glottocode != "noga1249") %>%
  filter(Glottocode != "oira1263") %>%
  filter(Glottocode != "peri1253") %>%
  filter(Glottocode != "taha1241") %>%
  filter(Glottocode != "tibe1272") %>%
  filter(Glottocode != "till1254") %>%
  filter(Glottocode != "toho1245") %>%
  filter(Glottocode != "kati1270")

#This next bit where we find the autotyp areas of languages was written by Y
# We know the autotyp-area of langauges in autotyp and their long lat. We don't know the autotyp area of languages in Glottolog. We also can't be sure that the long lat of languoids with the same glottoids in autotyp and glottolog_df have the exact identical long lat. First let's make two datasets, one for autotyp languages (hence lgs where we know the area) and those that we wish to know about, the Glottolog ones.

lgs_with_known_area <- as.matrix(AUTOTYP[!is.na(AUTOTYP$Area),c("Longitude","Latitude")])
rownames(lgs_with_known_area) <- AUTOTYP[!is.na(AUTOTYP$Area),]$Glottocode

known_areas <- AUTOTYP %>% 
  dplyr::filter(!is.na(Area)) %>% 
  dplyr::select(Glottocode, Area) %>% 
  distinct() %>% 
  dplyr::select(AUTOTYP_Glottocode = Glottocode, everything())

rm(AUTOTYP)

lgs_with_unknown_area <- as.matrix(glottolog_df[,c("Longitude","Latitude")])
rownames(lgs_with_unknown_area) <- glottolog_df$Glottocode

# For missing, find area of closest langauge
atDist <- rdist.earth(lgs_with_known_area,lgs_with_unknown_area, miles = F)

rm(lgs_with_known_area, lgs_with_unknown_area)

df_matched_up <- as.data.frame(unlist(apply(atDist, 2, function(x){names(which.min(x))})), stringsAsFactors = F) %>% 
  rename(AUTOTYP_Glottocode = `unlist(apply(atDist, 2, function(x) {     names(which.min(x)) }))`)

glottolog_df_with_AUTOTYP <- df_matched_up %>% 
  tibble::rownames_to_column("Glottocode") %>%
  full_join(known_areas) %>% 
  right_join(glottolog_df) %>% 
  dplyr::select(-AUTOTYP_Glottocode) %>% 
  rename(AUTOTYP_area = Area)

glottolog_df_with_AUTOTYP %>% 
  write_tsv("data/glottolog_AUTOTYPE_areas.tsv")