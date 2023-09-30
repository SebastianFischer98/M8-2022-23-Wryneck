setwd("C:/Users/User/Documents/WWU_Muenster/Studium/00_sciebo/Wendehals Projekt/R/")

library(terra)
library(raster)
library(mapview)
library(sf)
library(dplyr)


##################################################
### Weiterverarbeitung: Canopy Cover (alias cover) ###
##################################################

# Einladen der Daten (Wendehals-Walddaten (cover) in einer Tabelle)
Wendehals_cover <- readRDS("Daten/Wendehals_cover.RDS")


#NULL in "0" ändern
Wendehals_cover$Wendehals_2017_cover_[is.na(Wendehals_cover$Wendehals_2017_cover_)] <- 0
Wendehals_cover$Wendehals_2018_cover_[is.na(Wendehals_cover$Wendehals_2018_cover_)] <- 0
Wendehals_cover$Wendehals_2019_cover_[is.na(Wendehals_cover$Wendehals_2019_cover_)] <- 0
Wendehals_cover$Wendehals_2020_cover_[is.na(Wendehals_cover$Wendehals_2020_cover_)] <- 0
Wendehals_cover$Wendehals_2021_cover_[is.na(Wendehals_cover$Wendehals_2021_cover_)] <- 0
Wendehals_cover$Wendehals_2022_cover_[is.na(Wendehals_cover$Wendehals_2022_cover_)] <- 0

summary(Wendehals_cover)


# Gruppieren nach Revier ("ID") und Mittelwertbildung
Wendehals_cover_group <- 
  summarize(
    group_by(Wendehals_cover, ID),
    Jahr = mean(Jahr, na.rm = TRUE),
    mean_cover2017 = mean(Wendehals_2017_cover_, na.rm = TRUE),
    mean_cover2018 = mean(Wendehals_2018_cover_, na.rm = TRUE),
    mean_cover2019 = mean(Wendehals_2019_cover_, na.rm = TRUE),
    mean_cover2020 = mean(Wendehals_2020_cover_, na.rm = TRUE),
    mean_cover2021 = mean(Wendehals_2021_cover_, na.rm = TRUE),
    mean_cover2022 = mean(Wendehals_2022_cover_, na.rm = TRUE),
    o0_cover = length(which(Wendehals_2017_cover_ != 0))/
      length(Wendehals_2017_cover_) * 100)


saveRDS(Wendehals_cover_group, "Daten/Wendehals_cover_group.RDS")


#mit gruppierten Daten weiterarbeiten
Wendehals_cover <- readRDS("Daten/Wendehals_cover_group.RDS")
rm(Wendehals_cover_group)


#Auswahl der Reviere, in denen >= 70 % der Fläche als Wald definiert wurde und
#nach 2017 (Referenzjahr für Analysen bzgl. Wald) besetzt waren
Wendehals_cover_o70 <- Wendehals_cover[Wendehals_cover$o0_cover >= 70 &
                                         Wendehals_cover$Jahr > 2017,]


# Unter bestimmten Bedingungen eine neue Spalte (cover_Brut) anfügen und mit
# Inhalt füllen (cover aus dem jeweiligen Brutjahr)
# in character umwandeln, da die Funktion nicht mit numeric funktioniert 
# Package: dplyr
Wendehals_cover_o70 <- mutate(Wendehals_cover_o70,
                              cover_Brut = case_when(
                                Jahr == 2018 ~ as.character(mean_cover2018),
                                Jahr == 2019 ~ as.character(mean_cover2019),
                                Jahr == 2020 ~ as.character(mean_cover2020),
                                Jahr == 2021 ~ as.character(mean_cover2021),
                                Jahr == 2022 ~ as.character(mean_cover2022),
                                TRUE ~ NA_character_))

# Spalte "cover_Brut" wieder in numeric umwandeln 
Wendehals_cover_o70$cover_Brut <- as.numeric(Wendehals_cover_o70$cover_Brut)
str(Wendehals_cover_o70)

# nicht benötigte Spalten (Spalten 4-8) löschen
Wendehals_cover_2017vsBrut <- Wendehals_cover_o70[, -c(4:8)]
View(Wendehals_cover_2017vsBrut)

saveRDS(Wendehals_cover_2017vsBrut, "Daten/Wendehals_cover_2017vsBrut.RDS")





##################################################
### Weiterverarbeitung: above ground biomass density (alias agbd) ###
##################################################

# Einladen der Daten (Wendehals-Walddaten (cover) in einer Tabelle)
Wendehals_agbd <- readRDS("Daten/Wendehals_agbd.RDS")


#NULL in "0" ändern
Wendehals_agbd$Wendehals_2017_agbd_[is.na(Wendehals_agbd$Wendehals_2017_agbd_)] <- 0
Wendehals_agbd$Wendehals_2018_agbd_[is.na(Wendehals_agbd$Wendehals_2018_agbd_)] <- 0
Wendehals_agbd$Wendehals_2019_agbd_[is.na(Wendehals_agbd$Wendehals_2019_agbd_)] <- 0
Wendehals_agbd$Wendehals_2020_agbd_[is.na(Wendehals_agbd$Wendehals_2020_agbd_)] <- 0
Wendehals_agbd$Wendehals_2021_agbd_[is.na(Wendehals_agbd$Wendehals_2021_agbd_)] <- 0
Wendehals_agbd$Wendehals_2022_agbd_[is.na(Wendehals_agbd$Wendehals_2022_agbd_)] <- 0

summary(Wendehals_agbd)


# Gruppieren nach Revier ("ID") und Mittelwertbildung
Wendehals_agbd_group <- 
  summarize(
    group_by(Wendehals_agbd, ID),
    Jahr = mean(Jahr, na.rm = TRUE),
    mean_agbd2017 = mean(Wendehals_2017_agbd_, na.rm = TRUE),
    mean_agbd2018 = mean(Wendehals_2018_agbd_, na.rm = TRUE),
    mean_agbd2019 = mean(Wendehals_2019_agbd_, na.rm = TRUE),
    mean_agbd2020 = mean(Wendehals_2020_agbd_, na.rm = TRUE),
    mean_agbd2021 = mean(Wendehals_2021_agbd_, na.rm = TRUE),
    mean_agbd2022 = mean(Wendehals_2022_agbd_, na.rm = TRUE),
    o0_agbd = length(which(Wendehals_2017_agbd_ != 0))/
      length(Wendehals_2017_agbd_) * 100)

saveRDS(Wendehals_agbd_group, "Daten/Wendehals_agbd_group.RDS")


#mit gruppierten Daten weiterarbeiten
Wendehals_agbd <- readRDS("Daten/Wendehals_agbd_group.RDS")
rm(Wendehals_agbd_group)


#Auswahl der Reviere, in denen >= 70 % der Fläche als Wald definiert wurde und
#nach 2017 (Referenzjahr für Analysen bzgl. Wald) besetzt waren
Wendehals_agbd_o70 <- Wendehals_agbd[Wendehals_agbd$o0_agbd >= 70 &
                                         Wendehals_agbd$Jahr > 2017,]


# Unter bestimmten Bedingungen eine neue Spalte (agbd_Brut) anfügen und mit
# Inhalt füllen (agbd aus dem jeweiligen Brutjahr)
# in character umwandeln, da die Funktion nicht mit numeric funktioniert 
# Package: dplyr
Wendehals_agbd_o70 <- mutate(Wendehals_agbd_o70,
                             agbd_Brut = case_when(
                               Jahr == 2018 ~ as.character(mean_agbd2018),
                               Jahr == 2019 ~ as.character(mean_agbd2019),
                               Jahr == 2020 ~ as.character(mean_agbd2020),
                               Jahr == 2021 ~ as.character(mean_agbd2021),
                               Jahr == 2022 ~ as.character(mean_agbd2022),
                               TRUE ~ NA_character_))


# Spalte "agbd_Brut" wieder in numeric umwandeln 
Wendehals_agbd_o70$agbd_Brut <- as.numeric(Wendehals_agbd_o70$agbd_Brut)
str(Wendehals_agbd_o70)

# nicht benötigte Spalten (Spalten 4-8) löschen
Wendehals_agbd_2017vsBrut <- Wendehals_agbd_o70[, -c(4:8)]
View(Wendehals_agbd_2017vsBrut)

saveRDS(Wendehals_agbd_2017vsBrut, "Daten/Wendehals_agbd_2017vsBrut.RDS")


