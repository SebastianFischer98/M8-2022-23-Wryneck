setwd("C:/Users/User/Documents/WWU_Muenster/Studium/00_sciebo/Wendehals Projekt/R/")

library(terra)
library(raster)
library(mapview)
library(sf)


##################################################
### Extrahieren der Canopy Cover (alias cover) ###
##################################################

cover <- rast(c("Daten/Raster_Wald/Wendehals_2017_cover_.tif",
                "Daten/Raster_Wald/Wendehals_2018_cover_.tif",
                "Daten/Raster_Wald/Wendehals_2019_cover_.tif",
                "Daten/Raster_Wald/Wendehals_2020_cover_.tif",
                "Daten/Raster_Wald/Wendehals_2021_cover_.tif",
                "Daten/Raster_Wald/Wendehals_2022_cover_.tif"))
summary(cover)
head(cover)


#read vector data (Wendehals Brutvorkommen)
Wendehals_buffer <- read_sf(dsn = "Daten/Wendehals.gpkg",
                            layer = "wendehals_nw_2016_2022_wgs84_buffer350m")
mapview(Wendehals_buffer)


#Verknüpfen von Wald-Daten (cover) mit Wendehals-Daten
extr <- extract(cover, Wendehals_buffer)
Wendehals_buffer$ID <- 1:nrow(Wendehals_buffer)
extr <- merge(extr, Wendehals_buffer, by.x="ID", by.y="ID")
head(extr)
saveRDS(extr, file = "Daten/Wendehals_cover.RDS")




##################################################
### Extrahieren der above ground biomass density (alias agbd) ###
##################################################

agbd <- rast(c("Daten/Raster_Wald/Wendehals_2017_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2018_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2019_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2020_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2021_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2022_agbd_.tif"))
summary(agbd)
head(agbd)


#read vector data (Wendehals Brutvorkommen)
Wendehals_buffer <- read_sf(dsn = "Daten/Wendehals.gpkg",
                            layer = "wendehals_nw_2016_2022_wgs84_buffer350m")


#Verknüpfen von Wald-Daten (agbd) mit Wendehals-Daten
extr <- extract(agbd, Wendehals_buffer)
Wendehals_buffer$ID <- 1:nrow(Wendehals_buffer)
extr <- merge(extr, Wendehals_buffer, by.x="ID", by.y="ID")
head(extr)
saveRDS(extr, file = "Daten/Wendehals_agbd.RDS")

