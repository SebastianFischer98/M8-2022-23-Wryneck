setwd("C:/Users/User/Documents/WWU_Muenster/Studium/00_sciebo/Wendehals Projekt/R/")

library(terra)
library(dplyr)
library(ggplot2)


# Visualisierung der gesamten Walddaten
agbd <- rast(c("Daten/Raster_Wald/Wendehals_2017_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2018_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2019_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2020_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2021_agbd_.tif",
               "Daten/Raster_Wald/Wendehals_2022_agbd_.tif"))
agbd$Wendehals_2022_2017_agbd_ <- agbd$Wendehals_2022_agbd_ - agbd$Wendehals_2017_agbd_
plot(agbd$Wendehals_2022_2017_agbd_)
plot(agbd$Wendehals_2017_agbd_)
plot(agbd$Wendehals_2022_agbd_)



#Wendehals-Walddaten einlesen (Vergleich 2017 mit Brutjahr sowie alle Jahre; agbd)
Wendehals_agbd_2017vsBrut <- readRDS("Daten/Wendehals_agbd_2017vsBrut.RDS")
Wendehals_agbd <- readRDS("Daten/Wendehals_agbd_group.RDS")



#Subset erstellen
Wendehals_2017_agbd <- Wendehals_agbd[Wendehals_agbd$Jahr == '2017',]
Wendehals_2018_agbd <- Wendehals_agbd[Wendehals_agbd$Jahr == '2018',]
Wendehals_2019_agbd <- Wendehals_agbd[Wendehals_agbd$Jahr == '2019',]
Wendehals_2020_agbd <- Wendehals_agbd[Wendehals_agbd$Jahr == '2020',]
Wendehals_2021_agbd <- Wendehals_agbd[Wendehals_agbd$Jahr == '2021',]
Wendehals_2022_agbd <- Wendehals_agbd[Wendehals_agbd$Jahr == '2022',]




#################################
########## Abbildungen ##########


### Abb. 1: Wendehals Brutpaare sowie Waldzustand der Brutreviere aus 2022 seit 2017

Wendehals_2022_Waldzustand_agbd <- data.frame(
  Jahr = 2017:2022,
  Reviere = c(length(which(Wendehals_agbd$Jahr == '2017')),
              length(which(Wendehals_agbd$Jahr == '2018')),
              length(which(Wendehals_agbd$Jahr == '2019')),
              length(which(Wendehals_agbd$Jahr == '2020')),
              length(which(Wendehals_agbd$Jahr == '2021')),
              length(which(Wendehals_agbd$Jahr == '2022'))),
  mean_agbd = c(mean(Wendehals_2022_agbd$mean_agbd2017),
                mean(Wendehals_2022_agbd$mean_agbd2018),
                mean(Wendehals_2022_agbd$mean_agbd2019),
                mean(Wendehals_2022_agbd$mean_agbd2020),
                mean(Wendehals_2022_agbd$mean_agbd2021),
                mean(Wendehals_2022_agbd$mean_agbd2022)))

ggsave(filename = "Plots/Abb1_Reviere_Waldzustand_agbd.png",
       plot = (ggplot() +
                 geom_bar(data = Wendehals_2022_Waldzustand_agbd,
                          mapping = aes(x = Jahr, y = Reviere),
                          stat = "identity",
                          fill = "green2", alpha = 0.4, width = 0.9) +
                 geom_line(data = Wendehals_2022_Waldzustand_agbd,
                           mapping = aes(x = Jahr, y = mean_agbd),
                           col = "green4", size = 1) +
                 xlab("year") + ylab("breeding pairs | AGBD [t/ha]") +
                 scale_x_continuous(breaks = seq(2017, 2022, 1)) +
                 scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
                 theme_minimal()),
       width = 10, height = 10, units = "cm", dpi = 200,
       bg = "white")




## Abb. 2: Boxplot von agbd aller Reviere

Wendehals_Reviere_agbd <- data.frame(
  agbd = c(Wendehals_2017_agbd$mean_agbd2017,
           Wendehals_2018_agbd$mean_agbd2018,
           Wendehals_2019_agbd$mean_agbd2019,
           Wendehals_2020_agbd$mean_agbd2020,
           Wendehals_2021_agbd$mean_agbd2021,
           Wendehals_2022_agbd$mean_agbd2022))

Wendehals_Reviere_Jahr_agbd <- data.frame(
  Jahr = c(Wendehals_2017_agbd$Jahr,
           Wendehals_2018_agbd$Jahr,
           Wendehals_2019_agbd$Jahr,
           Wendehals_2020_agbd$Jahr,
           Wendehals_2021_agbd$Jahr,
           Wendehals_2022_agbd$Jahr),
  agbd = c(Wendehals_2017_agbd$mean_agbd2017,
           Wendehals_2018_agbd$mean_agbd2018,
           Wendehals_2019_agbd$mean_agbd2019,
           Wendehals_2020_agbd$mean_agbd2020,
           Wendehals_2021_agbd$mean_agbd2021,
           Wendehals_2022_agbd$mean_agbd2022))


ggsave(filename = "Plots/Abb2_Reviere_agbd.png",
       plot = (ggplot(data = Wendehals_Reviere_agbd, aes(x="", y=agbd))+
                 geom_boxplot(fill="grey", alpha = 0.6, width = 0.5)+
                 xlab("Wryneck territories") + ylab("mean AGBD [t/ha]")+
                 scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
                 theme_minimal()),
       width = 5, height = 7, units = "cm", dpi = 200,
       bg = "white")

ggsave(filename = "Plots/Abb2_Reviere_Jahr_agbd.png",
       plot = (ggplot(data = Wendehals_Reviere_Jahr_agbd,
                      aes(group=Jahr, x=Jahr, y=agbd))+
                 geom_boxplot(fill="grey", alpha = 0.6, width = 0.5)+
                 xlab("year") + ylab("mean AGBD [t/ha]")+
                 scale_x_continuous(breaks = seq(2017, 2022, 1)) +
                 scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
                 theme_minimal()),
       width = 10, height = 7, units = "cm", dpi = 200,
       bg = "white")


ggsave(filename = "Plots/Abb2_Reviere_agbd_violin.png",
       plot = (ggplot(data = Wendehals_Reviere_agbd, aes(x="", y=agbd))+
                 geom_violin(fill="grey", alpha = 0.6, width = 0.5)+
                 geom_point(position = position_jitter(seed = 1, width = 0.2))+
                 xlab("Wryneck territories") + ylab("mean AGBD [t/ha]")+
                 scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
                 theme_minimal()),
       width = 10, height = 7, units = "cm", dpi = 200,
       bg = "white")

ggsave(filename = "Plots/Abb2_Reviere_Jahr_agbd_violin.png",
       plot = (ggplot(data = Wendehals_Reviere_Jahr_agbd,
                      aes(group=Jahr, x=Jahr, y=agbd))+
                 geom_violin(fill="grey", alpha = 0.6, width = 0.5)+
                 geom_point(position = position_jitter(seed = 1, width = 0.2))+
                 xlab("year") + ylab("mean AGBD [t/ha]")+
                 scale_x_continuous(breaks = seq(2017, 2022, 1)) +
                 scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
                 theme_minimal()),
       width = 15, height = 7, units = "cm", dpi = 200,
       bg = "white")



## Abb. 3: agbd der Reviere aus 2022 + Vergleich agbd ab 2017

#Wendehals 2022 Jahre 
agbd_Reviere2022_Entwicklung <- data.frame(
  Jahr = c(rep(2017:2022, each=28)),
  agbd = c(Wendehals_2022_agbd$mean_agbd2017,
           Wendehals_2022_agbd$mean_agbd2018,
           Wendehals_2022_agbd$mean_agbd2019,
           Wendehals_2022_agbd$mean_agbd2020,
           Wendehals_2022_agbd$mean_agbd2021,
           Wendehals_2022_agbd$mean_agbd2022))


ggsave(filename = "Plots/Abb3_Reviere2022_Entwicklung.png",
       plot = (ggplot(data = agbd_Reviere2022_Entwicklung,
                      aes(group=Jahr, x=Jahr, y=agbd))+
                 geom_boxplot(fill="grey", alpha = 0.6, width = 0.5)+
                 xlab("year") + ylab("mean AGBD [t/ha]")+
                 scale_x_continuous(breaks = seq(2017, 2022, 1)) +
                 scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
                 theme_minimal()),
       width = 10, height = 8, units = "cm", dpi = 200,
       bg = "white")

ggsave(filename = "Plots/Abb3_Reviere2022_Entwicklung_violin.png",
       plot = (ggplot(data = agbd_Reviere2022_Entwicklung,
                      aes(group=Jahr, x=Jahr, y=agbd))+
                 geom_violin(fill="grey", alpha = 0.6, width = 0.5)+
                 geom_point(position = position_jitter(seed = 1, width = 0.2))+
                 xlab("year") + ylab("mean AGBD [t/ha]")+
                 scale_x_continuous(breaks = seq(2017, 2022, 1)) +
                 scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
                 theme_minimal()),
       width = 10, height = 8, units = "cm", dpi = 200,
       bg = "white")




#####################################################################
#####################################################################


#################################
# Abbildungen und Analysen für Reviere mit >70% Wald in 2017
#################################

agbd_2017vsBrutjahr_graphic <- data.frame(
  Jahr = c(rep(c("2017","breeding year"), each=22)),
  agbd = c(Wendehals_agbd_2017vsBrut$mean_agbd2017,
            Wendehals_agbd_2017vsBrut$agbd_Brut))

agbd_2017vsBrutjahr_graphic <- mutate(agbd_2017vsBrutjahr_graphic,
                                      Jahr = case_when(
                                        Jahr == "2017" ~ "2017 (reference)",
                                        Jahr == "breeding year" ~ "breeding year"))


ggsave(filename = "Plots/agbd_2017vsBrutjahr_violin.png",
       plot = (ggplot(data = agbd_2017vsBrutjahr_graphic,
                      aes(x="", y=agbd)) +
                 geom_violin(fill="grey", alpha = 0.6, width = 0.5) +
                 geom_point(position = position_jitter(seed = 1, width = 0.2)) +
                 facet_grid(~ Jahr) +
                 xlab(NULL) + ylab("mean AGBD [t/ha]") +
                 scale_y_continuous(breaks = seq(60, 300, 20)) +
                 theme_minimal()),
       width = 10, height = 7, units = "cm", dpi = 200,
       bg = "white")

ggsave(filename = "Plots/agbd_2017vsBrutjahr_boxplot.png",
       plot = (ggplot(data = agbd_2017vsBrutjahr_graphic,
                      aes(x="", y=agbd)) +
                 geom_boxplot(fill="grey", alpha = 0.6, width = 0.5) +
                 geom_point(position = position_jitter(seed = 1, width = 0.2)) +
                 facet_grid(~ Jahr) +
                 xlab(NULL) + ylab("mean AGBD [t/ha]") +
                 scale_y_continuous(breaks = seq(60, 300, 20)) +
                 theme_minimal()),
       width = 10, height = 7, units = "cm", dpi = 200,
       bg = "white")





Wendehals_Waldzustand_agbd <- data.frame(
  Jahr = 2017:2022,
  Reviere = c(length(which(Wendehals_agbd$Jahr == '2017')),
              length(which(Wendehals_agbd$Jahr == '2018')),
              length(which(Wendehals_agbd$Jahr == '2019')),
              length(which(Wendehals_agbd$Jahr == '2020')),
              length(which(Wendehals_agbd$Jahr == '2021')),
              length(which(Wendehals_agbd$Jahr == '2022'))),
  mean_agbd = c(mean(Wendehals_2017_agbd$mean_agbd2017),
                mean(Wendehals_2018_agbd$mean_agbd2018),
                mean(Wendehals_2019_agbd$mean_agbd2019),
                mean(Wendehals_2020_agbd$mean_agbd2020),
                mean(Wendehals_2021_agbd$mean_agbd2021),
                mean(Wendehals_2022_agbd$mean_agbd2022)))

ggsave(filename = "Plots/Abb1_Reviere_Waldzustand_agbd.png",
       plot = (ggplot() +
                 geom_bar(data = Wendehals_2022_Waldzustand_agbd,
                          mapping = aes(x = Jahr, y = Reviere),
                          stat = "identity",
                          fill = "green2", alpha = 0.4, width = 0.9) +
                 geom_line(data = Wendehals_2022_Waldzustand_agbd,
                           mapping = aes(x = Jahr, y = mean_agbd),
                           col = "green4", size = 1) +
                 xlab("year") + ylab("breeding pairs | AGBD [t/ha]") +
                 scale_x_continuous(breaks = seq(2017, 2022, 1)) +
                 scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
                 theme_minimal()),
       width = 10, height = 8, units = "cm", dpi = 200,
       bg = "white")





#################################
########## wilcox-test ##########

# testen, ob es einen Unterschied zwischen der agbd aus 2017 und dem Brutjahr gibt
shapiro.test(Wendehals_agbd_2017vsBrut$mean_agbd2017) #p = 0.3032 --> normalverteilt
shapiro.test(Wendehals_agbd_2017vsBrut$agbd_Brut) #p = 0.0008286 --> nicht normalverteilt

wilcox.test(Wendehals_agbd_2017vsBrut$mean_agbd2017,
            Wendehals_agbd_2017vsBrut$agbd_Brut)
# Ergebnis: p = 7.881e-05 --> signifikanter Unterschied

mean(Wendehals_agbd_2017vsBrut$mean_agbd2017) #150.6067
mean(Wendehals_agbd_2017vsBrut$agbd_Brut) #110.048
median(Wendehals_agbd_2017vsBrut$mean_agbd2017) #145.1673
median(Wendehals_agbd_2017vsBrut$agbd_Brut) #102.1398



