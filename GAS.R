library(tidyverse)
library(readxl)
library(lubridate)#install.packages ("lubridate") for editing time series
library(gridExtra)

#DATA:
#Turn wide format to long format for plotting:
#summer 2020 temperature data :========
gas <- read_excel("Ebullition_GasRetreival_Hobos.xlsx", sheet = "ALL_DATA")
str(gas)
gas$TotalGas_mL <- as.numeric(as.character(gas$TotalGas_mL))
gas$N2O_ppm  <- as.numeric(as.character(gas$N2O_ppm ))
gas$CH4_ppm  <- as.numeric(as.character(gas$CH4_ppm ))
gas$CO2_ppm  <- as.numeric(as.character(gas$CO2_ppm ))

gas$BoardPosition<- factor(gas$BoardPosition, levels = c( "South","Middle","North"))

#Gas Total Plot=======
options(scipen=1000000) #to stop axis labels being abbreviated: web = https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2


plot_gas <- ggplot(data = gas, aes(x= BoardPosition, y=TotalGas_mL/DaysDeployed,color = BoardPosition)) +
  labs(y = "Gas volume (mL/day)", x="Board") +
  geom_boxplot()+ geom_jitter()+
  ggtitle("Gas total at WTP")+
  coord_flip()+
  facet_grid(.~CollectionSeason)+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=13, face="bold", colour = "black"),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
        strip.text=element_text(size=18))
plot_gas

#CH4 Total Plot=======
plot_CO2_ppm <- ggplot(data = gas, aes(x= BoardPosition, y=CO2_ppm, color = BoardPosition)) +
  labs(y = "Gas concetration (ppm)", x="Board") +
  geom_boxplot()+ geom_jitter()+
  ggtitle("CO2 at WTP")+
  facet_grid(.~CollectionSeason)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=10, face="bold", colour = "black", angle=90),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
        strip.text=element_text(size=18))
plot_CO2_ppm

#CH4 Total Plot=======

plot_CH4_ppm <- ggplot(data = gas, aes(x= BoardPosition, y=CH4_ppm, color = BoardPosition)) +
  labs(y = "Gas concetration (ppm)", x="Board") +
  geom_boxplot()+ geom_jitter()+
  ggtitle("CH4 at WTP")+
  facet_grid(.~CollectionSeason)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=10, face="bold", colour = "black", angle=90),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
        strip.text=element_text(size=18))
plot_CH4_ppm

#N2O Total Plot=======
plot_N2O_ppm <- ggplot(data = gas, aes(x= BoardPosition, y=N2O_ppm, color = BoardPosition)) +
  labs(y = "Gas concetration (ppm)", x="Board") +
  geom_boxplot()+ geom_jitter()+
  ggtitle("N2O at WTP")+
  facet_grid(.~CollectionSeason)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=10, face="bold", colour = "black", angle=90),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
        strip.text=element_text(size=18))
plot_N2O_ppm

#PLOT TOGETHER:
grid.arrange(plot_gas, plot_CH4_ppm)
