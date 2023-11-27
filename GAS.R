library(tidyverse)
library(readxl)
library(lubridate)#install.packages ("lubridate") for editing time series
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(grid)
library(gridExtra)
library(nlme)
library(ggpmisc)


#DATA:
#summer 2020 Ebullition data :========
ebullition <- read_excel("Ebullition_GasRetreival_Hobos.xlsx", sheet = "ALL_DATA")
str(ebullition)
ebullition$Totalebullition_mL <- as.numeric(as.character(ebullition$Totalebullition_mL))
ebullition$N2O_ppm  <- as.numeric(as.character(ebullition$N2O_ppm ))
ebullition$CH4_ppm  <- as.numeric(as.character(ebullition$CH4_ppm ))
ebullition$CO2_ppm  <- as.numeric(as.character(ebullition$CO2_ppm ))
ebullition$BoardPosition<- factor(ebullition$BoardPosition, levels = c( "South","Middle","North"))


#ebullition CH4 Stats Table  :==========
names(ebullition)
ebullition$CH4_ppm  <- as.numeric(as.character(ebullition$CH4_ppm ))
model_ebullition<-( lmer(CH4_ppm ~ CollectionSeason + (1|BoardPosition), data=ebullition))
tab_model(model_ebullition,show.se = TRUE)
tab_model(model_ebullition,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#ebullition CO2 Stats Table  :==========
names(ebullition)
ebullition$CO2_ppm  <- as.numeric(as.character(ebullition$CO2_ppm ))
model_ebullition<-( lmer(CO2_ppm ~ CollectionSeason + (1|BoardPosition), data=ebullition))
tab_model(model_ebullition,show.se = TRUE)
tab_model(model_ebullition,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


#UGGA Stats Table  :==========
ugga <- read_excel("Master_BRP_GHG_Flux_Data.xlsx", sheet = "WTP_GHG_Winter")
names(ugga)#"Site_ID"            "Date"               "Season"             "CO2_Flux_mgm2day"   "CO2_Flux_mmolm2day" "R2_CO2"    "CH4_Flux_mgm2day" 
# [8] "CH4_Flux_mmolm2day" "R2_CH4"             "Site"               "Rehab"              "Position"           "Subplot"            "Rehab_FreshPond"   

ugga$CO2_Flux_mgm2day  <- as.numeric(as.character(ugga$CO2_Flux_mgm2day ))
model_ugga<-( lmer(CO2_Flux_mgm2day ~ Rehab_FreshPond + (1|Subplot), data=ugga))
tab_model(model_ugga,show.se = TRUE)
tab_model(model_ugga,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


#ebullition Total Plot=======
#options(scipen=1000000) #to stop axis labels being abbreviated: web = https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2


plot_ebullition <- ggplot(data = ebullition, aes(x= BoardPosition, y=Totalgas_mL/DaysDeployed,color = BoardPosition)) +
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
