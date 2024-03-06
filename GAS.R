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

#GETTING SUBSCRIPT IN GGPLOT 
#WEB: https://www.itcodar.com/r/subscript-letters-in-ggplot-axis-label.html


#DATA:
#summer 2020 Ebullition data :========
ebullition <- read_excel("Ebullition_GasRetreival_Hobos.xlsx", sheet = "ALL_DATA_Pawel")
ebullition$TotalGas_mL <- as.numeric(as.character(ebullition$TotalGas_mL))
ebullition$N2O_ppm  <- as.numeric(as.character(ebullition$N2O_ppm ))
ebullition$CH4_ppm  <- as.numeric(as.character(ebullition$CH4_ppm ))
ebullition$CO2_ppm  <- as.numeric(as.character(ebullition$CO2_ppm ))
ebullition$BoardPosition<- factor(ebullition$BoardPosition, levels = c( "South","Middle","North"))
ebullition$FunnelSurface_m2 <- 452.4 /10000 #converting 452.4 cm2 to m2
ebullition$FunnelThickness_m <-  0.24
ebullition$TotalGas_m3  <- ebullition$TotalGas_mL * 0.000001


#PPM FLux=======
e <- select(ebullition, N2O_ppm,CH4_ppm,CO2_ppm,BoardPosition,Season,DaysDeployed,TotalGas_m3,FunnelSurface_m2,Month)

#Summarise for plotting:
e_short_old <- e %>% gather(- Season,-BoardPosition,-DaysDeployed,-TotalGas_m3, key = "GasType", value = "PPM" ) %>%
  na.omit() %>%

  separate(GasType, into = c("Gas", "Unit"), remove = F) %>%
  mutate(MolecularWeight_gmol = ifelse(Gas == "N2O", 44.013, ifelse(Gas == "CO2",44.009 , 16.043 ))) %>%
  mutate(FunnelSurface_m2 = 452.4 /10000) %>%  #converting 452.4 cm2 to m2

  mutate(Flux_ppm = PPM/DaysDeployed) %>%
  mutate(Flux_mgm3day = PPM * MolecularWeight_gmol/ 24.45 / DaysDeployed) %>%  #Y mg/m3 = (X ppm)(molecular weight)/24.45
  mutate(Flux_mgm2day = Flux_mgm3day * TotalGas_m3 / FunnelSurface_m2 )
  
e_short <- e %>% gather(- Season,-BoardPosition,-DaysDeployed,-TotalGas_m3,-FunnelSurface_m2, -Month, key = "GasType", value = "PPM" ) %>%
  na.omit() %>%

  separate(GasType, into = c("Gas", "Unit"), remove = F) %>%
  
  
  mutate(Slope = PPM/DaysDeployed) %>% #Flux was reported in ppm
  
  mutate(ConversionFactor = ifelse(Gas == "N2O", 1798.56, ifelse(Gas == "CO2", 1798.45, 655.47 ))) %>%

  mutate(Flux_mgm2day = Slope* ConversionFactor*TotalGas_m3 / FunnelSurface_m2 *1000 )



#Get summary of flux values in ppm:============
e_short_sum1 <- e_short %>%
  group_by(Season,Month,Gas) %>%
    summarise( AV =  mean (Flux, na.rm=T),
               N = n(),
               SD = sd (Flux, na.rm=T),
      SE = SD / sqrt(N)) 

e_short_sum1
e_short_sum1$Season <- factor(e_short_sum1$Season, levels = c("Summer","Autumn","Winter"))

Barplot_ppm <- ggplot(data = e_short_sum1, aes(y= AV, x=Gas,fill=Gas)) +
  #labs(x = "Gas type", y="Gas flux (ppm/day)") +
   labs(y = bquote('Gas flux ' (ppm*~day^-1)), x="") +
   geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin = AV-SE, ymax = AV+SE), position = dodge, width = 0.25)+
  ggtitle("Greenhouse gas fluxes at WTP (PPM a DAY)")+
  facet_grid(.~Season)+
  theme_minimal()+
  theme(axis.text.x=element_text(vjust=0.5,size=13, face="bold", colour = "black"),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
    plot.title = element_text(hjust=0.5, size =18),
        strip.text=element_text(size=18))

Barplot_ppm
ggsave(Barplot_ppm,filename = "Barplot_Ebullition_ppm_WTP.jpg", height = 7,width = 11)




#Get summary of flux values in mgm2day:============
e_short_sum2 <- e_short %>%
  group_by(Season,Month,Gas) %>%
    summarise( AV =  mean (Flux_mgm2day/1000, na.rm=T), #/1000 to get grams instead of mg
               N = n(),
               SD = sd (Flux_mgm2day/1000, na.rm=T),
      SE = SD / sqrt(N)) 

e_short_sum2
e_short_sum2$Season <- factor(e_short_sum2$Season, levels = c("Summer","Autumn","Winter"))

Barplot_mgm2day <- ggplot(data = e_short_sum2, aes(y= AV, x=Gas,fill=Gas)) +
  #labs(x = "Gas type", y="Gas flux ") +
   labs(y = bquote('Gas flux ' (g*~m^-2*~day^-1)), x="") +
   geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin = AV-SE, ymax = AV+SE), position = dodge, width = 0.25)+
  ggtitle("Ebullitive fluxes in freshwater pond")+
  facet_grid(.~Season+Month)+
  theme_minimal()+
  theme(axis.text.x=element_text(vjust=0.5,size=13, face="bold", colour = "black"),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
       plot.title = element_text(hjust=0.5, size = 22, face="bold"),
        strip.text=element_text(size=18))

Barplot_mgm2day
ggsave(Barplot_mgm2day,filename = "Barplot_eBULLTION_mgm2day_POND.jpg", height = 7,width = 11)

#Ebullitive FLUX (CONVERT PPM to mgm2day)========
#Convert PPM to mg/m3: The formula for the conversion: concentration (mg/m3) = 0.0409 x concentration (ppm) x molecular weight
ebullition$N2O_mgm3 <- 0.0409 *  44.013 * ebullition$N2O_ppm / ebullition$TotalGas_mL * 0.000001 #mL to m3, molecular weight of N2O =  44.013 g / mol
ebullition$CH4_mgm3 <- 0.0409 *  16.043 * ebullition$CH4_ppm / ebullition$TotalGas_mL * 0.000001 #mL to m3, molecular weight of CH4 = 16.043  g / mol
ebullition$CO2_mgm3 <- 0.0409 *  44.009 * ebullition$CO2_ppm / ebullition$TotalGas_mL * 0.000001 #mL to m3, molecular weight of CO2 = 44.009 g /mol

#Convert to mg per m2 per Day:
ebullition$N2O_mgm2day <- ebullition$N2O_mgm3 / ebullition$FunnelThickness_m / ebullition$DaysDeployed
ebullition$CH4_mgm2day <- ebullition$CH4_mgm3 / ebullition$FunnelThickness_m / ebullition$DaysDeployed
ebullition$CO2_mgm2day <- ebullition$CO2_mgm3 / ebullition$FunnelThickness_m / ebullition$DaysDeployed


#Board position effect on GHG on fluxes:============
CO2_model <- lm(CO2_mgm2day~BoardPosition, data = ebullition)
CH4_model <- lm(CH4_mgm2dayBoardPosition, data =  ebullition)
N2O_model <- lm(N2O_mgm2day~BoardPosition, data =ebullition)

tab_model(CO2_model,CH4_model,N2O_model ,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


CO2_model <- lm(Flux_mgm2day~BoardPosition, data = e_short[e_short$Gas=="CO2",])
CH4_model <- lm(Flux_mgm2day~BoardPosition, data = e_short[e_short$Gas=="CH4",])
N2O_model <- lm(Flux_mgm2day~BoardPosition, data = e_short[e_short$Gas=="N2O",])

tab_model(CO2_model,CH4_model,N2O_model ,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


e <- select(ebullition, N2O_mgm2day,CH4_mgm2day,CO2_mgm2day,BoardPosition, CollectionSeason)
e_short <- e %>% gather(- CollectionSeason,-BoardPosition, key = "GasType", value = "Flux" ) %>%
  na.omit() %>%
  separate(GasType, into = c("Gas", "Unit"), remove = F)

plot_ebullition_mgm2day <- ggplot(data = e_short, aes(y= Gas, x=Flux, col = BoardPosition)) +
  labs(y = "Gas type", x="Gas flux (mg/m2/day)") +
  geom_boxplot()+ # geom_jitter()+
  ggtitle("Greenhouse gas fluxes at WTP")+
  coord_flip()+
  facet_grid(BoardPosition~CollectionSeason)+
  theme_minimal()+
  theme(axis.text.x=element_text(vjust=0.5,size=13, face="bold", colour = "black"),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "top",
        strip.background = element_rect(fill="white"),
    plot.title = element_text(hjust=0.5, size =18),
        strip.text=element_text(size=18))

plot_ebullition_mgm2day
ggsave(plot_ebullition_mgm2day,filename = "plot_ebullition_mgm2day_WTP.jpg", height = 7,width = 10)


#Get summary of flux values:
e_short_sum <- e_short %>%
  group_by(CollectionSeason,Gas) %>%
    summarise( AV =  mean (Flux, na.rm=T),
               N = n(),
               SD = sd (Flux, na.rm=T),
      SE = SD / sqrt(N)) 

e_short_sum


Barplot_ebullition_mgm2day <- ggplot(data = e_short_sum, aes(y= AV, x=Gas,fill=Gas)) +
  #labs(x = "Gas type", y="Gas flux (mg/m2/day)") +
   labs(y = bquote('Gas flux ' (mg*~m^-2~day^-1)), x="") +
   geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin = AV-SE, ymax = AV+SE), position = dodge, width = 0.25)+
  ggtitle("Greenhouse gas fluxes at WTP")+
  facet_grid(.~CollectionSeason)+
  theme_minimal()+
  theme(axis.text.x=element_text(vjust=0.5,size=13, face="bold", colour = "black"),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
    plot.title = element_text(hjust=0.5, size =18),
        strip.text=element_text(size=18))

Barplot_ebullition_mgm2day
ggsave(Barplot_ebullition_mgm2day,filename = "Barplot_ebullition_mgm2day_WTP.jpg", height = 7,width = 10)



#Ebullition Total BoxPlot (mL/day)=======
#options(scipen=1000000) #to stop axis labels being abbreviated: web = https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2

plot_ebullition <- ggplot(data = ebullition, aes(x= BoardPosition, y=TotalGas_mL/DaysDeployed,color = BoardPosition)) +
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

plot_ebullition



#ebullition CH4 PPM Stats Table  :==========
names(ebullition)
ebullition$CH4_ppm  <- as.numeric(as.character(ebullition$CH4_ppm ))
model_ebullition<-( lmer(CH4_ppm ~ CollectionSeason + (1|BoardPosition), data=ebullition))
tab_model(model_ebullition,show.se = TRUE)
tab_model(model_ebullition,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#ebullition CO2 PPM Stats Table  :==========
names(ebullition)
ebullition$CO2_ppm  <- as.numeric(as.character(ebullition$CO2_ppm ))
model_ebullition<-( lmer(CO2_ppm ~ CollectionSeason + (1|BoardPosition), data=ebullition))
tab_model(model_ebullition,show.se = TRUE)
tab_model(model_ebullition,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table



#UGGA diffusive flux============
ugga <- read_excel("Master_BRP_GHG_Flux_Data.xlsx", sheet = "WTP_GHG_Winter")
names(ugga)#"Site_ID","Date" ,"Season" , "CO2_Flux_mgm2day" ,"CO2_Flux_mmolm2day" "R2_CO2" , "CH4_Flux_mgm2day" 
# [8] "CH4_Flux_mmolm2day" "R2_CH4" ,"Site","Rehab","Position" , "Subplot",Rehab_FreshPond"   
ugga$CO2_Flux_mgm2day  <- as.numeric(as.character(ugga$CO2_Flux_mgm2day ))
ugga$CH4_Flux_mgm2day  <- as.numeric(as.character(ugga$CH4_Flux_mgm2day ))


#Get summary of flux values:
ugga_sum <- ugga %>%
  group_by(Rehab,Season) %>%
    summarise( AV =  mean (CH4_Flux_mgm2day, na.rm=T),
               N = n(),
               SD = sd (CH4_Flux_mgm2day, na.rm=T),
      SE = SD / sqrt(N)) 

ugga_sum

#CO2 & CH$ one way:
Barplot_UGGA_mgm2day <- ggplot(data = ugga_sum, aes(y= AV, x=Rehab,fill=Rehab)) +
  #labs(x = "Rehab type", y="Rehab flux (mg/m2/day)") +
   labs(y = bquote('Gas flux ' (mg*~m^-2~day^-1)), x="") +
   geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin = AV-SE, ymax = AV+SE), position = dodge, width = 0.25)+
  ggtitle("CH4 diffusive fluxes at WTP (UGGA)")+
  facet_grid(.~Season)+
  theme_minimal()+
  theme(axis.text.x=element_text(vjust=0.5,size=13, face="bold", colour = "black"),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=14,colour = "black",face = "bold",),
        axis.title.x=element_text(size=14, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
    plot.title = element_text(hjust=0.5, size =18),
        strip.text=element_text(size=18))

Barplot_UGGA_mgm2day
ggsave(Barplot_ebullition_mgm2day,filename = "Barplot_CH4_UGGA_mgm2day_WTP.jpg", height = 7,width = 10)


#UGGA Stats Table  :==========
ugga <- read_excel("Master_BRP_GHG_Flux_Data.xlsx", sheet = "WTP_GHG_Winter")
names(ugga)#"Site_ID"            "Date"               "Season"             "CO2_Flux_mgm2day"   "CO2_Flux_mmolm2day" "R2_CO2"    "CH4_Flux_mgm2day" 
# [8] "CH4_Flux_mmolm2day" "R2_CH4"             "Site"               "Rehab"              "Position"           "Subplot"            "Rehab_FreshPond"   

ugga$CO2_Flux_mgm2day  <- as.numeric(as.character(ugga$CO2_Flux_mgm2day ))
model_ugga<-( lmer(CO2_Flux_mgm2day ~ Rehab_FreshPond + (1|Subplot), data=ugga))
tab_model(model_ugga,show.se = TRUE)
tab_model(model_ugga,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


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
