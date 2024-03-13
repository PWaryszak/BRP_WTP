#LOAD PACKAGES:====
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(grid)) install.packages('grid')
if (!require(readxl)) install.packages('readxl')

library(tidyverse)
library(readxl)
library(grid)
library(gridExtra)
library(ggpmisc)

#LOAD and CLEAN DATA:=============
ebullition <- read_excel("Ebullition_GasRetreival_Hobos.xlsx", sheet = "ALL_DATA_Pawel")
ebullition$TotalGas_mL <- as.numeric(as.character(ebullition$TotalGas_mL))
ebullition$N2O_ppm  <- as.numeric(as.character(ebullition$N2O_ppm ))
ebullition$CH4_ppm  <- as.numeric(as.character(ebullition$CH4_ppm ))
ebullition$CO2_ppm  <- as.numeric(as.character(ebullition$CO2_ppm ))
ebullition$BoardPosition<- factor(ebullition$BoardPosition, levels = c( "South","Middle","North"))
ebullition$FunnelSurface_m2 <- 452.4 /10000 #converting 452.4 cm2 to m2
ebullition$FunnelThickness_m <-  0.24
ebullition$TotalGas_m3  <- ebullition$TotalGas_mL * 0.000001


#Convert PPM to FLux=======
ppm2flux <- ebullition %>% #  filter( CH4_ppm == 146314.00000)
  
  select( N2O_ppm,CH4_ppm,CO2_ppm,BoardPosition,Season,DaysDeployed,TotalGas_m3,FunnelSurface_m2,Month, GasVialLabel) %>%
  
  gather(- Season,-BoardPosition,-DaysDeployed,-TotalGas_m3,-FunnelSurface_m2, -Month,-GasVialLabel, key = "GasType", value = "PPM" ) %>%
 
  na.omit() %>% #Removes all N/A rows

  separate(GasType, into = c("Gas", "Unit"), remove = F) %>%
  
  mutate(Slope = PPM/DaysDeployed) %>% #Flux was reported in ppm times 1440 to convert days to minutes
  
  mutate(ConversionFactor = ifelse(Gas == "N2O", 1798.56, ifelse(Gas == "CO2", 1798.45, 655.47 ))) %>%

  mutate(Flux_mgm2day = (Slope * ConversionFactor * TotalGas_m3 ) / (FunnelSurface_m2 * 1000))


View(ppm2flux)

#Get summary of flux values in mgm2day:============
ppm2flux_sum2 <- ppm2flux %>%
  group_by(Season,Month,Gas) %>%
    summarise( AV =  mean (Flux_mgm2day, na.rm=T), #/1000 to get grams instead of mg
               N = n(),
               SD = sd (Flux_mgm2day, na.rm=T),
      SE = SD / sqrt(N)) 

ppm2flux_sum2
ppm2flux_sum2$Season <- factor(ppm2flux_sum2$Season, levels = c("Summer","Autumn","Winter"))


#PLOT FLUX:===========
options(scipen = 999) #To display huge numbers 

ggplot(data = ppm2flux_sum2, aes(y= AV, x=Gas,fill=Gas)) +
   labs(y = bquote('Gas flux ' (mg*~m^-2*~day^-1)), x="") +
   geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin = AV-SE, ymax = AV+SE), position = position_dodge(), width = 0.25)+
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


ggsave(filename = "Barplot_CH4_EBULLITION_mgm2day_GOOD.jpg", height = 7,width = 10)

