#LOAD PACKAGES:====
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(grid)) install.packages('grid')
if (!require(lubridate)) install.packages('lubridate')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(corrplot)) install.packages('corrplot')
if (!require(Hmisc)) install.packages('Hmisc')


library(ggpmisc)
library(tidyverse)
library(gridExtra)
library(grid)
library(lubridate)
library(corrplot)
library(Hmisc)


#COMPUTE CO2 EMISSION RATE OFF UGGA txt FILE =====
#Subset only pre-defined start and stop time (off our field notes).
ugga_data <- read.delim("gga_2019-08-06_f0003.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%m/%d/%Y %H:%M:%S")))


ugga_co2_raw <- ugga_data %>% 
  mutate(Site = ifelse(Datetime >='2019-08-06 17:14:52' & Datetime <='2019-08-06 17:21:30',"WTP_FreshPond_High_A2_Winter", "BLANK")) %>%  #Select time based on Field Data sheet!
  filter (Site != "BLANK") %>% #remove all records in between chambers.  I called them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000)) #Gas flux in micro-grams per m2 per Day (ug)

unique(ugga_co2_raw $Gas_Flux) #4947.442 mg per m2 per day

#Draw the plot for the site to visually assess how best fit the slope (Consult Field Data!):
ggplot(ugga_co2_raw[ugga_co2_raw$Site=="WTP_FreshPond_High_A2_Winter",] , aes(x = NumTime, y = X.CO2._ppm)) + 
  geom_point() +
    ggtitle("Freshpond High A1 Winter (2019-08-06)")+
   stat_smooth(method = "lm", col = "red")


#Filter out fit data by modifying NumTime based on the plot above:
ugga_co2_fit <- ugga_co2_raw %>% 
  filter ( NumTime >= 231 & NumTime <= 531) %>% #visually assess the cut-off point for the best fit of the regression.
  group_by(Site)%>% 
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000)) #Gas flux in micro-grams per m2 per Day (ug)

unique(ugga_co2_fit$Gas_Flux) #3688.905 mg per m2 per day
unique(ugga_co2_fit$Slope) # 0.04748051
summary(lm (X.CO2._ppm ~ NumTime, data = ugga_co2_fit))



#Draw the plot:
ggplot(ugga_co2_fit [ugga_co2_fit $Site=="WTP_FreshPond_High_A2_Winter" &  ugga_co2_fit $NumTime>100,] , aes(x = NumTime, y = X.CO2._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ ggtitle("Freshpond High A1 Winter")+
  stat_fit_glance(method = "lm",
                  label.x = c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)



#COMPUTE CH4 EMISSION RATE OFF UGGA txt FILE =====
#Subset only pre-defined start and stop time (off our field notes).
ugga_data <- read.delim("gga_2019-08-06_f0003.txt",sep = "," , skip =1) %>% #Read in gas flux data
    filter (X.CH4._ppm != "NA")%>%
    mutate(Datetime = as.POSIXct(strptime(Time,"%m/%d/%Y %H:%M:%S")))

ugga_CH4_raw <- ugga_data %>% 
  mutate(Site = ifelse(Datetime >='2019-08-06 17:14:52' & Datetime <='2019-08-06 17:21:30',"WTP_FreshPond_High_A2_Winter", "BLANK")) %>% 
                       
  filter (Site != "BLANK") %>% #remove all records in between chambers.  I called them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CH4._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000)) #Gas flux in micro-grams per m2 per Day (ug)

unique(ugga_CH4_raw $Gas_Flux) #20464.36 mg per m2 per day

#Draw the plot for the site to visually assess how best fit the slope:
ggplot(ugga_CH4_raw[ugga_CH4_raw$Site=="WTP_FreshPond_High_A2_Winter",] , aes(x = NumTime, y = X.CH4._ppm)) + 
  geom_point() +
  ggtitle("Freshpond High A1 Winter (2019-08-06)")+
  stat_smooth(method = "lm", col = "red")


ugga_CH4_fit <- ugga_CH4_raw %>% 
  filter ( NumTime >=197 & NumTime <= 497) %>% #visually assess the cut-off point for the best fit of the regression.
  group_by(Site)%>%
  mutate (Slope = coef(summary(lm (X.CH4._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000)) #Gas flux in micro-grams per m2 per Day (ug)

unique(ugga_CH4_fit$Gas_Flux) #-2986.407 mg per m2 per day
unique(ugga_CH4_fit$Slope) # -0.03843854
summary(lm (X.CH4._ppm ~ NumTime, data = ugga_CH4_fit))



#Draw the plot for WTP_FreshPond_High_A2_Winter:
ggplot(ugga_CH4_fit [ugga_CH4_fit $Site=="WTP_FreshPond_High_A2_Winter" &  ugga_CH4_fit $NumTime>100,] , aes(x = NumTime, y = X.CH4._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ 
  ggtitle("Freshpond High A1 Winter")+
  stat_fit_glance(method = "lm",
                  label.x = c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)




#Based on the Field data sheet it should be:
ggplot(ugga_CH4_raw [ugga_CH4_raw$Site=="WTP_FreshPond_High_A2_Winter" &  ugga_CH4_raw$NumTime>25 & ugga_CH4_raw$NumTime<90,] , aes(x = NumTime, y = X.CH4._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ 
  ggtitle("Freshpond High A1 Winter")+
  stat_fit_glance(method = "lm",
                  label.x = c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)



