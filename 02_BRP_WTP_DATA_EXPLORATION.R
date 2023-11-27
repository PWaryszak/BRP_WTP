#LOAD libraries========
library("tidyverse")#install.packages() first if not in your local library
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(grid)
library(gridExtra)
library(nlme)
library(ggpmisc)



#Compute % of shell-rich samples (HCl bubbly test):=======
wtp <- read_excel("BRP_CoreSlicingData.xlsx", sheet = "WTP")
bubbles <- length(wtp$HCl_Bubbles [wtp$HCl_Bubbles=="y"])
no_bubbles <- length(wtp$HCl_Bubbles [wtp$HCl_Bubbles=="n"])

#Compute percentage of samples with inorganic CaCO3:
(bubbles / (bubbles + no_bubbles)) *100 #63 % of samples contained CaCO3.
        
        
#LOAD and PROCESS DATA:========
NewDATA <- read.csv("CN_WTP_MASTER_DATA.csv")#
NewDATA <- NewDATA [ !is.na(NewDATA$C.percent),] #remove NA-s
NewDATA$C.percent <- ifelse(NewDATA$C.percent == 0, 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models
NewDATA$SliceLength.cm <- (NewDATA$DepthTo_cm - NewDATA$DepthFrom_cm) #round % to full numbers to run Poisson
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter_cm/2)^2)*NewDATA$SliceLength.cm  #slice volume

#Compute Compaction Correction Value
NewDATA$Core_in.mm <- (NewDATA$PipeLength_cm *10) - NewDATA$Cmptn_in_mm 
NewDATA$Pipe_in.mm <- (NewDATA$PipeLength_cm *10) - NewDATA$Cmptn_out_mm 
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.mm/NewDATA$Pipe_in.mm

#and multiply bulk density by Compaction Correction Value:
NewDATA$dry_bulk_density.gcm3           <- NewDATA$DryWeight_g                     / NewDATA$SampleVolume.cm3
NewDATA$dry_bulk_density_corrected.gcm3 <- NewDATA$dry_bulk_density.gcm3           * NewDATA$Compaction_Correction_Value

#CORRECTED C-stock to account for compaction:
NewDATA$CarbonDensity.gcm3              <- NewDATA$dry_bulk_density_corrected.gcm3 * NewDATA$C.percent/100
NewDATA$CarbonStock.Mgha <- (((NewDATA$CarbonDensity.gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength.cm )
dim(NewDATA)#179  47

#CN ANALYSIS:=============
levels(NewDATA$TimeSinceRehab)
NewDATA$TimeSinceRehab <- factor(NewDATA$TimeSinceRehab, levels = c("FreshPond","3yrRehab","9yrRehab" ))
model_cn<-( lmer(CarbonStock.Mgha ~ TimeSinceRehab + (1|location), data=NewDATA))
tab_model(model_cn,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table



#PLOT DATA======
#Plot by by habitat
#Plot %C by by TimeSinceRehab and DepthRange.cm============
#We agreed to look at the top 5 cm only:
top5cm <- filter(NewDATA, DepthTo_cm <= 5)

a <- ggplot(top5cm, aes(x = reorder(DepthRange_cm, desc(DepthRange_cm)),
                        y = C.percent, color= location)) +
  geom_point(size=2.5) +
  facet_grid(.~ TimeSinceRehab)+ geom_jitter()+
  ylab("Carbon (%)") + xlab("") +
  theme_bw() +
  coord_flip()+
  theme_bw() + theme(axis.text.x = element_text(size=10),
                     axis.text.y = element_text(size = 16),
                     axis.title.y = element_text(size = 16),
                     legend.position = "bottom",
                     legend.text = element_text(size = 16),
                     strip.text=element_text(size=16),
                     plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
                     strip.background =  element_rect(fill = "white"))
a

NewDATA3 <- top5cm  %>%
  group_by(TimeSinceRehab,Core) %>%
  summarise(TotalCarbonStock = sum(CarbonStock.Mgha, na.rm = T)) %>%
  separate(Core, into = c("site2", "location", "AB"), remove = F)

#Plot Carbon stock per core (Summing up slices): 
b <- ggplot(NewDATA3, aes(x = location, y = TotalCarbonStock*1000,color=location)) +
  geom_boxplot(outlier.shape = NA ) +
  facet_grid(.~ TimeSinceRehab)+ geom_jitter() +
  labs(y=bquote('Carbon stock  ' (kg*~ha^-1)),
       x="Cores", color ="")+
  theme_bw() +
  ggtitle("WTP (top 5 cm)")+
  theme(axis.text.x = element_text(size = 10,face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 18, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
b
grid.arrange(b,a)


