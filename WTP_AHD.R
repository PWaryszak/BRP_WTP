#Load libraries and data
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(rayshader)) install.packages('rayshader')

library(tidyverse)
library(rayshader)

WTP <- read.csv("BRP_Elevation_WTP_Rehab_AHD_corrected.csv") #read in data
str(WTP)#check structure of your data


#SUMMARY STATS:=========
elevations <- WTP %>%
  
  mutate(rehab = ifelse(location != "Fresh", "Rehab", "FreshPond")) %>% #FeshPond is more of a control
  
  group_by(elev) %>% #,location, rehab
  
  summarise(AV = mean(output_height),
            SD= mean(output_height),
            N =n(),
            SE = SD/sqrt(N))
elevations
write.csv(elevations, file = "AV_Rehab_Elevations3.csv", row.names = F)


#BOXPLOT Western Treatment Plant (Avalon Reserve)======
unique(WTP$location) #9yr 3yr Fresh
WTP$location <- factor(WTP$location, levels = c("9yr", "3yr","Fresh"))#order levels of year so that they are showed chronologically

ggplot(WTP,aes(x=location, y=output_height, color = location)) + #output_height is the AHD (Australian Height Datum) based off Ellipsoidal Height we get from RTK
  #AHD from the website here: https://geodesyapps.ga.gov.au/ausgeoid2020-batch-processing
  geom_boxplot()+
  labs(x = "",y="WTP Elevations (m)")+
  geom_boxplot()+ geom_jitter()+
  #facet_grid(.~where,scale="fixed") +
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=24),
        axis.title.x=element_blank(),
        legend.position = "none",
        strip.text=element_text(size=24))


#LOLIPLOT Western Treatment Plant (Avalon Reserve)======
##WEB:https://www.r-graph-gallery.com/301-custom-lollipop-chart/
WTP2 <- WTP %>% arrange(output_height) %>%
  mutate(name = factor(name, levels = .$name)) #arrange names in ascending way

 ggplot(WTP2, aes(x= name, y=output_height, color = location)) +
  geom_segment( aes(x=name, xend= name, y=0, yend=output_height),size =2)+
  geom_point(size = 4) +
  theme_bw() +
   coord_flip()+
  theme(axis.text.y=element_text(),
        axis.text.x=element_text(angle = 90,size =14),
        legend.position = "top")+
   ggtitle("Western Treatment Plant elevations (BRP, Avalon)")





#3D PLOT==========
#WEB: https://www.rayshader.com/
mtplot = ggplot(WTP) + 
  geom_point(aes(x = elevation, y = location , color = output_height)) + 
  scale_color_continuous()

par(mfrow = c(1, 2))
plot_gg(mtplot, width = 3.5, raytrace = FALSE, preview = TRUE)

plot_gg(mtplot, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1200, 960),
        fov = 70, zoom = 0.6, theta = 330, phi = 40)
  
render_snapshot(clear = TRUE)

