#LOAD PACKAGES:====
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(grid)) install.packages('grid')
if (!require(readxl)) install.packages('readxl')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(svglite)) install.packages('svglite')

library(tidyverse)
library(readxl)
library(grid)
library(gridExtra)
library(ggpmisc)
library (ggpubr)
library(svglite)
library(lubridate)


Summer2020 <- read_excel("All_cleaned_temp_data_and_calibration.xlsx", sheet = "SummerCalibrated")

#Quick Plot:
  ggplot(data = Summer, aes(y= Temp_C, x=Day, fill=BottomSurface)) +
   geom_point (aes(fill=BottomSurface))+
   labs(x="Day", y = (bquote(Water~Temperature~("\u00B0"~C))))+
  ggtitle("Winter")+
  facet_grid(.~BottomSurface)


#Summer NICE PLOT:=======
names(Summer)
SummerSum <- SummerNum %>%
  mutate(BottomSurface2 = ifelse(BottomSurface =="S","Surface","Bottom"))%>%
  group_by(Day, BottomSurface2       ) %>%
      summarise( AV =  mean (Temp_C, na.rm=T), 
                N = n(),
                SD = sd (Temp_C, na.rm=T),
                SE = SD / sqrt(N))
SummerSum

SummerTempPlot <- ggplot(data = SummerSum, aes(y= AV, x = Day,group=BottomSurface2)) +
  geom_point(aes(col=Day),size=4)+
  geom_errorbar(aes(ymin = AV-SD, ymax = AV+SD))+
  
  geom_line(aes(colour = BottomSurface2), linetype = 1,size=1.2, alpha=0.4)+
    scale_y_continuous(limits = c(10,25))+


   labs(x="Day", y = (bquote(Water~Temperature~("\u00B0"~C))))+
   ggtitle("Summer")+ # (Mean+SD)
   facet_grid(.~BottomSurface2)+
  
  theme_minimal()+
  theme(axis.text.x=element_text(vjust=0.5,size=10,colour = "black", angle=90),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=18,colour = "black",face = "bold"),
        axis.title.x=element_text(size=18,  hjust = 0.5,colour = "black",face = "bold"),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
       plot.title = element_text(hjust=0.5, size = 22, face="bold"),
        strip.text=element_text(size=18))

SummerTempPlot

#Winter NICE PLOT:=======
Winter2020 <- read_excel("All_cleaned_temp_data_and_calibration.xlsx", sheet = "WinterCalibrated")
View(Winter2020)
names(Winter2020)

Winter <- gather(Winter2020, "OnBoard", "Temp_C", -DateTime) %>%
  separate(OnBoard, into = c("BoardID", "BottomSurface"), remove = F) %>%
  mutate(Season = "Winter") %>%
    mutate( Day =
      format(as.POSIXct(.$DateTime,format="%Y-%m-%d %H:%M:%S"),format='%Y/%m/%d'))

names(Winter)
WinterSum <- Winter%>%
  mutate(BottomSurface2 = ifelse(BottomSurface =="S","Surface","Bottom"))%>%
  group_by(Day, BottomSurface2) %>%
      summarise( AV =  mean (Temp_C, na.rm=T), 
                N = n(),
                SD = sd (Temp_C, na.rm=T),
                SE = SD / sqrt(N))
WinterSum

WinterTempPlot <- ggplot(data = WinterSum, aes(y= AV, x = Day,group=BottomSurface2)) +
  geom_point(aes(col=Day),size=4)+
  geom_errorbar(aes(ymin = AV-SD, ymax = AV+SD))+
  
  geom_line(aes(colour = BottomSurface2), linetype = 1,size=1.2)+
  scale_y_continuous(limits = c(10,25))+

   labs(x="Day", y = (bquote(Water~Temperature~("\u00B0"~C))))+
   ggtitle("Winter")+ # (Mean+SD)
   facet_grid(.~BottomSurface2)+
  
  theme_minimal()+
  theme(axis.text.x=element_text(vjust=0.5,size=10,colour = "black", angle=90),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=18,colour = "black",face = "bold"),
        axis.title.x=element_text(size=18,  hjust = 0.5,colour = "black",face = "bold"),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
       plot.title = element_text(hjust=0.5, size = 22, face="bold"),
        strip.text=element_text(size=18))

WinterTempPlot


#CHATGPT=========
# Convert 'Day' variable to numeric
SummerSum$Day_numeric <- as.numeric(as.character(SummerSum$Day))
SummerSum$Day_numeric <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18)
# Define the color scale based on temperature
color_scale <- scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
                                     limits = range(SummerSum$Day_numeric), breaks = pretty(range(SummerSum$Day_numeric), n = 10))

# Plotting
ggplot(data = SummerSum, aes(y = AV, x = Day_numeric)) +
  geom_point(aes(col = Day_numeric), size = 4) +
  geom_errorbar(aes(ymin = AV - SD, ymax = AV + SD)) +
  
  scale_color_date(low = "#053061", high = "#67001f")+
  
  labs(x = "Day", y = expression(paste("Water Temperature (", degree, "C)"))) +
  ggtitle("Winter (Mean+SD)") +
  facet_grid(. ~ BottomSurface2) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black", angle = 90),
        axis.text.y = element_text(size = 13, colour = "black"),
        axis.title.y = element_text(size = 18, colour = "black", face = "bold"),
        axis.title.x = element_text(size = 18, hjust = 0.5, colour = "black", face = "bold"),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
        strip.text = element_text(size = 18)) +
  
  # Apply the color scale
  color_scale


#PLOT Winter Summer Water Temp data together
Summer2020 <- read_excel("All_cleaned_temp_data_and_calibration.xlsx", sheet = "SummerCalibrated")
Summer <- gather(Summer2020, "OnBoard", "Temp_C", -DateTime) %>%
  separate(OnBoard, into = c("BoardID", "BottomSurface"), remove = F) %>%
  mutate(Season = "Summer") %>%
    mutate( Day =
      format(as.POSIXct(.$DateTime,format="%Y-%m-%d %H:%M:%S"),format='%Y/%m/%d'))


unique(Summer$Day) 
# Create a vector of dates
dates <- c("2020/02/21", "2020/02/22", "2020/02/23", "2020/02/24", "2020/02/25",
           "2020/02/26", "2020/02/27", "2020/02/28", "2020/02/29", "2020/03/01",
           "2020/03/02", "2020/03/03", "2020/03/04", "2020/03/05", "2020/03/06",
           "2020/03/07", "2020/03/08", "2020/03/09")

# Create a data frame with the dates
df <- data.frame(Day = as.Date(dates))

# Add a column with sequential numerical values from 1 to 18
df$Sequential_Number <- 1:nrow(df)

#Merge df with Summer data:
Summer$Day <-  as.Date(Summer$Day)
SummerNum <- left_join(Summer, df, by = "Day" )
View(SummerNum)


#Quick Plot:
  ggplot(data = Summer, aes(y= Temp_C, x=Day, fill=BottomSurface)) +
   geom_point (aes(fill=BottomSurface))+
   labs(x="Day", y = (bquote(Water~Temperature~("\u00B0"~C))))+
  ggtitle("Winter")+
  facet_grid(.~BottomSurface)


#Summer + Winter NICE PLOT:=======
Summer2020 <- read_excel("All_cleaned_temp_data_and_calibration.xlsx", sheet = "SummerCalibrated")

Summer <- gather(Summer2020, "OnBoard", "Temp_C", -DateTime) %>%
         separate(OnBoard, into = c("BoardID", "BottomSurface"), remove = F) %>%
        mutate(Season = "Summer") %>%
       mutate( Day = format(as.POSIXct(.$DateTime,format="%Y-%m-%d %H:%M:%S"),format='%Y/%m/%d'))

unique(Summer$Day) 
# Create a vector of dates
dates <- c("2020/02/21", "2020/02/22", "2020/02/23", "2020/02/24", "2020/02/25",
           "2020/02/26", "2020/02/27", "2020/02/28", "2020/02/29", "2020/03/01",
           "2020/03/02", "2020/03/03", "2020/03/04", "2020/03/05", "2020/03/06",
           "2020/03/07", "2020/03/08", "2020/03/09")

# Create a data frame with the dates
df <- data.frame(Day = as.Date(dates))

# Add a column with sequential numerical values from 1 to 18
df$Sequential_Number <- 1:nrow(df)

#Merge df with Summer data:
Summer$Day <-  as.Date(Summer$Day)
SummerNum <- left_join(Summer, df, by = "Day" )


SummerSum2 <- SummerNum %>%
  mutate(BottomSurface2 = ifelse(BottomSurface =="S","Surface","Bottom"))%>%
  group_by(BottomSurface2,Sequential_Number ) %>%
      summarise( AV =  mean (Temp_C, na.rm=T), 
                N = n(),
                SD = sd (Temp_C, na.rm=T),
                SE = SD / sqrt(N)) %>%
  mutate(Season = "Summer")

SummerSum2



#Winter 
Winter2020 <- read_excel("All_cleaned_temp_data_and_calibration.xlsx", sheet = "WinterCalibrated")
Winter <- gather(Winter2020, "OnBoard", "Temp_C", -DateTime) %>%
  separate(OnBoard, into = c("BoardID", "BottomSurface"), remove = F) %>%
  mutate(Season = "Winter") %>%
    mutate( Day =
      format(as.POSIXct(.$DateTime,format="%Y-%m-%d %H:%M:%S"),format='%Y/%m/%d'))

# Create a vector of dates
winter_dates <- c("2021/08/05", "2021/08/06", "2021/08/07", "2021/08/08", "2021/08/09",
           "2021/08/10", "2021/08/11", "2021/08/12", "2021/08/13", "2021/08/14",
           "2021/08/15", "2021/08/16", "2021/08/17", "2021/08/18", "2021/08/19",
           "2021/08/20")

# Create a data frame with the dates
df2 <- data.frame(Day = as.Date(winter_dates))

# Add a column with sequential numerical values from 1 to 16
df2$Sequential_Number <- 1:nrow(df2)

#Merge df2 with Winter data:
Winter$Day <-  as.Date(Winter$Day)
WinterNum <- left_join(Winter, df2, by = "Day" )

names(Winter)
WinterSum2 <- WinterNum %>%
  mutate(BottomSurface2 = ifelse(BottomSurface =="S","Surface","Bottom"))%>%
  group_by( BottomSurface2,Sequential_Number ) %>%
      summarise( AV =  mean (Temp_C, na.rm=T), 
                N = n(),
                SD = sd (Temp_C, na.rm=T),
                SE = SD / sqrt(N)) %>%
    mutate(Season = "Winter")

WinterSum2



#PLOT 2 SEASONS TOGETHER:
Sum2 <- rbind( WinterSum2, SummerSum2)

AllTempPlot <- ggplot(data = Sum2, aes(y= AV, x = Sequential_Number, group=Season)) +
  geom_point(aes(col= Season),size=4)+
  geom_errorbar(aes(ymin = AV-SD, ymax = AV+SD))+
  
    geom_line(aes(colour = Season), linetype = 1,size=1.2, alpha=0.4)+
   # scale_y_continuous(limits = c(10,25))+


   labs(x="Day", y = (bquote(Water~Temperature~("\u00B0"~C))))+
   # ggtitle("Summer")+ # (Mean+SD)
   facet_grid(Season~BottomSurface2)+
  
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=10,colour = "black"),
        axis.text.y=element_text(size=13,colour = "black"),
        axis.title.y=element_text(size=18,colour = "black",face = "bold"),
        axis.title.x=element_text(size=18,  hjust = 0.5,colour = "black",face = "bold"),
        legend.position = "none",
        strip.background = element_rect(fill="white"),
       plot.title = element_text(hjust=0.5, size = 22, face="bold"),
        strip.text=element_text(size=18))

AllTempPlot
