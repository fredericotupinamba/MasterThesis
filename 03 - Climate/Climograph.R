# Verify the package is installed ####################################
#create a function to check for installed packages and install them if they are not installed
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}


# Packs to install and load
packs <- c("RStoolbox", "readxl", 
           # Plotting data
           "ggplotAssist", "shinyjs", "ggplot2", "ggthemes", "xlsx",
           # Processing data 
           "mondate", "tidyverse", "lmfor", 
           "deSolve", "measures", "sqldf", "zoo",
           "strucchange", "bfast", "dplyr", 
           # Maps
           "tmaptools", "maptools", "stringr", 'raster', 'rgdal', 'dismo', 
           "maps", "sf", "tmap", "rgeos", "RColorBrewer",
           # Others
           "dismo")

install(packs)

# Defining the path
setwd("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/")

# Maximum Temperatures
Tmax <- read.csv("1-DATA\\1.1-RAW\\4-CLIMATE\\CSV\\Tangara da Serra-Temperature - Maximum (ECMWF ERA5)-Monthly time series.csv",
                 sep = ",",
                 dec = ".",
                 skip = 1)

Tmax <- separate(Tmax, col = "Month", sep = "-", into = c("Month", "Year"))

Tmax$Month <- str_trim(Tmax$Month, "both")
Tmax$Year <- str_trim(Tmax$Year, "both")

Tmax$Month <- factor(Tmax$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Minimum Temperatures
Tmin <- read.csv("1-DATA\\1.1-RAW\\4-CLIMATE\\CSV\\Tangara da Serra-Temperature - Minimum (ECMWF ERA5)-Monthly time series.csv",
                 sep = ",",
                 dec = ".",
                 skip = 1)

Tmin <- separate(Tmin, col = "Month", sep = "-", into = c("Month", "Year"))

Tmin$Month <- str_trim(Tmin$Month, "both")
Tmin$Year <- str_trim(Tmin$Year, "both")

Tmin$Month <- factor(Tmin$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Mean Temperatures
Tmean <- read.csv("1-DATA\\1.1-RAW\\4-CLIMATE\\CSV\\Tangara da Serra-Temperature - Mean (ECMWF ERA5)-Monthly time series.csv",
                 sep = ",",
                 dec = ".",
                 skip = 1)

Tmean <- separate(Tmean, col = "Month", sep = "-", into = c("Month", "Year"))

Tmean$Month <- str_trim(Tmean$Month, "both")
Tmean$Year <- str_trim(Tmean$Year, "both")

Tmean$Month <- factor(Tmean$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Historic rainfall series
Precipitation <- read.csv("1-DATA\\1.1-RAW\\4-CLIMATE\\CSV\\Tangara da Serra-Precipitation - CHIRPS-Monthly time series.csv",
                          sep = ",",
                          dec = ".",
                          skip = 1)

Precipitation <- separate(Precipitation, col = "Month", sep = "-", into = c("Month", "Year"))

Precipitation$Month <- str_trim(Precipitation$Month, "both")
Precipitation$Year <- str_trim(Precipitation$Year, "both")

Precipitation$Month <- factor(Precipitation$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Sorting out the 2019 data
Tmean_2019 <- Tmean %>% filter(Year == 2019) %>%  group_by(Year, Month) %>%  summarise(Temp = mean(Monthly.Average))
Tmin_2019 <- Tmin %>% filter(Year == 2019) %>%  group_by(Year, Month) %>%  summarise(Temp = mean(Monthly.Average))
Tmax_2019 <- Tmax %>% filter(Year == 2019) %>%  group_by(Year, Month) %>%  summarise(Temp = mean(Monthly.Average))
Preci_2019 <- Precipitation %>% filter(Year == 2019) %>% group_by(Year, Month) %>% summarise(Prec = mean(Monthly.Sum))

# Plotting the graphic
graphico <- ggplot() +
  stat_summary(data = Tmax, fun = mean, geom = "point", aes(x = Month, y = Monthly.Average, color = "Max")) + 
  stat_summary(data = Tmean, fun = mean, geom = "point", aes(x = Month, y = Monthly.Average, color = "Mean")) + 
  stat_summary(data = Tmin, fun = mean, geom = "point", aes(x = Month, y = Monthly.Average, color = "Min")) + 
  geom_line(data = Tmax_2019, aes(x = Month, y = Temp, group = Year, linetype = "2019"), color = "black") + 
  geom_line(data = Tmean_2019, aes(x = Month, y = Temp, group = Year, linetype = "2019"), color = "black") + 
  geom_line(data = Tmin_2019, aes(x = Month, y = Temp, group = Year, linetype = "2019"), color = "black") +
  geom_bar(data = Preci_2019, aes(x = Month, y = Prec/20, fill = "Preci"), stat = "identity", alpha = 2) +
  stat_summary(data = Precipitation, fun = mean, geom = "point", aes(x = Month, y = Monthly.Sum/20, fill = "PreciP"), shape=23, size = 2, show.legend = T) + 
  scale_y_continuous(sec.axis = sec_axis(~.*20, name = "Precipitation (mm)")) +
  labs(x = NULL, y = "Temperature (ºC)") +
  scale_colour_manual("Temperature (1982 - 2019)",
    breaks = c("Max", "Mean", "Min"),
    values = c("Max"="red", "Mean"="orange", "Min"="blue4"), 
    label = c("Max mean", "Average mean", "Min mean")) + 
  scale_linetype_manual("Temperature 2019", values = c("2019" = "dashed")) + 
  scale_fill_manual("Precipitation", values = c("Preci" = "gray", "PreciP" = "cyan"), label = c("2019", "1981 - 2019")) + 
  theme(
    axis.title.y.left = element_text(vjust = 2, size = 12),
    axis.title.y.right = element_text(vjust = 2, size = 12),
    axis.text.y.left = element_text(angle = 90, size = 10, hjust = 0.5),
    axis.text.y.right = element_text(angle = 90, size = 10, hjust = 0.5)
  )


graphico                 

