# Verify the package is installed ####################################
#create a function to check for installed packages and install them if they are not installed
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# usage
packs <- c("sf", "tmap", "tmaptools", "maps", "stringr", "dplyr")
#"RStoolbox", "readxl", 
#, "ggplotAssist", "shinyjs", 
#"ggplot2", "ggthemes", "mondate", "lubridate",
#"mod", "minpack.lm", "tidyverse", "lmfor", 
#"deSolve", "vegan", "measures", "sqldf", "xlsx"


install(packs)



# Defining the path
setwd("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/")

# Shapes da Área Total da Propriedade
APRT <- st_read("1-DATA/1.1-RAW/2-SHAPE/TOTAL_AREA.shp")

# Shapes de Unidades de Produção Anual
AU <- st_read("1-DATA/1.1-RAW/2-SHAPE/AU.shp")                       # Import Annual Unit

# Shapes dos Talhões
STAND_SHP <- st_read("1-DATA/1.1-RAW/2-SHAPE/PARCEL2.shp")              # Import Parcel

for (i in 1:length(STAND_SHP$TALHAO)) {                                 # Fixing values
  STAND_SHP$TALHAO1[i] <- c(as.character(STAND_SHP$TALHAO[i]))
  STAND_SHP$TALHAO2[i] <- if((str_starts(STAND_SHP$TALHAO1[i], "0")) == TRUE) {
    str_remove(STAND_SHP$TALHAO1[i], "0")
  } else
    STAND_SHP$TALHAO2[i] <- STAND_SHP$TALHAO1[i]
}

STAND_SHP$ID <- paste(STAND_SHP$MODULO, STAND_SHP$TALHAO2, sep = "-")         # Creating the ID column

STAND_SHP <- STAND_SHP[ ,c(7, 3, 6, 4)]                                    # Arrange the data
colnames(STAND_SHP) <- c("ID", "AU", "STAND", "geometry")

STAND_SHP <- merge(x = STAND_SHP,                                          # Merge the data.frame to the SHP
                   y = STAND[ ,c("ID", "AREA_2019", "AREA_2020", "DENSITY", "DATA PLANTIO")], 
                   by = "ID", all.x=TRUE)


PARCEL2 <- AD %>% filter(YEAR == 2019)
PARCEL3 <- AD %>% filter(YEAR == 2020)

STAND_SHP_2019 <- merge(x = STAND_SHP,                                          # Merge the data.frame to the SHP
                        y = PARCEL2, 
                        by = c("AU", "STAND"), all.x = FALSE)

STAND_SHP_2020 <- merge(x = STAND_SHP,                                          # Merge the data.frame to the SHP
                        y = PARCEL3, 
                        by = c("AU", "STAND"), all.x = FALSE)


# Plots
PLOTS_SHP <- st_as_sf(Plot, coords = c("POINT_X", "POINT_Y"), crs = st_crs(STAND_SHP))
PLOTS_SHP_area <- st_buffer(PLOTS_SHP, 18)

# Printing the map
tmap_mode("view")                                                    # plot ou view
tm_shape(APRT) + 
  tm_polygons(alpha = 0, lwd = 2, border.col = "black") +
  tm_shape(STAND_SHP, is.master = TRUE) + 
  tm_polygons("Vtotal",
              id = "TALHAO",
              title = "Total Volume",
              palette = "YlOrRd", n = 5, contrast = c(0.26, 0.8)) + 
  #tm_legend(legend.outside = TRUE,
  #          legend.outside.position = "right",
  #          position = c("LEFT", "BOTTOM"), 
  #          frame = TRUE) + 
  #tm_credits("Frederico Simões", position = c("RIGHT", "BOTTOM")) + 
  #tm_layout(title = expression('Volume por hectare'~(m^{3})),
  #         title.position = c("center", "top")) + 
  tm_shape(PLOTS_SHP_area) + 
  tm_polygons(col = "white") +
  tm_shape(PLOTS_SHP) + 
  tm_dots(col = "black")

# Tree data
library("sp")

tree_xy <- read.xlsx2("1-DATA\\1.3-GNSS\\1.3.5-FINAL\\TREES_XYZ.xlsx",
                      sheetIndex = 1,
                      header = TRUE)

tree_xy$N <- as.numeric(tree_xy$N)
tree_xy$E <- as.numeric(tree_xy$E)




head(tree_xy[ ,c(8,7)])

#tmaptools::palette_explorer()

# Writing the SHP file
st_write(STAND_SHP_2019, "1-DATA/1.2-PROCESSED/2-SHAPE/2019/STAND_SHP_2019.shp", append = FALSE)
st_write(STAND_SHP_2020, "1-DATA/1.2-PROCESSED/2-SHAPE/2020/STAND_SHP_2020.shp", append = FALSE)

st_write(AU, "1-DATA/1.2-PROCESSED/2-SHAPE/2020/AU.shp", append = FALSE)

st_write(PLOTS_SHP, "1-DATA/1.2-PROCESSED/2-SHAPE/2020/PLOTS.shp")
st_write(PLOTS_SHP, "1-DATA/1.2-PROCESSED/2-SHAPE/2020/PLOTS_AREA.shp")
