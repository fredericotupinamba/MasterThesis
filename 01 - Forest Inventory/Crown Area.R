# HEADER #############################################################
#                                                                    #
#                  STATISTICAL INVENTORY ANALYSIS                    #
#                          DURALL PROJECT                            #
#                                                                    #
#   RESPONSIBLE: FREDERICO TUPINAMBA SIMÕES                          #
#   JOB TITLE:   ENGENHEIRO FLORESTAL                                #
#   E-MAIL:      frericotupinamba@gmail.com                          #
#                                                                    #


# Verify the package is installed ####################################
#create a function to check for installed packages and install them if they are not installed
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# Install packs
packs <- c("RStoolbox", "readxl", 
           "maps", "ggplotAssist", "shinyjs", 
           "ggplot2", "ggthemes", "mondate", "lubridate",
           "mod", "minpack.lm", "tidyverse", "lmfor", 
           "deSolve", "vegan", "measures", "sqldf", "xlsx")

install(packs)


# Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

######################################################################
#### BASICS DEFINITIONS ####
# Defining the path
getwd()
setwd("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/")
out_dir <- "E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/"


######################################################################
#### Crown Area ####
CAav <- inventory %>% filter(AU == "5", YEAR == "2020", DBH >= 0)

# Tree coordinates
TreeXY <- read_excel("1-DATA\\1.3-GNSS\\1.3.5-FINAL\\TREES_XYZ.xls", 
                        sheet = 1,
                        col_names = TRUE,
                        col_types = c(rep("text", 6), rep("numeric", 8)))

# Mergim table
TreeXY <- left_join(TreeXY, CAav[,c("ID", "YEAR", "AU", "PARCEL", "PLOT", "LINE", "TREE", "DBH", "H", "Hd" )], by = c("AU", "PARCEL", "PLOT", "LINE", "TREE"))

# Crown Área Calculos
TreeXY$CAav <- ((TreeXY$DBH/100) / exp(-2.13471))^(1/0.335344)
TreeXY$CAav2 <- (1.32508 + 0.004548 * TreeXY$DBH^2)

# Writing the data
getwd()
write.xlsx(x = TreeXY,
           file = file.path(out_dir, "1-DATA\\1.3-GNSS\\1.3.5-FINAL\\Processado_Tree_XYZ.xls"),
           sheetName = "TREEXY",
           showNA = FALSE)
