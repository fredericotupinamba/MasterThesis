# HEADER #############################################################
#                                                                    #
#                  Timber Price List from Durall                     #
#                          Mater Thesis                              #
#                                                                    #
#   RESPONSIBLE: FREDERICO TUPINAMBA SIMÕES                          #
#   JOB TITLE:   Forester                                            #
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
packs <- c("RStoolbox", "readxl", "forestmangr",
           "ggplotAssist", "ggplot2", "ggthemes", 
           "tidyverse", "xlsx")

install(packs)
######################################################################
#### BASICS DEFINITIONS ####
# Defining the path
getwd()
setwd("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/")

#### Reading Data ####
## Price list ##
PRICE <- read_excel("4-THESIS/4.1-DRAFTS/03 - Tables/Price List.xlsx", 
                    sheet =  1,
                    col_names = TRUE)

# Organazing the DATA
PRICE$`US$/un (5,69)`<- NULL
PRICE$`Vol (m3)` <- NULL
PRICE$`US$ / m3` <- NULL
colnames(PRICE)[2] <- "UN_real"

# Setting the diameter class
PRICE$Classe[PRICE$D2 <= 4] <- "Residue"
PRICE$Classe[PRICE$D2 >  4 & PRICE$D2 <=  10] <- "04 - 10"
PRICE$Classe[PRICE$D2 >  10 & PRICE$D2 <= 20] <- "10 - 20"
PRICE$Classe[PRICE$D2 >  20 & PRICE$D2 <= 30] <- "20 - 30"
PRICE$Classe[PRICE$D2 > 30] <- "> 30"

# Setting classe levels
PRICE$Classe <- factor(PRICE$Classe, levels = c("Residue", "04 - 10", "10 - 20", "20 - 30", "> 30"))

# Getting the volume
PRICE$UN_vol <- with(PRICE, ((pi/4)*((D1 + D2)/200)^2)*Lenght)

# Getting the price in US$
US <- 5.69
PRICE$UN_USS <- PRICE$UN_real / US

# Getting the price in cubic meter
PRICE$M3_USS <- PRICE$UN_USS / PRICE$UN_vol

# Prices Table
RESUMO_PRICE <-
  PRICE %>% group_by(Classe) %>% summarise(
    N_Class = n(),
    Price_MEAN = mean(M3_USS, na.rm = T),
    Price_MIN = min(M3_USS, na.rm = T), 
    Price_MAX = max(M3_USS, na.rm = T),
    Price_SD = sd(M3_USS, na.rm = T))


ggplot(PRICE, aes(x = Classe, y = M3_USS)) + 
  geom_boxplot() + 
  geom_line(data = RESUMO_PRICE, mapping = aes(x = Classe, y = Price_MEAN, group = 1, color = "red")) + 
  labs(title = "Timber price", x = "Class diameter", 
       y =  expression('Price'~(m^{3}~"/ US$")), col = "Mean:")


head(RESUMO_PRICE)
data.frame(unique(RESUMO_PRICE$Class))
