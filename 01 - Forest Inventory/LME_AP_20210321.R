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
           "deSolve", "vegan", "measures", "sqldf", "xlsx",
           "lme4", "lmerTest", "optimx", "RColorBrewer", "viridis", 
           "sjPlot", "ggeffects", "effects", "mgcv", "merTools", "psycho", "lmerTest")


install(packs)



######################################################################
# remove(TMP, TMP2, TMP2019, TMP2020, TMP3)

# Creating a tmp table
TMP <- inventory


# Add volume from inventory
TMP <- left_join(TMP, AD[,c("AU", "STAND", "YEAR", "TREE_HA", 'STAND_Area', "Vol_ha")], 
                     by = c("AU", "STAND", "YEAR"))


# Filtering out the unmeasured trees.
TMP <- TMP %>% filter(DBH != "")


# Transforming age to years
TMP$AGE_Y <- round(TMP$AGE / 365, 0)


# Creating class of Tree / ha
range(TMP$TREE_HA)
TMP$Tree_ha_Class[TMP$TREE_HA <= 250] <- "0-250"
TMP$Tree_ha_Class[TMP$TREE_HA > 250 & TMP$TREE_HA <= 500 ] <- "250-500"
TMP$Tree_ha_Class[TMP$TREE_HA > 500 & TMP$TREE_HA <= 750 ] <- "500-750"
TMP$Tree_ha_Class[TMP$TREE_HA > 750 & TMP$TREE_HA <= 1000 ] <- "750-1000"
TMP$Tree_ha_Class[TMP$TREE_HA > 1000 & TMP$TREE_HA <= 1200 ] <- "1000-1250"
TMP$Tree_ha_Class[TMP$TREE_HA > 1200 & TMP$TREE_HA <= 1500 ] <- "1250-1500"


# Transforming diameter class into factor, to sort them on graphs.
TMP$Tree_ha_Class <- factor(TMP$Tree_ha_Class, levels = c("0-250", "250-500", "500-750", "750-1000", "1000-1250", "1250-1500"))
TMP$AU <- factor(TMP$AU, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
TMP$AGE_Y <- factor(TMP$AGE_Y, levels = c("1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13") )

# Spliting the table by Year
TMP2019 <- TMP %>% filter(YEAR == 2019)
TMP2020 <- TMP %>% filter(YEAR == 2020)  


# Merge the year side by side                    # I am having problems here with unmeasured trees
TMP2 <- merge(x = TMP2020,
              y = TMP2019,
              by = c("ID", "AU", "STAND", "PLOT", "LINE", "TREE"),
              suffixes = c("_2020", "_2019"),
              #all.y = T   # only trees in 2019
              ) 


# Add data (Clones and Planting Density) from the STAND table
#TMP2 <- left_join(TMP2, STAND[,c("ID", "AU", "STAND", "MATERIAL", "DENSITY")], by = c("ID", "AU", "STAND"))


# Selecting some columns
TMP2 <- TMP2 %>% dplyr::select(
  # Stand data
  "ID", "AU", "STAND", MATERIAL = "MATERIAL_2020", "PLOT", "STAND_Area_2019",
  # 2020 inventory data
  "STEM_2020", "QUALITY_COD_2020", "DBH_2020", 
  "DG_2020", "g_2020", "G_2020", "GR_2020", "AGE_2020", 
  "Hd_2020", "aid_2020", "aih_2020", "Hp_2020", "v_ff_2020",
  "TREE_HA_2020", "Tree_ha_Class_2020", "Vol_ha_2020", "AGE_Y_2020",
  # 2019 inventory data
  "STEM_2019", "QUALITY_COD_2019",  "DBH_2019", 
  "DG_2019", "g_2019", "G_2019", "GR_2019", "AGE_2019", 
  "Hd_2019", "aid_2019", "aih_2019", "Hp_2019", "v_ff_2019",
  "TREE_HA_2019", "Tree_ha_Class_2019", "Vol_ha_2019", "AGE_Y_2019")


# Creating the mortality code (Live = 1, Dead = 0, Dead in 2019 = 2, Revived = 3 (???))
TMP2$Mortality[(TMP2$QUALITY_COD_2019 == 0 & TMP2$QUALITY_COD_2020 == 0)] <- 0
TMP2$Mortality[(TMP2$QUALITY_COD_2019 == 0 & TMP2$QUALITY_COD_2020 == 1)] <- 1
TMP2$Mortality[(TMP2$QUALITY_COD_2019 == 1 & TMP2$QUALITY_COD_2020 == 1)] <- 2
TMP2$Mortality[(TMP2$QUALITY_COD_2019 == 1 & TMP2$QUALITY_COD_2020 == 0)] <- 3 # Did this trees resurrected

summary(as.factor(TMP2$QUALITY_COD_2020))
summary(as.factor(TMP2$QUALITY_COD_2019))
summary(as.factor(TMP2$Mortality))            # WITH dead trees in 2019

# Data without the dead trees in 2019
TMP3 <- TMP2 %>% filter(QUALITY_COD_2019 != 1)

TMP3$MATERIAL <- factor(TMP3$MATERIAL, levels = c("APS F2", "H13", "I 144", "1277"))

######################################################################
# Random intercept with fixed mean
# Random effect on the intercept for each factor level
# 1
Model_01 <- lmer(Mortality ~ (1 | MATERIAL), data = TMP3)
# 2
Model_02 <- lmer(Mortality ~ (1 | Tree_ha_Class_2020), data = TMP3)
# 3
Model_03 <- lmer(Mortality ~ (1 | AGE_Y_2020), data = TMP3)

######################################################################
# RANDOM EFFECTS 
# Intercept varying among g1 and g2 within g1
Model_04 <- lmer(Mortality ~ (1 |  MATERIAL / Tree_ha_Class_2020), data = TMP3)
Model_05 <- lmer(Mortality ~ (1 |  MATERIAL / AGE_Y_2020), data = TMP3)


######################################################################
# RANDOM EFFECTS 
# Intercept varying among g1 and g2 within fixed variable
Model_06 <- lmer(Mortality ~ DBH_2019 + g_2019 + v_ff_2019 + DG_2019  + 
                (1 | Tree_ha_Class_2020 / AGE_Y_2020), data = TMP3)



# Summary
summary(Model_01)                      # Pr(>|t|) = 0.0703 .
summary(Model_02)                      # Pr(>|t|) = 0.0847 .
summary(Model_03)                      # Pr(>|t|) = 0.0852 .
summary(Model_04)                      # Pr(>|t|) = 0.0812 .
summary(Model_05)                      # Pr(>|t|) = 0.106
summary(Model_06)                      # Pr(>|t|) =

# Summary table
install.packages("pbkrtest-package")
require(pbkrtest)
install.packages("webshot")
library(webshot)

tab_model(Model_01, title = "MixModel 01 - Genotype", file = paste0(out, "Modelo_01.html"))
tab_model(Model_02, title = "MixModel 02 - Stocking", file = paste0(out, "Modelo_02.html"))
tab_model(Model_03, title = "MixModel 03 - Rotation age", file = paste0(out, "Modelo_03.html"))
tab_model(Model_04, title = "MixModel 04 - Interaction between Genotype and Stocking", file = paste0(out, "Modelo_04.html"))
tab_model(Model_05, title = "MixModel 05 - Interaction between Genotype and Rotation Age", file = paste0(out, "Modelo_05.html"))
tab_model(Model_06, title = "MixModel 06", file = paste0(out, "Modelo_06.html"))



# Randon Effects
library(Hmisc)
library(lattice)
ranef(Model_01)
capture.output(ranef(Model_01), file = paste0(out_dir,"RANEF.txt"), append = TRUE)
ranef(Model_02)
capture.output(ranef(Model_02), file = paste0(out_dir,"RANEF.txt"), append = TRUE)
ranef(Model_03)
capture.output(ranef(Model_03), file = paste0(out_dir,"RANEF.txt"), append = TRUE)
ranef(Model_04)
capture.output(ranef(Model_04), file = paste0(out_dir,"RANEF.txt"), append = TRUE)
ranef(Model_05)
capture.output(ranef(Model_05), file = paste0(out_dir,"RANEF.txt"), append = TRUE)
ranef(Model_06)
capture.output(ranef(Model_06), file = paste0(out_dir,"RANEF.txt"), append = TRUE)



dotplot(ranef(Model_04, condVar=TRUE))
  # dev.copy(jpeg , paste0(out, 'RANEF - 02 - Material', '.jpg'),
  #          width = 611, # The width of the plot in inches
  #          height = 379) # The height of the plot in inches
  # dev.off()
  
# Graphics
######### MODEL 01 #########
Graphic_model01 <- plot_model(Model_01, type = "re")

Graphic_model01 <- Graphic_model01 + ylim(-.25, .25) +
  labs(title = "", y = "Mortality", x = "Genotype")       #title = "Random effects - Model 1"
  
ggsave(
  filename = "RANEF - 01 - Modelo_01.jpg",
  path = out,
  plot = Graphic_model01,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)

######### MODEL 02 #########                          
Graphic_model02 <- plot_model(Model_02, type = "re")

Graphic_model02 <- Graphic_model02 + 
  ylim(-.25, .25) + 
  labs(title = "", y = "Mortality", x = "Stocking (tree/ha)")

ggsave(
  filename = "RANEF - 02 - Modelo_02.jpg",
  path = out,
  plot = Graphic_model02,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
) 

######### MODEL 03 #########                          
Graphic_model03 <- plot_model(Model_03, type = "re")

Graphic_model03 <- Graphic_model03 + ylim(-.25, .50) +
  labs(title = "", x = "Age (yrs)", y = "Mortality")

ggsave(
  filename = "RANEF - 03 - Modelo_03.jpg",
  path = out,
  plot = Graphic_model03,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)

######### MODEL 04 #########                          
Graphic_model04 <- plot_model(Model_04, type = "re")

Graphic_model04A <- Graphic_model04[[1]] + ylim(-.25, .25) +
  labs(title = "", y = "Mortality", x = "Stocking / Genotype")

ggsave(
  filename = "RANEF - 04 - Modelo_04 a.jpg",
  path = out,
  plot = Graphic_model04A,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)

Graphic_model04B <- Graphic_model04[[2]] + ylim(-.25, .25) +
  labs(title = "", y = "Mortality", x = "Stocking")

ggsave(
  filename = "RANEF - 04 - Modelo_04 B.jpg",
  path = out,
  plot = Graphic_model04B,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)

######### MODEL 05 #########                          
Graphic_model05 <- plot_model(Model_05, type = "re")

Graphic_model05A <- Graphic_model05[[1]] + ylim(-.25, .25) +
  labs(title = "", y = "Mortality", x = "Age / Genotype")

ggsave(
  filename = "RANEF - 05 - Modelo_05 A.jpg",
  path = out,
  plot = Graphic_model05A,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)

Graphic_model05B <- Graphic_model05[[2]] + ylim(-.25, .25) +
  labs(title = "", y = "Mortality", x = "Stocking")

ggsave(
  filename = "RANEF - 05 - Modelo_05 B.jpg",
  path = out,
  plot = Graphic_model05B,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)

######### MODEL 06 #########                          
Graphic_model06 <- plot_model(Model_06, type = "re")

Graphic_model06A <- Graphic_model06[[1]] + ylim(-.25, .25) +
  labs(title = "", y = "Mortality", x = "Age / Stocking")

ggsave(
  filename = "RANEF - 06 - Modelo_06 A.jpg",
  path = out,
  plot = Graphic_model06A,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)

Graphic_model06B <- Graphic_model06[[2]] + ylim(-.25, .25) +
  labs(title = "", y = "Mortality", x = "Stocking")

ggsave(
  filename = "RANEF - 06 - Modelo_06 B.jpg",
  path = out,
  plot = Graphic_model06B,
  device = "jpeg",
  dpi = 600,
  width = 20,
  height = 15,
  units = "cm"
)


# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
# library(ggpubr)

ggarrange(Graphic_model01, Graphic_model02, Graphic_model03,
          Graphic_model04A, Graphic_model05A, Graphic_model06A,
          labels = c("A)", "B)", "C)",
                     "D)", "E)", "F)"
                     ),
          ncol = 3, nrow = 2, align = "hv")

ggsave(
  filename = "RANEF - Grid 3x2.jpg",
  path = out,
  plot = last_plot(),
  device = "jpeg",
  dpi = 600,
  width = 30,
  height = 20,
  units = "cm"
)


# AIC
MODELOS <- 
  anova(Model_01,            # 2896.9
        Model_02,           # 3279.6
        Model_03,           # 3259.5
        Model_04,
        Model_05,
        Model_06)



MODELOS %>% arrange(AIC)