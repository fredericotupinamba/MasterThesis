library(ggplot2)
library(dplyr)

# Setting the values for B(s)
parametros <- data.frame(Classe = "Clone", B0 = 1.1985, B1 = -3.4085, B2 = 14.7784, B3 = -35.5733, B4 = 37.9110, B5 = -14.9297)
parametros <- rbind(parametros, data.frame(Classe = "Seminal", B0 = 1.1728, B1 = -3.7189, B2 = 18.7673, B3 = -48.2095, B4 = 54.2005, B5 = -22.3883 ))

# Creating a new database
TAPER <- inventory %>% filter(YEAR == 2020, DBH > 0) %>% dplyr::select(ID, YEAR, AU, STAND, MATERIAL, PLOT, LINE, TREE, QUALITY_COD, DBH, Hp, v_ff)

# Loop to split each tree into 2-meter log and calculate the smallest diameter
TREES_TAPER1 <- NULL
TREES_TAPER2 <- NULL
DB_TAPER_2020 <- NULL
for (i in 1:nrow(TAPER)) {
  tree <- TAPER[i,]
  #ID <- tree[c("ID", "YEAR", "AU", "STAND", "PLOT", "LINE", "TREE", "MATERIAL", "DBH", "Hp", "v_ff")]
  for (h in seq(0,tree$Hp,2)) {
    t <- h / tree$Hp
    if(grepl("APS F2", tree$MATERIAL)) {
      # The parameters for seminal
      valores <- parametros[2,]
      # Schöepfer (1966)
      dap <- tree$DBH*(valores$B0 + valores$B1*(t) + valores$B2*(t^2) + valores$B3*(t^3) + valores$B4*(t^4) + valores$B5*(t^5))
      TREES_TAPER1 <- rbind(TREES_TAPER1, data.frame(tree, dap, h))
    } else {
      # The parameter for clonal
      valores <- parametros[1,]
      # Schöepfer (1966)
      dap <- tree$DBH*(valores$B0 + valores$B1*(t) + valores$B2*(t^2) + valores$B3*(t^3) + valores$B4*(t^4) + valores$B5*(t^5))
      TREES_TAPER2 <- rbind(TREES_TAPER2, data.frame(tree, dap, h))
    }
  }
  DB_TAPER_2020 <- rbind(DB_TAPER_2020, TREES_TAPER1, TREES_TAPER2)
  TREES_TAPER1 <- NULL
  TREES_TAPER2 <- NULL
}

# Creating one Tree Id for each tree
DB_TAPER_2020$TREE_ID <- with(DB_TAPER_2020, paste(AU, STAND, PLOT, LINE, TREE, sep = "-"))

# # Equation for volume calculation used in the paper
# fpoli <- function(h, dap, ht) if(grepl("APS F2", tree$MATERIAL)){
#   valores <- parametros[2,]
#   (pi / 40000) * 
#   ((dap * (valores$B0 + 
#              valores$B1 * (h / ht) + 
#              valores$B2 * ((h / ht)^2) + 
#              valores$B3 * ((h / ht)^3) + 
#              valores$B4 * ((h / ht)^4) + 
#              valores$B5 * ((h / ht)^5)))^2)
# } else{
#   valores <- parametros[1,]
#   (pi / 40000) * 
#     ((dap * (valores$B0 + 
#                valores$B1 * (h / ht) + 
#                valores$B2 * ((h / ht)^2) + 
#                valores$B3 * ((h / ht)^3) + 
#                valores$B4 * ((h / ht)^4) + 
#                valores$B5 * ((h / ht)^5)))^2)
# }
# 
# vpoli <- function(h,dap,ht) integrate(fpoli,lower=0.1, upper=h, dap=dap, ht=ht)$value 
# poli.m3 <- mapply(vpoli, h = as.list(DB_TAPER$h, DB_TAPER$TREE_ID), dap=as.list(DB_TAPER$dap), ht=as.list(DB_TAPER$Hp))
# DB_TAPER <- cbind(DB_TAPER, v = as.numeric(poli.m3))

# Volume calculation in each log
for (i in 1:nrow(DB_TAPER_2020)) {
  Tree1 <- DB_TAPER_2020[i, ]
  Tree1$g <- (pi/4)*(Tree1$dap/100)^2
  
  Tree2 <- DB_TAPER_2020[i+1, ]
  Tree2$g <- (pi/4)*(Tree2$dap/100)^2
  
  if (Tree1$TREE_ID == Tree2$TREE_ID) {
    DB_TAPER_2020$v_ff_tapper[i] <- ((Tree1$g + Tree2$g)/2 * (Tree2$h - Tree1$h))
  } else{
    DB_TAPER_2020$v_ff_tapper[i] <- Tree1$g * (Tree1$Hp - Tree1$h) * 1/3
  } 
}

# Setting the diameter class
DB_TAPER_2020$Classe[DB_TAPER_2020$dap <= 4] <- "Residue"
DB_TAPER_2020$Classe[DB_TAPER_2020$dap >  4 & DB_TAPER_2020$dap <=  10] <- "04 - 10"
DB_TAPER_2020$Classe[DB_TAPER_2020$dap >  10 & DB_TAPER_2020$dap <= 20] <- "10 - 20"
DB_TAPER_2020$Classe[DB_TAPER_2020$dap >  20 & DB_TAPER_2020$dap <= 30] <- "20 - 30"
DB_TAPER_2020$Classe[DB_TAPER_2020$dap > 30] <- "> 30"

# Setting classe levels
DB_TAPER_2020$Classe <- factor(DB_TAPER_2020$Classe, levels = c("Residue", "04 - 10", "10 - 20", "20 - 30", "> 30"))
DB_TAPER_2020$MATERIAL <- factor(DB_TAPER_2020$MATERIAL, levels =c("APS F2", "H13", "I 144", "1277"))

# Plotting the data
ggplot(DB_TAPER_2020, aes(x = Classe, fill = MATERIAL)) + 
  geom_histogram(stat="count") + 
  labs(title = "Total of logs per diameter class", x = "Diameter Class", y = "Numerber of Logs", fill = "Genotypes") +
  facet_wrap(~MATERIAL) + 
  scale_fill_grey(start=0.8, end=0.2) +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  theme_bw()

# out <- "E:\\OneDrive\\R\\Durall\\MScThesis_MEDFOR_UVa\\4-THESIS\\4.1-DRAFTS\\01 - Graphics\\"
# dev.copy(jpeg , paste0(out, 'Taper - 01 - Number of logs 2020', '.jpg'),
#          width = 563, # The width of the plot in inches
#          height = 473) # The height of the plot in inches
# dev.off()

ggplot(DB_TAPER_2020, aes(x = Classe, y = v_ff_tapper, fill = MATERIAL)) + 
  geom_col() + 
  labs(title = "Total volume per diameter class", x = "Diameter Class", y = expression("Volume"~(m^{3})), fill = "Genotypes") +
  facet_wrap(~MATERIAL) + 
  scale_fill_grey(start=0.8, end=0.2) +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  theme_bw()

# dev.copy(jpeg , paste0(out, 'Taper - 02 - Total volume 2020', '.jpg'),
#          width = 563, # The width of the plot in inches
#          height = 473) # The height of the plot in inches
# dev.off()


DB_TAPER_2020 <- left_join(DB_TAPER_2020, RESUMO_PRICE[,c("Classe", "Price_MEAN")], by = "Classe")


######################################################################
#### Processing the Volume by Plot ####
# Summary of plots = with all the data (dead and live)
summary_inventory <- DB_TAPER_2020 %>% 
  filter(QUALITY_COD != 99) %>% 
  group_by(ID, YEAR, AU, STAND, PLOT, MATERIAL, Classe) %>% 
  summarise(Vol = sum(v_ff_tapper, na.rm = TRUE), # Volume for all the trees // fator de forma 0.46
            N = n(),
            Price = mean(Price_MEAN, na.rm = TRUE))


# Adding area to the summary
summary_inventory <- left_join(summary_inventory, STAND[c("ID", "AREA_2020")], by = "ID")
names(summary_inventory)[11] <- "AREA"

AD_Taper <- summary_inventory %>% 
  group_by(AU, STAND, Classe) %>% 
  summarise(
    STAND_Area = mean(AREA, na.rm = T),
    Plot_N = (mean(STAND_Area, na.rm = T)*10000)/AP,
    Plot_MEAN = mean(Vol, na.rm = T),
    Vol_Total = Plot_MEAN * Plot_N,
    Vol_ha = Vol_Total / mean(AREA, na.rm = T),
    Value = mean(Price, na.rm = T),
    GrossValue = Value * Vol_Total)

AD_Total <- AD_Taper %>%  group_by(Classe) %>% summarise(Vol_2020 = sum(Vol_Total), GrossValue = sum(GrossValue))

# Summary of plots = with all the data (dead and live)
summary_inventory <- DB_TAPER_2020 %>% 
  filter(QUALITY_COD == 0) %>% 
  group_by(ID, YEAR, AU, STAND, PLOT, Classe) %>% 
  summarise(Vol = sum(v_ff_tapper, na.rm = TRUE), # Volume for all the trees // fator de forma 0.46
            N = n(),
            Price = mean(Price_MEAN, na.rm = TRUE))


# Adding area to the summary
summary_inventory <- left_join(summary_inventory, STAND[c("ID", "AREA_2020")], by = "ID")
names(summary_inventory)[10] <- "AREA"

AD_Taper <- summary_inventory %>% 
  group_by(AU, STAND, Classe) %>% 
  summarise(
    STAND_Area = mean(AREA),
    Plot_N = (mean(STAND_Area)*10000)/AP,
    Plot_MEAN = mean(Vol),
    Vol_Total = Plot_MEAN * Plot_N,
    Vol_ha = Vol_Total / mean(AREA),
    Value = mean(Price),
    GrossValue = Value * Vol_Total)

AD_Total <- cbind(AD_Total, (AD_Taper %>%  group_by(Classe) %>% summarise(Vol_live = sum(Vol_Total), LiveValue = sum(GrossValue)))[,2:3])

# Summary of plots = with all the data (dead and live)
summary_inventory <- DB_TAPER_2020 %>% 
  filter(QUALITY_COD == 1) %>% 
  group_by(ID, YEAR, AU, STAND, PLOT, Classe) %>% 
  summarise(Vol = sum(v_ff_tapper, na.rm = TRUE), # Volume for all the trees // fator de forma 0.46
            N = n(),
            Price = mean(Price_MEAN, na.rm = TRUE))


# Adding area to the summary
summary_inventory <- left_join(summary_inventory, STAND[c("ID", "AREA_2020")], by = "ID")
names(summary_inventory)[10] <- "AREA"


AD_Taper <- summary_inventory %>% 
  group_by(AU, STAND, Classe) %>% 
  summarise(
    STAND_Area = mean(AREA),
    Plot_N = (mean(STAND_Area)*10000)/AP,
    Plot_MEAN = mean(Vol),
    Vol_Total = Plot_MEAN * Plot_N,
    Vol_ha = Vol_Total / mean(AREA),
    Value = mean(Price),
    GrossValue = Value * Vol_Total)

AD_Total <- left_join(AD_Total, (AD_Taper %>%  group_by(Classe) %>% summarise(Vol_Dead = sum(Vol_Total), DeadValue = sum(GrossValue))), by = "Classe") 


view(AD_Total)

# Adding current density and material
AD_Taper <- left_join(AD_Taper, (AD %>% filter(YEAR == 2020) %>% dplyr::select(AU, STAND, TREE_HA)), by = c("AU", "STAND"))
AD_Taper <- left_join(AD_Taper, STAND[,c("AU", "STAND", "MATERIAL")], by = c("AU", "STAND"))

# Creating class of Tree / ha
AD_Taper$Tree_ha_Class[AD_Taper$TREE_HA <= 250] <- "0-250"
AD_Taper$Tree_ha_Class[AD_Taper$TREE_HA > 250 & AD_Taper$TREE_HA <= 500 ] <- "250-500"
AD_Taper$Tree_ha_Class[AD_Taper$TREE_HA > 500 & AD_Taper$TREE_HA <= 750 ] <- "500-750"
AD_Taper$Tree_ha_Class[AD_Taper$TREE_HA > 750 & AD_Taper$TREE_HA <= 1000 ] <- "750-1000"
AD_Taper$Tree_ha_Class[AD_Taper$TREE_HA > 1000 & AD_Taper$TREE_HA <= 1200 ] <- "1000-1250"
AD_Taper$Tree_ha_Class[AD_Taper$TREE_HA > 1200 & AD_Taper$TREE_HA <= 1500 ] <- "1250-1500"

TAPER_DENSITY <- AD_Taper %>% group_by(Classe) %>% 
  summarise("0-250" = sum(Vol_Total[Tree_ha_Class == "0-250"]),
            "250-500" = sum(Vol_Total[Tree_ha_Class == "250-500"]),
            "500-750" = sum(Vol_Total[Tree_ha_Class == "500-750"]),
            "750-1000" = sum(Vol_Total[Tree_ha_Class == "750-1000"]),
            "1000-1250" = sum(Vol_Total[Tree_ha_Class == "1000-1250"]),
            "1250-1500" = sum(Vol_Total[Tree_ha_Class == "1250-1500"]))

TAPER_Material <- AD_Taper %>% group_by(Classe) %>% 
  summarise("APS F2" = sum(Vol_Total[MATERIAL == "APS F2"]),
            "H13" = sum(Vol_Total[MATERIAL == "H13"]),
            "I 144" = sum(Vol_Total[MATERIAL == "I 144"]),
            "1277" = sum(Vol_Total[MATERIAL == "1277"]))



# out_dir <- file.path("E:\\OneDrive\\R\\Durall\\MScThesis_MEDFOR_UVa\\4-THESIS\\4.1-DRAFTS\\01 - Graphics\\")
# 
# write.xlsx2(x = as.data.frame(AD_Total),
#             file = file.path(out_dir, "Tapper.xlsx"),
#             sheetName = "2020",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE,
#             showNA = TRUE)

write.xlsx2(x = as.data.frame(TAPER_Material),
            file = file.path(out_dir, "Tapper_material.xlsx"),
            append = TRUE,
            showNA = TRUE)
