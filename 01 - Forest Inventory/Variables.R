# Coping the inventory data to RESUMO
RESUMO <- inventory

# Adding the tree density
RESUMO <- add_column(RESUMO,TREE_HA = left_join(RESUMO, Tree, by = c("AU", "STAND", "YEAR"))[,"TREE_HA"],.before = NULL)

# Creating class of Tree / ha
RESUMO$Tree_ha_Class[RESUMO$TREE_HA <= 250] <- "0-250"
RESUMO$Tree_ha_Class[RESUMO$TREE_HA > 250 & RESUMO$TREE_HA <= 500 ] <- "250-500"
RESUMO$Tree_ha_Class[RESUMO$TREE_HA > 500 & RESUMO$TREE_HA <= 750 ] <- "500-750"
RESUMO$Tree_ha_Class[RESUMO$TREE_HA > 750 & RESUMO$TREE_HA <= 1000 ] <- "750-1000"
RESUMO$Tree_ha_Class[RESUMO$TREE_HA > 1000 & RESUMO$TREE_HA <= 1200 ] <- "1000-1250"
RESUMO$Tree_ha_Class[RESUMO$TREE_HA > 1200 & RESUMO$TREE_HA <= 1500 ] <- "1250-1500"

# Annual increment btw 2020 and 2019
# Spliting the data by year
AI_2019 <- inventory %>% filter(YEAR == 2019) %>% dplyr::select("ID", "YEAR", "AU", "STAND", "PLOT", "LINE", "TREE", "DBH", "Hp", "AGE", "v_ff")
AI_2020 <- inventory %>% filter(YEAR == 2020) %>% dplyr::select("ID", "YEAR", "AU", "STAND", "PLOT", "LINE", "TREE", "DBH", "Hp", "AGE", "v_ff")

# Merging the data side by side
AI <- merge(x = AI_2019,
            y = AI_2020,
            by = c("ID" , "AU", "STAND", "PLOT", "LINE", "TREE"),
            suffixes = c("_2019", "_2020"))

# Calculating the annual increment between 2019 and 2020
AI$AID <- with(AI, (DBH_2020-DBH_2019)/(AGE_2020-AGE_2019)*365)
AI$AIH <- with(AI, (Hp_2020-Hp_2019)/(AGE_2020-AGE_2019)*365)
AI$AIV <- with(AI, (v_ff_2020-v_ff_2019)/(AGE_2020-AGE_2019)*365)

# Filtering the columns
AI <- AI %>% select("ID", "AU", "STAND", "PLOT", "LINE", "TREE", 'AID', 'AIH', "AIV")
AI$YEAR <- as.character(2020)

# Joing the AID and AIH
RESUMO <- left_join(RESUMO, AI, by = c("ID", "YEAR", "AU", "STAND", "PLOT", "LINE", "TREE"))

# Setting order
RESUMO$MATERIAL <- factor(RESUMO$MATERIAL, levels = c("APS F2", "H13", "I 144", "1277"))
RESUMO$GENETICA <- factor(RESUMO$GENETICA, levels = c("E. urograndis", "E. urophylla", "Hibrido E. urophylla x E. grandis", "Híbrido de E. camaldulensis x E. grandis"))
RESUMO$Tree_ha_Class <- factor(RESUMO$Tree_ha_Class, levels = c("0-250", "250-500", "500-750", "750-1000", "1000-1250", "1250-1500"))

RESUMO %>% group_by(STAND) %>% summarise(AID = range(AID, na.rm = T), AIH = range(AIH, na.rm = T), AIV = range(AIV, na.rm = T)) %>% head(20)

################################################################################
# By Genetica
################################################################################
VARIABLES_GENETICA <-
RESUMO %>% group_by(MATERIAL) %>% 
  summarise(
    # Counting the trees
    Alive_T_2019 = sum(QUALITY_COD == 0 & YEAR == 2019),
    Dead_T_2019 = sum(QUALITY_COD == 1 & YEAR == 2019),
    Alive_T_2020 = sum(QUALITY_COD == 0 & YEAR == 2020),
    Dead_T_2020 = sum(QUALITY_COD == 1 & YEAR == 2020),
    # DBH measurements 
    M_DBH_2019 = mean(aid[YEAR == 2019], na.rm = T),
    SD_DBH_2019 = sd(aid[YEAR == 2019], na.rm = T),
    M_DBH_2020 = mean(aid[YEAR == 2020], na.rm = T),
    SD_DBH_2020 = sd(aid[YEAR == 2020], na.rm = T),
    DBH_btw_m = mean(AID, na.rm = T),
    DBH_btw_sd = sd(AID, na.rm = T),
    # Height measurements
    M_Height_2019 = mean(aih[YEAR == 2019], na.rm = T),
    SD_Height_2019 = sd(aih[YEAR == 2019], na.rm = T),
    M_Height_2020 = mean(aih[YEAR == 2020], na.rm = T),
    SD_Height_2020 = sd(aih[YEAR == 2020], na.rm = T),
    Height_btw_m = mean(AIH, na.rm = T),
    Height_btw_sd = mean(AIH, na.rm = T),
    # Volume
    V_2019 = sum(v_ff[YEAR == 2019], na.rm = T),
    V_2019_al = sum(v_ff[YEAR == 2019 & QUALITY_COD == 0], na.rm = T),
    V_2019_dead = sum(v_ff[YEAR == 2019 & QUALITY_COD == 1], na.rm = T),
    V_2020 = sum(v_ff[YEAR == 2020], na.rm = T),
    V_2020_al = sum(v_ff[YEAR == 2020 & QUALITY_COD == 0], na.rm = T),
    V_2020_dead = sum(v_ff[YEAR == 2020 & QUALITY_COD == 1], na.rm = T),
  )

out_dir <- file.path("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/1-DATA/1.2-PROCESSED/3-INVENTORY")

# write.xlsx2(x = VARIABLES_GENETICA,
#             file = file.path(out_dir, "VARIABLES.xlsx"),
#             sheetName = "GENETICA",
#             append = TRUE,
#             showNA = FALSE)

################################################################################
# By Density
################################################################################

VARIABLES_TREE_HA <-
  RESUMO %>% filter(!is.na(Tree_ha_Class)) %>% 
  group_by(Tree_ha_Class) %>% 
  summarise(
    # Counting the trees
    Alive_T_2019 = sum(QUALITY_COD == 0 & YEAR == 2019),
    Dead_T_2019 = sum(QUALITY_COD == 1 & YEAR == 2019),
    Alive_T_2020 = sum(QUALITY_COD == 0 & YEAR == 2020),
    Dead_T_2020 = sum(QUALITY_COD == 1 & YEAR == 2020),
    # DBH measurements 
    M_DBH_2019 = mean(aid[YEAR == 2019], na.rm = T),
    SD_DBH_2019 = sd(aid[YEAR == 2019], na.rm = T),
    M_DBH_2020 = mean(aid[YEAR == 2020], na.rm = T),
    SD_DBH_2020 = sd(aid[YEAR == 2020], na.rm = T),
    DBH_btw_m = mean(AID, na.rm = T),
    DBH_btw_sd = sd(AID, na.rm = T),
    # Height measurements
    M_Height_2019 = mean(aih[YEAR == 2019], na.rm = T),
    SD_Height_2019 = sd(aih[YEAR == 2019], na.rm = T),
    M_Height_2020 = mean(aih[YEAR == 2020], na.rm = T),
    SD_Height_2020 = sd(aih[YEAR == 2020], na.rm = T),
    Height_btw_m = mean(AIH, na.rm = T),
    Height_btw_sd = mean(AIH, na.rm = T),
    # Volume
    V_2019 = sum(v_ff[YEAR == 2019], na.rm = T),
    V_2019_al = sum(v_ff[YEAR == 2019 & QUALITY_COD == 0], na.rm = T),
    V_2019_dead = sum(v_ff[YEAR == 2019 & QUALITY_COD == 1], na.rm = T),
    V_2020 = sum(v_ff[YEAR == 2020], na.rm = T),
    V_2020_al = sum(v_ff[YEAR == 2020 & QUALITY_COD == 0], na.rm = T),
    V_2020_dead = sum(v_ff[YEAR == 2020 & QUALITY_COD == 1], na.rm = T),
  )

out_dir <- file.path("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/1-DATA/1.2-PROCESSED/3-INVENTORY")

# write.xlsx2(x = VARIABLES_TREE_HA,
#             file = file.path(out_dir, "VARIABLES.xlsx"),
#             sheetName = "DENSITY",
#             append = TRUE,
#             showNA = FALSE)

################################################################################
# By Genetica and Density
################################################################################

VARIABLES <-
  RESUMO %>% filter(!is.na(Tree_ha_Class)) %>% 
  group_by(GENETICA, Tree_ha_Class) %>% 
  summarise(
    # Counting the trees
    Alive_T_2019 = sum(QUALITY_COD == 0 & YEAR == 2019),
    Dead_T_2019 = sum(QUALITY_COD == 1 & YEAR == 2019),
    Alive_T_2020 = sum(QUALITY_COD == 0 & YEAR == 2020),
    Dead_T_2020 = sum(QUALITY_COD == 1 & YEAR == 2020),
    # DBH measurements 
    M_DBH_2019 = mean(aid[YEAR == 2019], na.rm = T),
    SD_DBH_2019 = sd(aid[YEAR == 2019], na.rm = T),
    M_DBH_2020 = mean(aid[YEAR == 2020], na.rm = T),
    SD_DBH_2020 = sd(aid[YEAR == 2020], na.rm = T),
    DBH_btw_m = mean(AID, na.rm = T),
    DBH_btw_sd = sd(AID, na.rm = T),
    # Height measurements
    M_Height_2019 = mean(aih[YEAR == 2019], na.rm = T),
    SD_Height_2019 = sd(aih[YEAR == 2019], na.rm = T),
    M_Height_2020 = mean(aih[YEAR == 2020], na.rm = T),
    SD_Height_2020 = sd(aih[YEAR == 2020], na.rm = T),
    Height_btw_m = mean(AIH, na.rm = T),
    Height_btw_sd = mean(AIH, na.rm = T),
    # Volume
    V_2019 = sum(v_ff[YEAR == 2019], na.rm = T),
    V_2019_al = sum(v_ff[YEAR == 2019 & QUALITY_COD == 0], na.rm = T),
    V_2019_dead = sum(v_ff[YEAR == 2019 & QUALITY_COD == 1], na.rm = T),
    V_2020 = sum(v_ff[YEAR == 2020], na.rm = T),
    V_2020_al = sum(v_ff[YEAR == 2020 & QUALITY_COD == 0], na.rm = T),
    V_2020_dead = sum(v_ff[YEAR == 2020 & QUALITY_COD == 1], na.rm = T),
  )

out_dir <- file.path("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/1-DATA/1.2-PROCESSED/3-INVENTORY")

# write.xlsx2(x = as.data.frame(VARIABLES),
#             file = file.path(out_dir, "VARIABLES.xlsx"),
#             sheetName = "GEN_DENSITY",
#             append = TRUE,
#             showNA = FALSE)

################################################################################
# By Age
################################################################################

RESUMO$AGE_Y <- as.integer(RESUMO$AGE/365)

VARIABLE_AGE <-
  RESUMO %>% filter(!is.na(Tree_ha_Class)) %>% 
  group_by(AGE_Y) %>% 
  summarise(
    # Counting the trees
    Alive_T_2019 = sum(QUALITY_COD == 0 & YEAR == 2019),
    Dead_T_2019 = sum(QUALITY_COD == 1 & YEAR == 2019),
    Alive_T_2020 = sum(QUALITY_COD == 0 & YEAR == 2020),
    Dead_T_2020 = sum(QUALITY_COD == 1 & YEAR == 2020),
    # DBH measurements 
    M_DBH_2019 = mean(aid[YEAR == 2019], na.rm = T),
    SD_DBH_2019 = sd(aid[YEAR == 2019], na.rm = T),
    M_DBH_2020 = mean(aid[YEAR == 2020], na.rm = T),
    SD_DBH_2020 = sd(aid[YEAR == 2020], na.rm = T),
    DBH_btw_m = mean(AID, na.rm = T),
    DBH_btw_sd = sd(AID, na.rm = T),
    # Height measurements
    M_Height_2019 = mean(aih[YEAR == 2019], na.rm = T),
    SD_Height_2019 = sd(aih[YEAR == 2019], na.rm = T),
    M_Height_2020 = mean(aih[YEAR == 2020], na.rm = T),
    SD_Height_2020 = sd(aih[YEAR == 2020], na.rm = T),
    Height_btw_m = mean(AIH, na.rm = T),
    Height_btw_sd = mean(AIH, na.rm = T),
    # Volume
    V_2019 = sum(v_ff[YEAR == 2019], na.rm = T),
    V_2019_al = sum(v_ff[YEAR == 2019 & QUALITY_COD == 0], na.rm = T),
    V_2019_dead = sum(v_ff[YEAR == 2019 & QUALITY_COD == 1], na.rm = T),
    V_2020 = sum(v_ff[YEAR == 2020], na.rm = T),
    V_2020_al = sum(v_ff[YEAR == 2020 & QUALITY_COD == 0], na.rm = T),
    V_2020_dead = sum(v_ff[YEAR == 2020 & QUALITY_COD == 1], na.rm = T),
  )

out_dir <- file.path("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/1-DATA/1.2-PROCESSED/3-INVENTORY")

# write.xlsx2(x = as.data.frame(VARIABLE_AGE),
#             file = file.path(out_dir, "VARIABLES.xlsx"),
#             sheetName = "AGE",
#             append = TRUE,
#             showNA = FALSE)

# Diameter
AID <- INV_GRAPH %>% filter(YEAR == 2019, AU != 10) %>% 
  ggplot(aes(x = MATERIAL, y = aid, na.rm = TRUE)) +
  geom_boxplot(na.rm = TRUE) + 
  #ylim(0, 6) + 
  labs(x = NULL,y = 'Diameter increment (cm)') + 
  stat_summary(data = RESUMO %>% filter(YEAR == 2020) %>% group_by(MATERIAL) %>% summarise(IPA_2020 = mean(AID, na.rm = T)),
               fun = mean, 
               geom = "point", 
               aes(x = MATERIAL, y = IPA_2020), color = "red", size = 3, shape = 18) +
  theme_bw(base_size = 14)
AID

# Height
AIH <- INV_GRAPH %>% filter(YEAR == 2019, AU != 10) %>% 
  ggplot(aes(x = MATERIAL, y = aih, na.rm = TRUE)) +
  geom_boxplot(na.rm = TRUE) +
  #ylim(0, 6) + 
  labs(x = "Clones", y = 'Height increment (m)') +
  stat_summary(data = RESUMO %>% filter(YEAR == 2020) %>% group_by(MATERIAL) %>% summarise(IPA_2020 = mean(AIH, na.rm = T)),
               fun = mean, 
               geom = "point", 
               aes(x = MATERIAL, y = IPA_2020), color = "red", size = 3, shape = 18) +
  theme_bw(base_size = 14)
AIH

# Volume
IMA_2019 <- INV_GRAPH %>% 
  filter(YEAR == 2019, AU != 10) %>% 
  group_by(AU, STAND, PLOT, MATERIAL) %>% 
  summarise(Vol_2019 = ((sum(v_ff, na.rm = T)/AP)*10000),
            Idade = mean(AGE_Y),
            IMA_2019 = Vol_2019/Idade)

IMA_2020 <- INV_GRAPH %>% 
  filter(YEAR == 2020, AU != 10) %>% 
  group_by(AU, STAND, PLOT, MATERIAL) %>% 
  summarise(Vol_2020 = ((sum(v_ff, na.rm = T)/AP)*10000),
            Idade = mean(AGE_Y),
            IMA_2020 = Vol_2020/Idade)

IMA <- merge(IMA_2019[,c("AU", "STAND", "PLOT", "MATERIAL", "Vol_2019", "IMA_2019")], 
             IMA_2020[,c("AU", "STAND", "PLOT", "MATERIAL", "Vol_2020", "IMA_2020")], 
             by = c("AU", "STAND", "PLOT", "MATERIAL"))

IMA$IPA <- with(IMA, Vol_2020 - Vol_2019)


AIV <- IMA %>% 
  ggplot(aes(x = MATERIAL, y = IMA_2019, na.rm = TRUE)) +
  geom_boxplot(na.rm = TRUE) +
  #ylim(0, 6) + 
  labs(x = NULL, y = expression('Volume ' (m^3))) +
  stat_summary(data = IMA %>% filter(IPA > 0),
               fun = mean, 
               geom = "point", 
               aes(x = MATERIAL, y = IPA), color = "red", size = 3, shape = 18) +
  theme_bw(base_size = 14)
AIV



# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
library(ggpubr)

ggarrange(AID, AIH, AIV,
          labels = c("A", "B", "C"),
          label.x = 0,
          align = "h",
          ncol = 3, nrow = 1)

out <- "E:\\OneDrive\\R\\Durall\\MScThesis_MEDFOR_UVa\\4-THESIS\\4.1-DRAFTS\\01 - Graphics\\"
dev.copy(jpeg , paste0(out, 'AI - 01 - Annual Increment by Material - 2', '.jpg'),
         width = 820, # The width of the plot in inches
         height = 490) # The height of the plot in inches
dev.off()
