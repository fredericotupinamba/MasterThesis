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
packs <- c("RStoolbox", "readxl", "forestmangr",
           "maps", "ggplotAssist", "shinyjs", 
           "ggplot2", "ggthemes", "mondate", "lubridate",
           "mod", "minpack.lm", "tidyverse", "lmfor", 
           "deSolve", "vegan", "measures", "sqldf", "xlsx", "forestmangr")

install(packs)


# Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

######################################################################
#### BASICS DEFINITIONS ####
# Defining the path
getwd()
setwd("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/")
out_dir <- file.path("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/1-DATA/1.2-PROCESSED/3-INVENTORY")

# # Variables
AT <- 1402.04 * 10000 # Total Area in square meters
AP <- base::pi * 18^2 # Plot area

#### Reading Data ####
#### READING DATA FROM THE STANDS ####
# Parcel
STAND <- read_excel("1-DATA/1.1-RAW/1-INVENTORY/STAND.xlsx", 
                    sheet =  "Dados",
                    col_names = TRUE,
                    col_types = c("skip", 
                                  "text", 
                                  "text",
                                  "numeric",
                                  "numeric",
                                  "text",
                                  "text",
                                  "skip", 
                                  "skip",
                                  "skip",
                                  "numeric",
                                  "date",
                                  "skip",
                                  "skip"))


STAND <- add_column(STAND, ID = paste(STAND$AU, STAND$STAND, sep = "-"), .before = "AU")

STAND$ID <- as.factor(STAND$ID)
STAND$AU <- as.factor(STAND$AU)
STAND$STAND <- as.factor(STAND$STAND)
STAND$MATERIAL <- as.factor(STAND$MATERIAL)
STAND$GENETICA <- as.factor(STAND$GENETICA)
#levels(STAND$GENETICA) <- gsub(" x ", "\n", levels(STAND$GENETICA)) # setting the GENETICA name for GGPLOT

#### READING DATA FROM THE PLOTS ####
# Plot
Plot <- read_excel("1-DATA/1.1-RAW/1-INVENTORY/Table_Plots.xlsx", 
                   sheet = 1,
                   col_names = TRUE)

Plot <- add_column(Plot, ID = paste(Plot$AU, Plot$STAND, sep = "-"), .before = "AU") # setting the ID to match with inventory stands id

Plot$ID <- as.factor(Plot$ID)
Plot$AU <- as.factor(Plot$AU)
Plot$STAND <- as.factor(Plot$STAND)
Plot$PLOT <- as.factor(Plot$PLOT) 

# Inventory Data 2019
dados2019 <- read_excel("1-DATA/1.1-RAW/1-INVENTORY/Inventario_Geral_2019.xlsx", 
                        sheet = 1,
                        col_names = TRUE,
                        col_types = c("numeric", rep("text", 7), rep("numeric", 3)))

dados2019 <- add_column(dados2019, ID = paste(dados2019$AU, dados2019$STAND, sep = "-"), .before = "YEAR")

dados2019$INVENTORY_DATE <- c(as.POSIXct("2019-04-01"))        # setting the inventory date

# Inventory Data 2020
dados2020 <- read_excel("1-DATA/1.1-RAW/1-INVENTORY/Inventario_Geral_2020.xlsx", 
                        sheet = 1,
                        col_names = TRUE,
                        col_types = c(rep("text", 8), rep("numeric", 3)))


dados2020 <- add_column(dados2020, ID = paste(dados2020$AU, dados2020$STAND, sep = "-"), .before = "YEAR")

dados2020$INVENTORY_DATE <- c(as.POSIXct("2020-06-01"))        # setting the inventory date

# FULL DATA
inventory <- as.data.frame(rbind(dados2019, dados2020))

# Annual Unit as Factor level
inventory$AU <- factor(inventory$AU, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

######################################################################
# Tree Quality
inventory$QUALITY_COD <- NA
inventory$QUALITY_COD[inventory$QUALITY %in% c("N", "ND", "n")] <- 0    # 0 = Normal
inventory$QUALITY_COD[inventory$QUALITY %in% c("A", "G", "S", "J", "B", "AJ", "GA", "BJ", "AD", "JA", "JG", "QJ", "JQ", "SB")] <- 0 # 0 = Live but injured
inventory$QUALITY_COD[inventory$QUALITY %in% c("M", "Q", "F", "MQ")] <- 1 # 0 = Dead
inventory$QUALITY_COD[is.na(inventory$QUALITY)] <- 99     # Harvested

summary(as.factor(inventory$QUALITY_COD))

# Determinando as variaveis categoricas
inventory$ID <- as.factor(inventory$ID)
inventory$STAND <- as.factor(inventory$STAND)
inventory$PLOT <- as.factor(inventory$PLOT)
inventory$LINE <- as.factor(inventory$LINE)
inventory$TREE <- as.factor(inventory$TREE)
inventory$STEM <- as.factor(inventory$STEM)
inventory$QUALITY <- as.factor(inventory$QUALITY)


# Add Variety to the data
inventory <- merge(inventory, STAND[,c("ID", "MATERIAL", "GENETICA")], by = "ID")
#inventory <- merge(inventory, STAND[,c("ID", "GENETICA")], by = "ID")

######################################################################
#### INVENTORY PROCESSING ####
# Calculate the DBH average
inventory$DBH <- c((inventory$DAP1+inventory$DAP2)/2)

# Quadratic Mean Diameter - QMD
dg <- inventory %>% 
  filter(!is.na(DBH)) %>% 
  group_by(YEAR, PLOT) %>% 
  summarise(
    N = n(),
    DG = sqrt(sum(DBH * DBH, na.rm = TRUE) / n())) %>% 
  ungroup()

inventory$DG <- left_join(inventory, dg, by = c("YEAR", "PLOT"))[,"DG"]

# Tree basal area
inventory$g <- (pi/4)*(inventory$DBH/100)^2                          # Basal area by tree

G <- inventory %>%                                                   # Sum of the basal area by plot
    filter(!is.na(DBH)) %>% 
    dplyr::select(YEAR, ID, PLOT, g) %>%
  group_by(YEAR, PLOT) %>% 
  summarise(G = sum(g, na.rm = TRUE) / AP * 10000)                   # Basal area by plot, expressed in m^2/ha

inventory$G <- left_join(inventory, G, by = c("YEAR", "PLOT"))[,"G"]

inventory$GR <- c(with(inventory, g/G))                             # Relatively Basal Area


# Calculating the age of the plantation on the inventory date
inventory$PLANTING <- left_join(inventory, STAND[,c("ID", "DATA PLANTIO")], by = "ID")[,"DATA PLANTIO"]     # Planting data
inventory$AGE <- as.numeric(difftime(inventory$INVENTORY_DATE, inventory$PLANTING,  units = "days"))       # Age in 2019

# Filter Normal and Dominants trees in a new data.frame
inventory_N <- inventory %>% filter(QUALITY_COD == 0)

# Filter trees with measured height in a new data.frame, from "Normal TREEs"
inventory_H <- na.omit(inventory_N[(inventory_N$DBH  > 0 & inventory_N$H > 0), ])

# Dominants Height by Parcel
dominantes <- inventory_H %>% 
  dplyr::select(ID, YEAR, PLOT, DBH, H) %>% 
  group_by(ID, YEAR, PLOT) %>%
  arrange(desc(DBH)) %>%
  slice(1:10) %>%
  arrange(ID) %>% 
  dplyr::summarize(Hd = mean(H))

# Joing the dominats height to the tables
inventory$Hd   <- left_join(inventory, dominantes, by = c("ID", "YEAR", "PLOT"))[,"Hd"]
inventory_N$Hd <- left_join(inventory_N, dominantes, by = c("ID", "YEAR", "PLOT"))[,"Hd"]
inventory_H$Hd <- left_join(inventory_H, dominantes, by = c("ID", "YEAR", "PLOT"))[,"Hd"]

# Annual Increment 
## (Using the Normal and Normal/Dominant trees)
inventory_N$aid <- c(with(inventory_N, (DBH / (AGE/365))))
inventory$aid <- c(with(inventory, (DBH / (AGE/365))))

# Annual height Increment
inventory_H$aih <- c(with(inventory_H, (H / (AGE/365))))

######################################################################
#### HEIGHT CALCULATION ####
# Non Linear Equation, with initialization starters####
# Non-linear regression
Henricksen <- nls(H ~ b0 + b1 * log(DBH),
                  data = inventory_H,
                  start = list(b0 = 2, b1 = 10))

Parabolico <- nls(H ~ b0 + b1 * DBH + b2 * (DBH^2),
                  data = inventory_H, 
                  start = list(b0 = 1.79, b1 = 1.70 , b2 = 1))

Azevedo <- nls(H ~ b0 + b1 * (DBH^2), 
               data = inventory_H, 
               start = list(b0 = 19, b1 = 0.02))

Soares <- nls(H ~ b0 + b1 * (1/(DBH^2)), 
              data = inventory_H, 
              start = list(b0 = 30, b1 = -1800))

Trorey <- nls(H ~b0 + b1 * (1/DBH) + b2 * (1/(DBH^2)), 
              data = inventory_H, 
              start = list(b0 = 40, b1 = -343.5, b2 = 896.79))

Hossfeld1 <- nls(H ~ 1.3 + (b0 * (DBH^2))/(DBH + b1)^2,
                 data = inventory_H, 
                 start = list(b0 = 46.104,b1 = 13.638))

Hossfeld2 <- nls(H ~ 1.37 + (DBH^b0)/(b1 + b2*(DBH^b0)),
                 data = inventory_H, 
                 start = list(b0 = 1.2479,b1 =  1.6736,b2 = 0.022))

Weibull <- nls(H ~ 1.3 + b0*(1 - exp(-b1 * (DBH^b2))),
               data = inventory_H,
               start = list(b0 = 33.5721, b1 = 0.0205, b2 = 1.157))

Champman_Richards <- nls(H ~ 1.3 + b0 * (1 - exp(-b1 * DBH))^b2,
                         data = inventory_H,
                         start = list(b0 = 35.032, b1 = 0.0374, b2 = 1.2075))

Campos <- nls(H ~ exp(b0 - (b1/DBH) + (b2*Hd)), 
             data = inventory_H, 
             start = list(b0 = 1.40095, b1 = 7.678502, b2 = 0.6928211))

# Nome das equações NLS
equacoes <- list(Henricksen = Henricksen, 
                 Parabolico = Parabolico, 
                 Azevedo = Azevedo, 
                 Soares = Soares, 
                 Trorey = Trorey,
                 Hossfeld1 = Hossfeld1, 
                 Hossfeld2 = Hossfeld2,
                 Weibull = Weibull, 
                 Champman_Richards = Champman_Richards,
                 Campos = Campos)

# RMSE - Root Mean Squared Error
Formula <- NULL
Rmse <- NULL
Rmse_a <- NULL
Bias <- NULL
Bias_a <- NULL
RMSE_Results <- NULL
AIC_ <- NULL
COEF_i <- data.frame(b0 = NA, b1 = NA, b2 = NA)
COEF <- NA

for (i in 1:length(equacoes)) {
  fit <- fitted(equacoes[[i]])
  Formula <- as.character(names(equacoes[i]))
  Rmse <- measures::RMSE(inventory_H$H, fit)
  Rmse_a <- sqrt(sum((inventory_H$H - fit)^2)/nrow(inventory_H))/mean(inventory_H$H)
  Bias <- sum(inventory_H$H - fit)/nrow(inventory_H)
  Bias_a <- sum(inventory_H$H - fit)/nrow(inventory_H)/mean(inventory_H$H)
  COEF_i <- c(Formula, coef(equacoes[[i]]))
  RMSE_Results <- rbind(RMSE_Results, data.frame(Formula, Rmse, Rmse_a, Bias, Bias_a))
  COEF <- qpcR:::rbind.na(COEF, COEF_i)
  }

AIC_ <- data.frame(AIC = sapply(equacoes, AIC))
RMSE_Results <- cbind(RMSE_Results, AIC = AIC_)

colnames(COEF) <- c("Formula", "B0", "B1", "B2")
COEF <- as.data.frame(COEF)
RMSE_Results <- left_join(RMSE_Results, COEF, by = "Formula")

# BEST RMS
RMSE_Results <- RMSE_Results[order(RMSE_Results$Rmse), ]
RMSE_Results[1]

  # out_dir <- file.path("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/1-DATA/1.2-PROCESSED/3-INVENTORY")
  # write.xlsx2(x = as.data.frame(RMSE_Results),
  #             file = file.path(out_dir, "RMSE_Height2.xlsx"),
  #             sheetName = "RMSE")

# Graphics - Residuals vs Fitted Plot
inventory_H$Hp <- predict(get(RMSE_Results$Formula[1]), newdata = inventory_H, subset("ID", "YEAR"))
inventory_H$residuals <- residuals(get(RMSE_Results$Formula[1]), newdata = subset(c("ID", "YEAR")))

# Height Prediction for all data
inventory$Hp <- predict(get(RMSE_Results$Formula[1]), newdata = inventory)

# Annual increment for Height
inventory$aih <- c(with(inventory, (Hp / (AGE/365))))

######################################################################
#### VOLUME EQUATION ####
# Volume - using Factor Form 0.46
inventory$v_ff <- c(inventory$g * inventory$Hp * 0.46)

# Importing data from Smalian
FF_trees <- read_excel("1-DATA\\1.1-RAW\\1-INVENTORY\\FF_R.xlsx",
                       sheet = 1,
                       col_names = TRUE,
                       col_types = c(rep("numeric", 41)))

# Changing CBH do DBH
FF_trees[,4:41] <- c(FF_trees[,4:41]/pi)

# Objeto com as distancias medidas
medidas <- read_excel("1-DATA\\1.1-RAW\\1-INVENTORY\\FF_R_MEDIDAS.xlsx",
                      sheet = 1,
                      col_names = TRUE)

# Equações de volume
#remove(FF)
FF <- FF_trees
FF$Ponta <- NA
FF$Vreal <- NA
FF$Vcilindro <- NA
FF$FF_SEMA <- NA
FF$ff <- NA
CAP1 <- NULL
CAP2 <- NULL
Alt1 <- NULL
Alt2 <- NULL

for (i in 1:nrow(FF_trees)) {
  for (j in 5:(rowSums(!is.na(FF_trees[i,])))) {
    CAP1 <- as.numeric(FF_trees[i,j]/100)^2*(4/pi)
    CAP2 <- as.numeric(FF_trees[i,j+1]/100)^2*(4/pi)
    Alt1 <- as.numeric(medidas[1,j])
    Alt2 <- as.numeric(medidas[1,j+1])
    FF[i,j] <- (CAP1+CAP2)/2 * (Alt2 - Alt1)
  }

  FF$Ponta[i] <- CAP1 * (FF$HT[i] - Alt1) * (1/3)
  FF$Vcilindro[i] <- as.numeric((FF_trees[i,4]/100)^2*(4/pi) * FF_trees$HT[i])
  FF$FF_SEMA[i] <- as.numeric((FF_trees[i,4]/100)^2*(4/pi) * FF_trees$HT[i]) * 0.46
  FF$Vreal[i] <- rowSums(FF[i,5:42], na.rm = TRUE, dims = 1)
  FF$ff[i] <- FF$Vreal[i] / FF$Vcilindro[i]
}

mean(FF$ff)
 
#Spurr
EQ_VOL_1 <- nls(Vreal ~ b0 + b1 * (DBH^2)* HT,
             data = FF,
             start = list(b0 = 0.00365, b1 = 0.00003))


#Schumacher_Hall
EQ_VOL_2 <- nls(log(Vreal) ~ b0 + b1*log(DBH) + b2*log(HT),
             data = FF,
             start = list(b0 = -10.028, b1 = 1.7767, b2 = 1.1336))

#Hohenadl_Krenn
EQ_VOL_3 <- nls(Vreal ~ b0 + b1*DBH + b2*(DBH^2),
             data = FF,
             start = list(b0 = -0.1338, b1 = 0.01898, b2 = -0.00009))

# Meyer
EQ_VOL_4 <- nls(Vreal ~ b0 + b1*DBH + b2*(DBH^2) + b3*DBH*HT + b4*(DBH^2)*HT + b5*HT,
              data = FF,
              start = list(b0 = 0.13278, b1 = -0.0222, b2 = 0.000842,
                           b3 = 0.00174, b4 = -0.0003, b5 = -0.0099))

# Kopezky-Gehrhardt
EQ_VOL_5 <- nls(Vreal ~ b0 + b1*(DBH^2),
              data = FF,
              start = list(b0 = -0.0189, b1 = 0.00068))

# Stoate (australiana)
EQ_VOL_6 <- nls(Vreal ~ b0 + b1*(DBH^2) + b2*(DBH^2)*HT + b3*HT,
              data = FF,
              start = list(b0 = -0.0027, b1 = -0.00005, b2 = 0.000036,
                           b3 = 0.00077))

# Spurr (logaritimica)
EQ_VOL_7 <- nls(log(Vreal) ~ b0 + b1*log((DBH^2)*HT),
              data = FF,
              start = list(b0 = -9.9247, b1 = 0.96119))

# Naslund
EQ_VOL_8 <- nls(Vreal ~ b0 + b1*(DBH^2) + b2*(DBH^2)*HT + b3*DBH*(HT^2) + b4*(HT^2),
              data = FF,
              start = list(b0 = 0.00097, b1 = -0.00001, b2 = 0.000032,
                           b3 = 0.00001, b4 = 0.00003))

# Nome das equações NLS
EQ_VOL <- list(EQ_VOL_1 = EQ_VOL_1,
             EQ_VOL_2 = EQ_VOL_2,
             EQ_VOL_3 = EQ_VOL_3,
             EQ_VOL_4 = EQ_VOL_4,
             EQ_VOL_5 = EQ_VOL_5,
             EQ_VOL_6 = EQ_VOL_6,
             EQ_VOL_7 = EQ_VOL_7,
             EQ_VOL_8 = EQ_VOL_8)

# RMSE - Root Mean Squared Erro
FORMULA <- NULL
RMSE <- NULL
RMSE_a <- NULL
BIAS <- NULL
BIAS_a <- NULL
RMSE_Results <- NULL

for (i in 1:length(EQ_VOL)) {
  fit <- fitted(EQ_VOL[[i]])
  FORMULA[i] <- as.character(names(EQ_VOL[i]))
  RMSE[i] <- RMSE(FF$Vreal, fit)
  RMSE_a[i] <- sqrt(sum((FF$Vreal - fit)^2)/nrow(FF))/mean(FF$Vreal)*100
  BIAS[i] <- sum(FF$Vreal - fit)/nrow(FF)
  BIAS_a[i] <- sum(FF$Vreal - fit)/nrow(FF)/mean(FF$Vreal)*100
  RMSE_Results <- rbind(data.frame(FORMULA, RMSE, RMSE_a, BIAS, BIAS_a))
}

# Seleção do melhor Erro Medio
RMSE_Results <- RMSE_Results[order(RMSE_Results$RMSE), ]
RMSE_Results

# Grafico do Residuo X Volume
FF$Vp <- predict(get(RMSE_Results$FORMULA[1]))
FF$residuals <- residuals(get(RMSE_Results$FORMULA[1]))

ggplot(FF, aes(x = Vreal, y = Vp, na.rm = TRUE)) +
  labs(title = "Equação de Volume", x = "Smalian (m3)", y = "Equação (m3)") +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_point(aes(color = abs(residuals))) +
  scale_color_continuous(low = "green", high = "red") +
  theme_solarized()

# Grafico do Residuo X Volume
# ggplot(FF, aes(x = Vreal, y = residuals, na.rm = TRUE)) +
#   labs(title = "Residuo X Volume", x = "Volume (m3)", y = "Volume Residual (m3)") +
#   geom_hline(yintercept = 0, size = 2) +
#   geom_point(size = 2, colour = "black", alpha = 0.5) +
#   geom_point(size = 1, colour = "blue", alpha = 0.2) +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_solarized() + scale_color_solarized()
#   #facet_wrap(~CLONE)

# Volume prediction for all trees
inventory$Vp <- predict(get(RMSE_Results$FORMULA[1]), newdata = inventory)


# V <- inventory %>%                                                   # Sum of the volume by plot
#   select(ID, PLOT, v_ff, Vp) %>%
#   group_by(PLOT) %>%
#   summarise(V_ff = sum(v_ff, na.rm = TRUE) / AP * 10000,
#             V_pred = sum(Vp, na.rm = TRUE) / AP * 10000)                   # Sum of the volume by plot, expressed in m^3/ha
# 
# inventory <- left_join(inventory, V, by = "PLOT")
# 
# inventory$VR <- c(with(inventory, v/V))


######################################################################
#### Processing the Volume by Plot ####
# Summary of plots
summary_inventory <- inventory %>% 
  filter(QUALITY_COD != 99) %>% 
  group_by(ID, YEAR, AU, STAND, PLOT) %>% 
  summarise(Vol = sum(v_ff, na.rm = TRUE), # Volume for all the trees // fator de forma 0.46
            N = n(),
            TREE_HA = ((N/AP)*10000)) %>%  
  filter(Vol > 0)


# Adding area to the summary
summary_inventory_2019 <- summary_inventory %>% filter(YEAR == '2019') %>% left_join(STAND[c("ID", AREA = "AREA_2019")], by = "ID")
summary_inventory_2020 <- summary_inventory %>% filter(YEAR == "2020") %>% left_join(STAND[c("ID", AREA = "AREA_2020")], by = "ID")
names(summary_inventory_2019)[9] <- "AREA"
names(summary_inventory_2020)[9] <- "AREA"
summary_inventory <- rbind(summary_inventory_2019, summary_inventory_2020)

# Analytical Data 
AD <- summary_inventory %>% 
  group_by(AU, STAND, YEAR) %>% 
  summarise(
    TREE_HA = mean(TREE_HA), 
    STAND_Area = mean(AREA),
    Plot_N = (mean(STAND_Area)*10000)/AP,
    Plot_ni = n(),
    Plot_MEAN = mean(Vol),
    Plot_SUM = sum(Vol),
    Plot_Var = var(Vol),
    Plot_SD = sd(Vol),
    Plot_CV = (Plot_SD/Plot_MEAN)*100,
    Plot_E = signif(0.1*Plot_MEAN, 4),
    Plot_f = Plot_ni/Plot_N,
    Plot_FC = 1-Plot_f, 
    Plot_FC2 = if(Plot_FC >= 0.98) {"INFINITA"} else{"FINITA"},
    Plot_t = qt(0.90, df = Plot_ni-1),
    Plot_IA = round((Plot_N * Plot_t^2 * var(Vol))/(Plot_N * Plot_E^2 + Plot_t^2 * var(Vol)), 0), # Intensidade Amostral
    Plot_MV = if(Plot_FC2 == "FINITA"){(Plot_Var / Plot_ni) * Plot_FC} else{(Plot_Var / Plot_ni)}, # Mean Variance
    Plot_MSE = sqrt(Plot_MV),                   # Mean standard error
    Plot_EA = Plot_t * Plot_MSE,                # Erro Absoluto
    Plot_ER = (Plot_EA / Plot_MEAN) * 100,      # Erro Relativo
    Vol_Total = Plot_MEAN * Plot_N,
    Vol_ha = Vol_Total / mean(AREA),
    Plot_LI = Plot_MEAN - Plot_EA,
    Plot_LS = Plot_MEAN + Plot_EA,
    Parcel_LI = Vol_Total - (Plot_N * Plot_EA),
    Parcel_LS = Vol_Total + (Plot_N * Plot_EA))

AD %>% group_by(YEAR) %>% summarise(Vol = sum(Vol_Total))


Tree <- inventory %>%
  filter(QUALITY_COD != 99, !is.na(DBH)) %>% 
  group_by(AU, STAND, YEAR) %>% 
  summarise(
    N = n(),
    Plots = length(unique(PLOT)),
    Height_Min = min(Hp, na.rm = TRUE),
    Height_Mean = mean(Hp, na.rm = TRUE),
    Height_Max = max(Hp, na.rm = TRUE),
    DBH_Min = min(DBH, na.rm = TRUE),
    DBH_Mean = mean(DBH, na.rm = TRUE),
    DBH_Max = max(DBH, na.rm = TRUE),
    DBH_QMD = mean(DG, na.rm = TRUE),
    G = mean(G, na.rm = TRUE),
    AI_DBH = mean(aid, na.rm = TRUE),
    AI_H = mean(aih, na.rm = TRUE),
    TREE_HA = ((N / length(unique(PLOT))) / AP) * 10000) %>%
  ungroup()



# inventory %>%  filter(QUALITY_COD == 0, AU == 10) %>% 
#   group_by(AU, YEAR) %>% summarise(N = n(),
#                                    Plot = length(unique(PLOT)))

# #####################################################################
# ### Exporting data ####
# 
# 
# write.csv(x = as.data.frame(AD),
#            file = file.path(out_dir, "AD_20210219.xlsx"),
#            #sheetName = "Vol_Talhao",
#            #col.names = TRUE,
#            row.names = FALSE,
#            #showNA = TRUE
#           )
# 
# 
# write.csv2(x = inventory,
#           file = file.path(out_dir, "Inventory_data_20210219.csv"),
#           na = "NA")

# write.xlsx2(x = as.data.frame(Tree),
#             file = file.path(out_dir, "TREE_202102191.xlsx"),
#             sheetName = "TREE",
#             col.names = TRUE,
#             row.names = FALSE,
#             showNA = TRUE)

# write.csv2(x = as.data.frame(RESUMO3),
#            file = file.path(out_dir, "RESUMO_2_20210228.csv"),
#            na = "", sep = ";", dec = ",",
#            row.names = FALSE)

# # Exporting Image
# save.image()




