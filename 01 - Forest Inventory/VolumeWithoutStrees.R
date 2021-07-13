# Creating a new database to work with
VOL_DEAD <- inventory %>% dplyr::select(ID, YEAR, AU, STAND, PLOT, QUALITY_COD, DBH, Hp, aid, aih, AGE) %>% 
  filter(YEAR == 2019)

# Adding the annual increments to the measurements
VOL_DEAD$DBH_2019_ai <- c(VOL_DEAD$DBH + VOL_DEAD$aid)
VOL_DEAD$Hp_2019_ai  <- c(VOL_DEAD$Hp + VOL_DEAD$aih)

# New basal area
VOL_DEAD$g_2019_ai <- c((pi/4)*(VOL_DEAD$DBH_2019_ai/100)^2)                          # Basal area by tree

# New Volume withou drought stress
VOL_DEAD$v_ff <- c(VOL_DEAD$g_2019_ai * VOL_DEAD$Hp_2019_ai * 0.46)

#### Processing the Volume by Plot ####
# Summary of plots
summary_inventory_ai <- VOL_DEAD %>% 
  filter(QUALITY_COD != 99) %>% 
  group_by(ID, YEAR, AU, STAND, PLOT) %>% 
  summarise(Vol = sum(v_ff, na.rm = TRUE), # Volume for all the trees // fator de forma 0.46
            N = n(),
            TREE_HA = ((N/AP)*10000),
            TREE_AGE = mean(AGE)) %>%  
  filter(Vol > 0)


# Adding area to the summary
summary_inventory_ai <- left_join(summary_inventory_ai, STAND[c("ID", AREA = "AREA_2019")], by = "ID")
names(summary_inventory_ai)[10] <- "AREA"

head(inventory %>% filter(YEAR == 2019, PLOT == 125) %>% select(DBH, Hp, aid)) # %>%  group_by(AU, PLOT) %>%  summarise(VOL = sum(v_ff, na.rm = T))
head(VOL_DEAD %>% filter(YEAR == 2019, PLOT == 125) %>% select(DBH, Hp, aid, DBH_2019_ai, AGE)) # %>%  group_by(AU, PLOT) %>%  summarise(VOL = sum(v_ff, na.rm = T))

# Analytical Data 
AD_ai <- summary_inventory_ai %>%
  group_by(AU, STAND, YEAR) %>% 
  summarise(
    TREE_HA = mean(TREE_HA), 
    TREE_AGE = as.integer(mean(TREE_AGE)/365),
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

RESUMO_AI <- right_join(AD[,c('AU', "STAND", "YEAR", "TREE_HA", "Vol_ha", "Vol_Total")],
                        AD_ai[,c("AU", "STAND", "YEAR", "TREE_HA", "TREE_AGE" , "Vol_ha", "Vol_Total")],
                        by = c("AU", "STAND", "YEAR"),
                        suffix = c("_2019", "_AI"))

RESUMO_AI$Volume_ai <- RESUMO_AI$Vol_Total_AI - RESUMO_AI$Vol_Total_2019

RESUMO_AI <- left_join(x = RESUMO_AI,
                       y = AD %>% filter(YEAR == 2020) %>% dplyr::select(AU, STAND, TREE_HA, Vol_ha, Vol_Total),
                       by = c("AU", "STAND"),
                       suffix = c(x = "", y = "_2020"))

T1 <- data.frame(AU = NULL, STAND = NULL, TREE_AGE = NULL, Vol_HA = NULL, Group = NULL)
T1 <- rbind(T1, as.data.frame(c(RESUMO_AI %>% dplyr::select(AU, STAND, TREE_AGE, Vol_HA =  Vol_ha_2019), GROUP =  "Observed in 2019")))
T1 <- rbind(T1, as.data.frame(c(RESUMO_AI %>% dplyr::select(AU, STAND, TREE_AGE, Vol_HA =  Vol_ha_AI),   GROUP =  "No-drought")))
T1 <- rbind(T1, as.data.frame(c(RESUMO_AI %>% dplyr::select(AU, STAND, TREE_AGE, Vol_HA =  Vol_ha), GROUP =  "Observed in 2020")))

ggplot(T1, aes(x = TREE_AGE, y = Vol_HA, fill = GROUP)) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(title = "Difference in volume per hectare, without drought occurrence", x = "Rotation age", 
       y =  expression('Volume'~(m^{3})), fill = "Volume:") +
  scale_x_continuous(breaks=seq(1, 12, 1), labels = waiver()) + 
  theme_bw()


# Tapper function
# schopfer
schopfer <- nls(di~dap*(b0+b1*(t)+b2*(t^2)+b3*(t^3)+b4*(t^4)+b5*(t^5)),
                start = list(b0=0.1,b1=0.1,b2=0.1,b3=0.1,b4=0.1,b5=0.1),
                data=dados)


schopfer <-  DBH*(B0 + B1*(t) + B2*(t^2) + B3*(t^3) + B4*(t^4) + B5*(t^5))
