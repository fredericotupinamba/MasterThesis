Table1 <- 
inventory %>% group_by(YEAR) %>% 
  summarise(# AGE
            AGE_min = min(AGE/365),
            AGE_mean = mean(AGE/365),
            AGE_max = max(AGE/365),
            AGE_SD = sd(AGE/365, na.rm = T),
            # DBH
            DBH_min = min(DBH, na.rm = T), 
            DBH_mean = mean(DBH, na.rm = T),
            DBH_max = max(DBH, na.rm = T),
            DBH_SD = sd(DBH, na.rm = T),
            # H
            H_min = min(Hp, na.rm = T),
            H_mean = mean(Hp, na.rm = T),
            H_max = max(Hp, na.rm = T),
            H_SD = sd(Hp, na.rm = T),
            # Hd
            Hd_min = min(Hd, na.rm = T),
            Hd_mean = mean(Hd, na.rm = T),
            Hd_max = max(Hd, na.rm = T),
            Hd_SD = sd(Hd, na.rm = T),
            # G
            G_min = min(G, na.rm = T),
            G_mean = mean(G, na.rm = T),
            G_max = max(G, na.rm = T),
            G_SD = sd(G, na.rm = T),
            # DG
            DG_min = min(DG, na.rm = T),
            DG_mean = mean(DG, na.rm = T),
            DG_max = max(DG, na.rm = T),
            DG_SD = sd(DG, na.rm = T),
            # AID
            AID_min = min(aid, na.rm = T),
            AID_mean = mean(aid, na.rm = T),
            AID_max = max(aid, na.rm = T),
            AID_SD = sd(aid, na.rm = T),
            # AIH
            AIH_min = min(aih, na.rm = T),
            AIH_mean = mean(aih, na.rm = T),
            AIH_max = max(aih, na.rm = T),
            AIH_SD = sd(aih, na.rm = T),
            # Volume
            V_min = min(v_ff, na.rm = T),
            V_mean = mean(v_ff, na.rm = T),
            V_max = max(v_ff, na.rm = T),
            V_SD = sd(v_ff, na.rm = T),
            )

out_dir <- file.path("E:\\OneDrive\\R\\Durall\\MScThesis_MEDFOR_UVa\\4-THESIS\\4.1-DRAFTS\\03 - Tables\\")

write.xlsx2(x = Table1,
             file = file.path(out_dir, "Summary_PLOT.xlsx"),
             sheetName = "Summary",
             showNA = FALSE)

inventory %>% group_by(YEAR, PLOT) %>% summarise(DBH = mean(DBH, na.rm = T), H = mean(Hp, na.rm = T))

# Inventory
Table2 <- AD %>% left_join(., STAND[,c("AU", "STAND", "MATERIAL")]) %>% 
  group_by(YEAR, MATERIAL) %>% 
    summarise(V_ha = mean(Vol_ha),
              Vol = sum(Vol_Total),
              TREE_HA = mean(TREE_HA))
