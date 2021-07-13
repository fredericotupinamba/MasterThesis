######################################################################
#### Processing the Volume by Plot ####
# Summary of plots
summary_mortality <- inventory %>% filter(YEAR == 2020) %>% 
  group_by(ID, YEAR, AU, STAND, PLOT, QUALITY_COD) %>% 
  summarise(Vol = sum(v_ff, na.rm = TRUE), # Volume for all the trees // fator de forma 0.46
            N = n(),
            TREE_HA = ((N/AP)*10000)) %>%  
  filter(Vol > 0)

out_dir <- "E:\\OneDrive\\R\\Durall\\MScThesis_MEDFOR_UVa\\1-DATA\\1.2-PROCESSED\\3-INVENTORY\\"

# write.xlsx2(x = as.data.frame(summary_mortality),
#             file = file.path(out_dir, "TalhoesMortosRF.xlsx"),
#             sheetName = "TREE",
#             col.names = TRUE,
#             row.names = FALSE,
#             showNA = F)

summary_mortality %>% filter(QUALITY_COD == 1) %>% 
ggplot(aes(x=N)) +
  geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9, geom="text", aes(label=..count..) , 
                 vjust = -1)
