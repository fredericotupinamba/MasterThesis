# Diameter Class
# Setting the Diameter Class
v1ToClass <- function(v1) {
  if (!is.na(v1)) {
    v1 <- v1 + 1e-08
    xclass <- 9
    if((v1 >0) & (v1 <=10)) 
      xclass <- 1
    if((v1 >10) & (v1 <=15)) 
      xclass <- 2
    if((v1 > 15) & (v1 <=20)) 
      xclass <- 3
    if((v1 >20) & (v1 <= 25)) 
      xclass <- 4
    if((v1 > 25) & (v1 <= 30))
      xclass <- 5
    if((v1 > 30) & (v1 <= 35))
      xclass <- 6
    if((v1 > 35)& (v1 <= 40)) 
      xclass <- 7
    if(v1 > 40) 
      xclass <- 8  
  } else {xclass <- NA}
  return(xclass)
}

for (i in 1 : length(inventory$DBH)){
  inventory$CD[i] <- v1ToClass(inventory$DBH[i])
}


# Different format
DBH_CLASS <- inventory %>% filter(!is.na(DBH)) %>% group_by(YEAR, AU, STAND, CD) %>% 
  summarise(
    treeHa = n(),
    volHa = sum(v_ff))

DBH_CLASS2 <- inventory %>% filter(!is.na(DBH)) %>% group_by(YEAR, AU, STAND) %>% 
  summarise(Plot = length(unique(PLOT)))

DBH_CLASS <- left_join(DBH_CLASS2, DBH_CLASS, by = c("YEAR", "AU", "STAND"))

DBH_CLASS$treeHa <- with(DBH_CLASS, (treeHa / (Plot * AP) * 10000)) 
DBH_CLASS$volHa <- with(DBH_CLASS, (volHa / (Plot * AP) * 10000))

DBH_CLASS <- left_join(DBH_CLASS,
          STAND[,c("AU", "STAND", "AREA_2019", "AREA_2020", "MATERIAL")], 
          by = c("AU", "STAND"))

# write.xlsx2(as.data.frame(DBH_CLASS),
#             file = paste0(out_dir, "/ClasseDiametrica.xlsx"),
#             sheetName = "volHa2",
#             append = T,
#             showNA = FALSE)
