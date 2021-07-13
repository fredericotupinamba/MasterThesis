# Verify the package is installed ###################################
# create a function to check for installed packages and install them if they are not installed
install <- function(packages){
     new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
     if (length(new.packages)) 
          install.packages(new.packages, dependencies = TRUE)
     sapply(packages, require, character.only = TRUE)
}

# Used here
packs <- c("dplyr",
           "ggplot2",
           "raster",
           "RStoolbox",
           "stringr",
           "sf",
           "tmap",
           "tmaptools",
           "gdalUtils")


install(packs)

# Defining path #####################################################
getwd()
setwd("E:/OneDrive/R/Durall/MScThesis_MEDFOR_UVa/")
raw_data <- paste0(getwd(), "/1-DATA/1.1-RAW/3-IMAGEM/2020/CLIP/")
processing_path <- paste0(getwd(),"/1-DATA/1.2-PROCESSED/1-IMAGEM/Index")

# Loop ##############################################################
# Variables 
modulo <- c(1:10)
path_modulo <- dir(raw_data, pattern = (".tif$")) #"Modulo07.tif", "Modulo09.tif"
mosaico <- data.frame(modulo, path_modulo)
subset_rgb_R <- subset_rgb_G <- subset_rgb_B <- VDVI <- VARI <- GRDI <- RGRI <- NGRVI <- ExG <- CIVE <- VEG <- NULL

#length(mosaico)
for (i in 1:1) {
     img <- stack(paste0(raw_data, mosaico[i,2]))                    # stack RGB raster
     names(img[[1]]) <- "R"
     names(img[[2]]) <- "G"
     names(img[[3]]) <- "B"

     # subset_rgb_R <- list(R = subset(img,c(1),drop=FALSE))                     # Subset bands
     # subset_rgb_G <- list(G = subset(img,c(2),drop=FALSE))
     # subset_rgb_B <- list(B = subset(img,c(3),drop=FALSE))
     # 
     # # Single Band
     # R <- stack(subset_rgb_R[i])                                        # STACK each band
     # G <- stack(subset_rgb_G[i])
     # B <- stack(subset_rgb_B[i])

     # Index
     # Visible-band Difference Vegetation Index (VDVI)
     calcVDVI <- function(R, G, B) return(((2*G - R - B)/(2 * G + R + B)))
     VDVI <- overlay(img$R, img$G, img$B, fun = calcVDVI)   
     
     # # Visible Atmospherically Resistant Index (VARI)
     # VARI <- list("VARI" = ((G - R) / (G + R - B)))  
     # 
     # # Normalized Green-Red Difference Index (NGRDI)
     # GRDI <- list("GRDI" = ((G - R) / (G + R)))        
     # 
     # # Red-Green Ratio Index (RGRI)
     # RGRI <- list("RGRI" = ((R / G)))    
     # 
     # # Normalized Green-Red Difference Index (NGRDI)
     # NGRVI <- list("NGRVI" = ((G^2 - R^2)/(G^2 + R^2)))          
     # 
     # # Excess Green Index (ExG)
     # ExG <- list("Exg" = (2*G - R - B))  
     # 
     # # Color Index of Vegetation (CIVE)
     # CIVE <- list("CIVE"= (0.441*R - 0.811*G + 0.385*B + 18.787))           
     # 
     # # Vegetativen (VEG)
     # a = 0.667
     # VEG[i] <- list("VEG"= (G / (R^a * B^(1 - a)))) 
     # 
     # 
     # INDEX <- list(subset_rgb_R, subset_rgb_G, subset_rgb_B, VDVI, VARI, GRDI, RGRI, NGRVI, ExG, CIVE, VEG)
     INDEX <- c("VDVI")


for (n in 1:length(INDEX)) {
  path = file.path(processing_path, paste0("AU_",mosaico[i,1],"_",names(INDEX)[n],".tif"))
          
          writeRaster(VDVI,
                      filename = path,
                      format="GTiff", 
                      overwrite=TRUE)
          
          # gdalwarp(srcfile = path,
          #          dstfile = path2,
          #          t_srs = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
          #          tr = c(1,1),
          #          r = "bilinear",
          #          output_Raster = TRUE,
          #          overwrite = TRUE,
          #          verbose = TRUE)
}
}
           