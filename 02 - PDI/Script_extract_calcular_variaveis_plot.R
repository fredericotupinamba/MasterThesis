rm(list = ls(all.names = T))
#Load packages
library(pacman)
pacman::p_load(raster,
               SegOptim,
               stringr,
               sp, 
               RColorBrewer,
               rgl,
               rgeos,
               rgdal,
               data.table,
               glcm,
               ggplot2,
               cowplot,
               ggthemes, 
               dplyr, 
               reshape2, 
               tidyr,
               lubridate,
               plyr, 
               caret,
               foreach,
               lidR,
               randomForest, 
               doParallel,
               magrittr,
               raster,
               pROC,
               plotROC,
               VGAM)
# Este Script se ha preparado para la extraccion de indices de la seqoia a nivel de copa 
# As?, el script trabaja con 4 bandas del sensor multiespectral y estructura vertical

# 1.  B1.-Green
# 2.  B2-.red
# 3.  B3.-red_egge
# 4.  B4- NIR
# 7.  NDVI
#-------------Datos y rutas de entrada---------------------------------
# Ruta de la shp plots

POLYGON=shapefile("D:\\.shp")

#-------------Rutas de los mosaicos---------------------------------------

ruta1="D:\\PROYECTOS_DRON\\Fluvial_Life\\01_data\\133_life_fluvial_2\\7_mosaico\\133_mosaic_G_R_RE_NIR.tif"
ruta2="D:\\PROYECTOS_DRON\\Fluvial_Life\\01_data\\133_life_fluvial_2\\7_mosaico\\MOSAICO_vertical_LMIN_LMAX_LAVG_LSTD_LSKE_LKUR_LP05_LP20_LP50_LP95_LP99_LCOV_LDNS_LD1_LD2_LD3_LD4.tif"
ruta3="D:\\PROYECTOS_DRON\\Fluvial_Life\\01_data\\133_life_fluvial_2\\7_mosaico\\133_indice_ndvi.tif"
ruta4="D:\\PROYECTOS_DRON\\Fluvial_Life\\01_data\\133_life_fluvial_2\\5_mds\\133_dsm.tif"
#-------------Extraccion de info dos plots-------------------
MOSAICO1=stack(ruta1)
MOSAICO1
MOSAICO2=stack(ruta2)
MOSAICO2
crs(MOSAICO2)<-"+init=epsg:3763"
#MOSAICO3=stack(MOSAICO1,MOSAICO2)
MOSAICO2_res<-projectRaster(MOSAICO2,MOSAICO1,crs="+init=epsg:3763",method="ngb",alignOnly = TRUE)
#MOSAICO3=stack(MOSAICO1,MOSAICO2_res)

DATOS4<-extract(x=MOSAICO1, y=POLYGON,fun=mean, sp=TRUE,na.rm=TRUE)
DATOS@names
DATOS.tabla4=as.data.frame(DATOS4)
DATOS.tabla4

colnames(DATOS.tabla4)= c("codigo","Esanitario","Archi","check","Health status","B1", "B2", "B3", "B4")

DATOS5=extract(x=MOSAICO2, y=POLYGON,fun=mean,sp=TRUE,na.rm=TRUE)

DATOS5=extract(x=MOSAICO2, y=POLYGON,fun=mean, sp=TRUE,na.rm=TRUE)
DATOS5@names
DATOS5
DATOS.tabla5=as.data.frame(DATOS5)

colnames(DATOS.tabla5)= c("codigo","Esanitario","Archi","check","Health status","LMIN","LMAX","LAVG","LSTD","LSKE","LKUR","LP05","LP20","LP50","LP95","LP99","LCOV","LDNS","LD1","LD2","LD3","LD4")


#DATOS6
#DATOS.tabla6=as.data.frame(TEXTURE)


# ------------Calculo de los indices espectrales----------------------------------------------

DATOSextraidos=DATOS.tabla4

B1 = DATOSextraidos$B1
B2 = DATOSextraidos$B2
B3 = DATOSextraidos$B3
B4 = DATOSextraidos$B4

# 1.  B1.-GReen
# 2.  B2-.Red/NIR
# 3.  B3.-red_ege/red
# 4.  B4-NIR/red_edge

#NDVI
NDVI  = (B4-B2)/(B4+B2)
#Green NDVI (GNDVI)
GNDVI=(B4-B1)/(B4+B1)
#RENDVI
RENDVI=(B4-B3)/(B4+B3)
#REGNDVI
REGNDVI=(B3-B1)/(B3+B1)
#RERNDVI
RERNDVI=(B3-B2)/(B3+B2)
#Normalized Green-Red Vegetation Index
NGRVI=(B1-B2)/(B1+B2)
#Non Linear Index (NLI)
NLI=((B4^2)-(B2))/((B4^2)+(B2))
#A soil-adjusted vegetation index.
SAVI=((B4-B2)/((B4+B2)+0.15))*(1+0.5)

# ------------Calculo de textura ndvi----------------------
ndvi=raster(ruta3)

library(glcm)
t
texture_mosaico3<-glcm(ndvi, n_grey = 32, window = c(3, 3), shift = c(1, 1), statistics =
       c("mean_ENVI", "variance_ENVI", "homogeneity", "contrast", "dissimilarity", "entropy",
         "second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any",na_val=NA, scale_factor=1, asinteger=FALSE)

MOSAICO3<- stack(texture_mosaico3)

DATOS6=extract(x=MOSAICO3, y=POLYGON,fun=mean, sp=TRUE,na.rm=TRUE)
DATOS6@names
DATOS6
DATOS.tabla6=as.data.frame(DATOS6)

colnames(DATOS.tabla6)= c("codigo","Esanitario","Archi","check","Health status","ndvi_Glcm_mean", "ndvi_Glcm_variace", "ndvi_Glcm_homomgeneity", 
                          "ndvi_Glcm_contrast","ndvi_Glcm_dissimilarity","ndvi_Glcm_entropy","ndvi_Glcm_second_moment","ndvi_Glcm_correlation")


DATOS6
DATOS.tabla6=as.data.frame(TEXTURE)


# ------------Calculo de variables DSM----------------------
dsm=raster(ruta4)
slope_mosaico4 <- terrain(dsm, opt=c('slope', 'aspect'), unit='degrees')
TPI_mosaico4<-terrain(dsm, opt=c("TPI","TRI","roughness","flowdir")) 

MOSAICO4<- stack(slope_mosaico4,TPI_mosaico4)

DATOS7=extract(x=MOSAICO4, y=POLYGON,fun=mean, sp=TRUE,na.rm=TRUE)
DATOS8=extract(x=MOSAICO4, y=POLYGON,fun=sd, sp=TRUE,na.rm=TRUE)

DATOS7@names
DATOS7
DATOS.tabla7=as.data.frame(DATOS7)
DATOS.tabla7
DATOS.tabla8=as.data.frame(DATOS8)

colnames(DATOS.tabla7)= c("codigo","Esanitario","Archi","check","Health status","SLOPE_Mean", "ASPECT_Mean", "TPI_Mean","TRI_Mean","ROUGHNESS_Mean","FLOWDIR_Mean")
colnames(DATOS.tabla8)= c("codigo","Esanitario","Archi","check","Health status","SLOPE_sd", "ASPECT_sd", "TPI_sd","TRI_sd","ROUGHNESS_sd","FLOWDIR_sd")

texture_dsm<-glcm(dsm, n_grey = 32, window = c(3, 3), shift = c(1, 1), statistics =
                         c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy",
                           "second_moment", "correlation"), scale_factor=1, asinteger=FALSE)

MOSAICO5<- stack(texture_dsm)

DATOS9=extract(x=MOSAICO5, y=POLYGON,fun=mean, sp=TRUE,na.rm=TRUE)
DATOS9@names
DATOS9
DATOS.tabla9=as.data.frame(DATOS9)

colnames(DATOS.tabla9)= c("codigo","Esanitario","Archi","check","Health status","Glcm_mean", "Glcm_variace", "Glcm_homomgeneity", 
                          "dsm_Glcm_contrast","dsm_Glcm_dissimilarity","dsm_Glcm_entropy","dsm_Glcm_second_moment","dsm_Glcm_correlation")

#-------------export results------------------------------------------------

DATOSextraidos_copas_130519=cbind(DATOS.tabla4,NDVI,GNDVI,RENDVI,REGNDVI,RERNDVI,NGRVI,NLI,SAVI,DATOS.tabla5,DATOS.tabla6, DATOS.tabla7,DATOS.tabla8,DATOS.tabla9)

write.csv(DATOSextraidos_copas_130519,"D:\\PROYECTOS_DRON\\Fluvial_Life\\04_modeling\\20190506_Fluvial_life_shapes_estado_fitosanitario\\20190513_DATOSextraidos_copas.csv")

