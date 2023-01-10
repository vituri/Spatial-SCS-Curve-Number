#Convert land use and soil to curve number to use in SCS method, 
#conversion of rain to effective runoff. Method developed by the USDA 
#(https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb1044171.pdf)

#Data:
#Land use from the Mapbiomas Landsat classification collection, from GEE: https://mapbiomas.org/

library(terra)    

dir.create(path = 'temp', showWarnings = FALSE) #To create a temp directory to exclude in the final

terraOptions(tempdir = 'temp')


#########################################################################
############################# Soil raster ###############################
#########################################################################

#SCS Soil group from SCS method	A = (1)	B = (2)	C = (3)	D = (4) 
# Layers from Soil Grid
# See my script to download in Google Earth Engine
# https://github.com/lvsantarosa/Soil-Grid-on-Google-Earth-Engine

Clay <- terra::rast('Soil/clay.tif')
Sand <- terra::rast('Soil/sand.tif')
Silt <- terra::rast('Soil/silt.tif')

#Convert to percentage

Clay <- terra::mean(Clay)/1000
Sand <- terra::mean(Sand)/1000
Silt <- terra::mean(Silt)/1000

# Reclassify the values into groups occording to textural triangle
# https://hess.copernicus.org/preprints/hess-2017-13/hess-2017-13.pdf

Sand_m <- matrix(c(0.50, 1   , 10,
                   0.01, 0.50, 20), ncol = 3, byrow=TRUE)

Sand_rec <- classify(Sand, Sand_m, include.lowest=TRUE)


Clay_m <- matrix(c(0.01, 0.20, 100,
                   0.20, 0.40, 200,
                   0.40, 1   , 300), ncol = 3, byrow=TRUE)

Clay_rec <- classify(Clay, Clay_m, include.lowest=TRUE)


Silt_m <- matrix(c(0.01, 0.30, 1000,
                   0.30, 0.75, 2000,
                   0.75, 1   , 3000), ncol = 3, byrow=TRUE)

Silt_rec <- classify(Silt, Silt_m, include.lowest=TRUE)


#Sum the values
Sum_ly <- Sand_rec + Clay_rec + Silt_rec

#Discriminate the groups based in the sum result
grupos <- rbind(c(1110, 2),
                c(1120, 3),
                c(1220, 3),
                c(1210, 2),
                c(1310, 4),
                c(1320, 4),
                c(2320, 4),
                c(2220, 1))

Soil_Hidro <- classify(Sum_ly, grupos)

plot(Soil_Hidro)


#########################################################################
################################## LULC #################################
#########################################################################

lista <- list.files('LULC/', full.names = TRUE, pattern = '.tif$')

uses_orig = terra::rast(lista)

Soil_Hidro <- project(Soil_Hidro, uses_orig[[1]])

SH_m <- matrix(c(0   , 0.1, 0,
                 0.1, 1.5, 1,
                 1.5, 2.5, 2,
                 2.5, 3.5, 3,
                 3.5, 4.5, 4), ncol = 3, byrow=TRUE)

Soil_Hidro <- classify(Soil_Hidro, SH_m, include.lowest=TRUE)

plot(Soil_Hidro)
hist(Soil_Hidro)

#Reduce the amount of data for easier processing, adjustable as needed (grid). 
mapbio_resample = terra::resample(uses_orig, Soil_Hidro, method="near") 

# MAPBIOMAS land use values to be reclassified as SCS method
# There are different classes depending on the MAPBIOMAS collection. 

#10	Urban (24)
#20	Agriculture (14, 18, 19, 20, 21, 36, 39, 40, 41, 46, 47, 48, 62)
#30	Forest (1, 3, 4, 5, 49)
#40	Silviculture (9)
#50	Grassland  (12, 13, 32, 50)
#60	Uncover (23,25, 29, 30)
#70	Pasture (15)
#100 Water (10, 27,31,33)


# multiple replacements
class_map_scs <- rbind(c(24, 10), 
                       c(14, 20),c(18, 20), c(19, 20), c(20, 20), c(21, 20), c(36, 20), c(39, 20), c(40, 20), c(41, 20), c(46, 20), c(47, 20), c(62, 20), 
                       c(1, 30), c(3, 30), c(4, 30), c(5, 30), c(49, 30), 
                       c(9, 40), 
                       c(12, 50), c(13, 50), c(32, 50), c(50, 50), 
                       c(23, 60), c(25, 60), c(29, 60), c(30, 60),
                       c(15, 70),
                       c(11, 100), c(27, 100), c(31, 100), c(33, 100))


#Reclassify uses based in the matrix
rc_map_scs = classify(mapbio_resample, class_map_scs)

#Remove 0 
rc_map_scs[rc_map_scs <= 0] = NA


#####################CN final##########################

# Values based in the method => https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb1044171.pdf

#	Sum Soil and Land Use -> to reclassify using the SCS values above				
#	Uses/Soil	    A	  B	  C	  D
#	Urban	        11	12	13	14
#	Crops	        21	22	23	24
#	Florest	      31	32	33	34
#	Silviculture	41	42	43	44
#	Herbaceus	    51	52	53	54
#	Baresoil	    61	62	63	64
#	Pasture	      71	72	73	74
#	No Data	      101	102	103	104

#	SCS method values 				
#	Uses/Soil     A	  B	  C	  D
#	Urban	        89	92	94	95
#	Crops	        64	75	82	85
#	Florest	      30	55	70	77
#	Silviculture	45	66	77	86
#	Herbaceus	    48	62	71	85
#	Bare Soil	    77	86	91	94
#	Pasture	      39	61	74	80
#	No Data	      0	  0	  0	  0


class_cn <- rbind(c(11,89), c(12,92), c(13,94), c(14,95), 
                  c(21,64), c(22,75), c(23,82), c(24,85), 
                  c(31,30), c(32,55), c(33,70), c(34,77), 
                  c(41,45), c(42,66), c(43,77), c(44,86), 
                  c(51,48), c(52,62), c(53,71), c(54,85),
                  c(61,77), c(62,86), c(63,91), c(64,94), 
                  c(71,39), c(72,61), c(73,74), c(74,80), 
                  c(101,0), c(102,0), c(103,0), c(104,0), 
                  c(10,0 ), c(20,0 ), c(30,0 ), c(40,0 ), 
                  c(50,0 ), c(60,0 ), c(70,0 ), c(100,0))

#Reclassify

sum_soil_uses = Soil_Hidro + rc_map_scs

cn_final = classify(sum_soil_uses, class_cn)


#Names
Nomes = seq(from = 2000, to = 2021)
names(cn_final) = Nomes

cn_final[cn_final <= 0] = NA


library(glue)
library(dplyr)

#Export in different archives
filename = glue("Results/CN_{Nomes}.tif")
cn_final %>% writeRaster(filename = filename, overwrite = TRUE)

#Export soil layer
Soil_Hidro <- mask(Soil_Hidro, cn_final[[1]])
terra::writeRaster(Soil_Hidro, "Results/Soil_Hidro.tif", overwrite = TRUE)

###########################################################
unlink(x = list.files('temp', full.names = TRUE)) #Delete the temp archives




